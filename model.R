# Poll Tracker
#
# Copyright 2020 Jack Bailey <jack.bailey@manchester.ac.uk>
# Copyright 2021 Justas Lavišius <bucaneer@gmail.com>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

library(tidyverse)
library(tidybayes)
library(lubridate)
library(brms)
library(here)
library(zoo)
library(rjson)

# Set random seed
seed <- 19900311
set.seed(seed)

load_raw <- function() {
    raw_data <<- read.csv('polls.csv')
    
    partynames <<- make.names(c(
        "TS-LKD",
        "LVZS",
        "LSDP",
        "LRLS",
        "DP",
        "LP",
        "LSDDP/LRP",
        "LLRA-KSS",
        "TT/LT",
        "VL",
        "NA."
        #"CP/TTS",
    ))
    
    # Set time range starting 2 years before the end of the latest poll,
    # + 3 months starting buffer not included in the final output
    END_DATE <<- ymd(max(raw_data$end_date))
    OUTPUT_START_DATE <<- END_DATE %m-% years(2)
    START_DATE <<- OUTPUT_START_DATE %m-% months(3)
    
    # Remove data outside date range
    raw_data <<- raw_data[!raw_data$`start_date` < START_DATE,]
    raw_data <<- raw_data[!raw_data$`end_date` > END_DATE,]
    
    # Only keep polls from pollsters with 3 or more polls
    series <<- raw_data[raw_data$pollster %in% names(table(raw_data$pollster)[table(raw_data$pollster) >= 3]),]
    
    # Calculate middle of the polling period
    series["date"] <<- ymd(series$start_date) + ((ymd(series$end_date) - ymd(series$start_date)) / 2)
    
    # Replace NA & 0 values with reduced per-pollster averages
    for (party in partynames) {
		all_avg <- mean(series[!is.na(series[[party]]) & series[[party]] > 0,party])
		for (pollster in names(table(series$pollster))) {
			poll_filter <- series$pollster == pollster
			poll_avg <- mean(series[poll_filter & !is.na(series[[party]]) & series[[party]] > 0,party])
			poll_bias <- 1 + (poll_avg - all_avg) / all_avg
			# NA ~ 1%
			series[poll_filter & is.na(series[[party]]),party] <<- poll_bias * 0.01
			# 0 ~ 0.1%
			series[poll_filter & series[[party]] == 0,party] <<- poll_bias * 0.001
		}
	}

    series["oth"] <<- (1 - rowSums(series[partynames]))

    dta <<- series[c("date", "pollster", partynames, "oth")] %>% tibble()
        
    dta <<-
      dta %>%
      mutate(time = interval(START_DATE, date)/years(1))
    
    dta <<-
      dta %>%
      mutate(
        outcome = as.matrix(dta[names(dta) %in% c(partynames, "oth")])
      )
}

run_pred <- function() {
    dpars <- map_chr(partynames, ~ paste0("mu", gsub(".","",.x,fixed=T)))
    
    priors <- set_prior("gamma(1, 0.01)", class = "phi") +
		set_prior("normal(0, 1.5)", class = "Intercept", dpar = dpars) +
		set_prior("normal(0, 0.5)", class = "b", dpar = dpars) +
		set_prior("exponential(2)", class = "sd", dpar = dpars) +
		set_prior("exponential(2)", class = "sds", dpar = dpars)
    
    m1 <<-
      brm(formula = bf(outcome ~ 1 + s(time, k = 40) + (1 | pollster)),
          family = dirichlet(link = "logit", refcat = "oth"),
          prior = priors,
          backend = "cmdstanr",
          data = dta,
          seed = seed,
          iter = 2e3,
          chains = 4,
          cores = parallel::detectCores(),
          threads = threading(parallel::detectCores()),
          refresh = 5,
          adapt_delta = .97,
          max_treedepth = 15,
          silent = 0,
          file = here("_output", paste0("model", "_", START_DATE, "_", END_DATE, "_", nrow(dta))),
          save_model = "brm_model"
      )
}

analyze_pred <- function() {
    end_time <- interval(START_DATE, END_DATE)/years(1)
    
    pred_dta_raw <<-
      tibble(
        time = seq(0, end_time, length.out = round(interval(START_DATE, END_DATE)/weeks(1))),
        date = as.Date(time*365, origin = START_DATE),
      )
    
    pred_dta_raw <<-
      add_epred_draws(
        object = m1,
        newdata = pred_dta_raw,
        re_formula = NA,
        value = ".value"
      ) %>%
      group_by(date, .category) %>%
      summarise(
        est = median(.value),
        lower = quantile(.value, probs = .05),
        upper = quantile(.value, probs = .95),
        .groups = "drop"
      ) %>%
      ungroup() %>%
      rename(party = .category)
    
    pred_dta <<- pred_dta_raw %>%
      filter(date > OUTPUT_START_DATE)
     
    csv_filename <- here("_output", paste0("model", "_", START_DATE, "_", END_DATE, ".csv"))
    write.csv(pred_dta, csv_filename, row.names=F)
    file.copy(csv_filename, here("_output", "model-latest.csv"), overwrite=T)
}

pred_model <- function() {
	# Load model generated by loyalty.py
	model <<- fromJSON(file="loyalty_model.json")

	c_factor <<- as.data.frame(model$factor) %>%
		relocate(all_of(partynames))
	c_inc <<- model$incumbent
	c_new <<- model$new
	c_change <<- model$change
	c_change6 <<- model$change6
	
	rn <<- c(
		"2020-12-11",
		"2024-12-12",
		"2025-10-01"
	)
	new_l <<- t(data.frame(
		"2020-12-11"=c(
			"TS-LKD"=0,
	        "LVZS"=0,
	        "LSDP"=0,
	        "LRLS"=0,
	        "DP"=0,
	        "LP"=0,
	        "LSDDP/LRP"=0,
	        "LLRA-KSS"=0,
	        "TT/LT"=0,
	        "VL"=1,
	        "NA."=1
		),
		"2024-12-12"=c(
			"TS-LKD"=0,
	        "LVZS"=0,
	        "LSDP"=0,
	        "LRLS"=0,
	        "DP"=0,
	        "LP"=0,
	        "LSDDP/LRP"=0,
	        "LLRA-KSS"=0,
	        "TT/LT"=0,
	        "VL"=0,
	        "NA."=0
		)
	))
	
	inc_l <<- t(data.frame(
		"2020-12-11"=c(
			"TS-LKD"=1,
	        "LVZS"=0,
	        "LSDP"=0,
	        "LRLS"=1,
	        "DP"=0,
	        "LP"=1,
	        "LSDDP/LRP"=0,
	        "LLRA-KSS"=0,
	        "TT/LT"=0,
	        "VL"=0,
	        "NA."=0
		),
		"2024-12-12"=c(
			"TS-LKD"=0,
	        "LVZS"=0,
	        "LSDP"=1,
	        "LRLS"=0,
	        "DP"=0,
	        "LP"=0,
	        "LSDDP/LRP"=0,
	        "LLRA-KSS"=0,
	        "TT/LT"=0,
	        "VL"=1,
	        "NA."=1
		),
		"2025-10-01"=c(
			"TS-LKD"=0,
	        "LVZS"=1,
	        "LSDP"=1,
	        "LRLS"=0,
	        "DP"=0,
	        "LP"=0,
	        "LSDDP/LRP"=0,
	        "LLRA-KSS"=1,
	        "TT/LT"=0,
	        "VL"=0,
	        "NA."=1
		)
	))
	
	rownames(new_l) <<- rn[1:nrow(new_l)]
	rownames(inc_l) <<- rn[1:nrow(inc_l)]
	
	forecast <<- pred_dta_raw %>%
		select(date, party, est) %>%
		pivot_wider(names_from=party,values_from=est) %>%
		mutate(date=date + weeks(2)) %>%
		column_to_rownames('date') %>%
		select(all_of(partynames))
	
	change <<- (forecast - lag(forecast, 4)) / forecast
	change6 <<- (forecast - lag(forecast, 6*4)) / forecast
	
	new_df <<- forecast * 0
	for (i in 1:nrow(new_l)) {
		filter <- rownames(new_df) > rownames(new_l)[i]
		new_df[filter,] <<- matrix(rep(new_l[i,], each=sum(filter)), nrow=sum(filter))
	}
	
	inc_df <<- forecast * 0
	for (i in 1:nrow(inc_l)) {
		filter <- rownames(inc_df) > rownames(inc_l)[i]
		inc_df[filter,] <<- matrix(rep(inc_l[i,], each=sum(filter)), nrow=sum(filter))
	}
	
	fact_df <- forecast
	fact_df[,] <- c_factor[,]
	
	pred_raw <<- forecast * (
		fact_df +
		c_inc * inc_df +
		c_new * new_df +
		c_change * change +
		c_change6 * change6
	)
	
	rownorm <<- (1 - rowSums(pred_raw)) / rowSums(forecast)
	pred <<- pred_raw + rownorm * forecast
	
	other <<- 0.087
	pred <<- (pred * (1 - other)) %>%
		drop_na()
	
	mandates <<- pred_to_mandates(pred)
	
	res_dta <<- pred %>%
		mutate(date=rownames(pred)) %>%
		pivot_longer(cols=all_of(partynames), names_to="party", values_to="res")
	
	mand_dta <<- mandates %>%
		mutate(date=rownames(mandates)) %>%
		pivot_longer(cols=all_of(partynames), names_to="party", values_to="mandates")
	
	res_dta["mandates"] <- mand_dta["mandates"]
	
	csv_filename <- here("_output", paste0("res", "_", START_DATE, "_", END_DATE, ".csv"))
    write.csv(res_dta, csv_filename, row.names=F)
    file.copy(csv_filename, here("_output", "res-latest.csv"), overwrite=T)
}

pred_to_mandates <- function(pred) {
	output <- pred * 0
	
	LIMIT <- 0.05
	TOTAL <- 70
	
	for (i in 1:nrow(pred)) {
		row <- pred[i,]
		over_lim <- row[,row > LIMIT]
		quota <- sum(over_lim) / TOTAL
		mandates <- floor(over_lim / quota)
		while (sum(mandates) < TOTAL) {
			remainder <- over_lim - mandates * quota
			max_rem <- max(remainder)
			for (p in colnames(remainder)) {
				if (remainder[p] == max_rem) {
					if (p %in% colnames(mandates)) {
						mandates[p] <- mandates[p] + 1
					} else {
						mandates[p] <- 1
					}
					break
				}
			}
		}
		output[i,colnames(mandates)] <- mandates
	}
	
	output
}

args <- commandArgs(trailingOnly=T)

if ((length(args) > 1) && (args[[2]] > 0)) {
    load_raw()
    run_pred()
    analyze_pred()
    pred_model()
}
