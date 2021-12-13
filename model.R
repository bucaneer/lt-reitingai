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
        "LLRA-KSS"
        #"TT/LT",
        #"CP/TTS",
    ))
    
    # Set time range starting 2 years before the end of the latest poll
    END_DATE <<- ymd(max(raw_data$end_date))
    START_DATE <<- END_DATE - years(2)
    
    # Remove data outside date range
    raw_data <<- raw_data[!raw_data$`start_date` < START_DATE,]
    raw_data <<- raw_data[!raw_data$`end_date` > END_DATE,]
    
    # Only keep polls from pollsters with 3 or more polls
    series <<- raw_data[raw_data$pollster %in% names(table(raw_data$pollster)[table(raw_data$pollster) >= 3]),]
    
    # Calculate middle of the polling period
    series["date"] <<- ymd(series$start_date) + ((ymd(series$end_date) - ymd(series$start_date)) / 2)
    
    ## Fill NA with normally distributed values around 1%
    ## (disabled due to brm_multi race-condition(?) issues)
    # multi_dta <<- list()
    # for (i in 1:3) {
    #     copy <- series
    #     for (j in 1:length(partynames)) {
    #         copy[partynames[j]][is.na(copy[partynames[j]])] <- rnorm(sum(is.na(copy[partynames[j]])), 0.01, 0.002)
    #        
    #         # Replace zeros with 0.01%
    #         copy[partynames[j]][copy[partynames[j]] <= 0] <- 0.0001
    #     }
    #     copy["oth"] <- (1 - rowSums(copy[partynames]))
    #    
    #     dta <- copy[c("date", "pollster", partynames, "oth")] %>% tibble()
    #    
    #     dta <-
    #       dta %>%
    #       mutate(time = interval(START_DATE, date)/years(1))
    #    
    #     dta <-
    #       dta %>%
    #       mutate(
    #         outcome = as.matrix(dta[names(dta) %in% c(partynames, "oth")])
    #       )
    #    
    #     multi_dta[[i]] <<- dta
    # }

    # Replace NA values with 1%
    series[is.na(series)] <- 0.01

    series["oth"] <- (1 - rowSums(series[partynames]))

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
    priors <- set_prior("gamma(1, 0.01)", class = "phi")
    
    for(i in 1:length(partynames)) {
        dpar1 = paste0("mu", gsub(".","",partynames[i],fixed=T))
        priors <- priors +
            set_prior("normal(0, 1.5)", class = "Intercept", dpar = dpar1) +
            set_prior("normal(0, 0.5)", class = "b", dpar = dpar1) +
            set_prior("exponential(2)", class = "sd", dpar = dpar1) +
            set_prior("exponential(2)", class = "sds", dpar = dpar1)
    }
    
    m1 <<-
      brm(formula = bf(outcome ~ 1 + s(time, k = 20) + (1 | pollster)),
          family = dirichlet(link = "logit", refcat = "oth"),
          prior = priors,
          backend = "cmdstanr",
          data = dta,
          seed = seed,
          iter = 5e3,
          chains = 6,
          cores = 12,
          threads = threading(6),
          refresh = 5,
          adapt_delta = .97,
          max_treedepth = 15,
          silent = 0,
          file = here("_output", paste0("model", "_", START_DATE, "_", END_DATE))
      )
}

analyze_pred <- function() {
    end_time <- interval(START_DATE, END_DATE)/years(1)
    
    pred_dta <<-
      tibble(
        time = seq(0, end_time, length.out = round(interval(START_DATE, END_DATE)/weeks(1))),
        date = as.Date(time*365, origin = START_DATE),
      )
    
    pred_dta <<-
      add_fitted_draws(
        model = m1,
        newdata = pred_dta,
        re_formula = NA
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
     
    csv_filename <- here("_output", paste0("model", "_", START_DATE, "_", END_DATE, ".csv"))
    write.csv(pred_dta, csv_filename, row.names=F)
    file.copy(csv_filename, here("_output", "model-latest.csv"), overwrite=T)
}

args <- commandArgs(trailingOnly=T)

if ((length(args) > 1) && (args[[2]] > 0)) {
    load_raw()
    run_pred()
    analyze_pred()
}