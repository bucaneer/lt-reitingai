#!/usr/bin/env python3

import sys
import json
from ortools.linear_solver import pywraplp
import csv
import numpy as np

USE_ALIAS = True

def main():
  data = load_data()

  model = optimize_factors(data)

  with open('loyalty_model.json', 'w') as f:
    json.dump(model, f, indent=2)

def load_input (input_path: str) -> dict:
  with open(input_path, "r") as f:
    data = json.load(f)
  return data

def optimize_factors(
  data: dict
):
  solver = pywraplp.Solver.CreateSolver("SCIP")

  # number of observations (elections) in training data
  n = len(data["years"])

  # number of components (parties) per observation
  k = len(data["parties"])

  # main party loyalty coefficients
  factors = [solver.NumVar(1, 10, f"factor_{j}") for j in range(k)]

  # additional coeff for incumbent parties
  inc_fact = solver.NumVar(-5, 5, "inc_fact")
  # additional coeff for change over 1 month
  change_fact = solver.NumVar(-5, 5, "change_fact")
  # additional coeff for change over 6 months
  change6_fact = solver.NumVar(-5, 5, "change6_fact")
  # additional coeff for new parties
  new_fact = solver.NumVar(-5, 5, "new_fact")
  
  if USE_ALIAS:
    dummies = [solver.NumVar(1, 10, f"dummy_{j}") for j in range(k)]
    inc_dummy = solver.NumVar(-5, 5, "inc_dummy")
    change_dummy = solver.NumVar(-5, 5, "change_dummy")
    change6_dummy = solver.NumVar(-5, -5, "change6_dummy")
    new_dummy = solver.NumVar(-5, 5, "new_dummy")
  
  _n = n*2 if USE_ALIAS else n

  # factors for ensuring row normalization (sum to 1)
  rownorms = [solver.NumVar(-100*k, 100*k, f"rownorm_{i}") for i in range(_n)]

  # output values: predicted election results
  preds = [[solver.NumVar(0, 1, f"pred_{i}_{j}") for j in range(k)] for i in range(_n)]

  # score values: difference between predicted and measured results  
  errors = [[solver.NumVar(0, 1, f"error_{i}_{j}") for j in range(k)] for i in range(_n)]
  
  for i in range(_n):
    for j in range(k):
      # apply alias logic
      is_main = i < n
      _i = i if is_main else i - n
      is_alias = data["aliases"][_i][j] is not None
      if not is_main:
        if is_alias:
          _j = data["aliases"][_i][j]
        else:
          if j in data["aliases"][_i]:
            _j = data["aliases"][_i].index(j)
          else:
            _j = j
      else:
        _j = j
      if is_main ^ is_alias:
        factor = factors[j]
        _inc_fact = inc_fact
        _new_fact = new_fact
        _change_fact = change_fact
        _change6_fact = change6_fact
      else:
        factor = dummies[j]
        _inc_fact = inc_dummy
        _new_fact = new_dummy
        _change_fact = change_dummy
        _change6_fact = change6_dummy
      
      # main model formula: mutiply forecast by sum of coefficients to calculate predicted results
      solver.Add(
        preds[i][j] ==
          data["forecasts"][_i][_j] *
          (
            factor +
            data["incumbent"][_i][_j] * _inc_fact +
            data["new"][_i][_j] * _new_fact +
            data["changes"][_i][_j] * _change_fact +
            data["changes6"][_i][_j] * _change6_fact +
            rownorms[i]
          )
      )

      # ensure predicted results are positive
      solver.Add(preds[i][j] >= 0)

      # ensure prediction rows are normalized
      solver.Add(solver.Sum(preds[i]) == 1)

      if is_main ^ is_alias:
        # ensure predictions are on the correct side of the 5%/7% threshold
        _base_threshold = 0.07 if data["coalition"][_i][_j] else 0.05
        _threshold = _base_threshold / (1 - data["other"][_i])
        if data["measurements"][_i][_j] >= _threshold:
          solver.Add(preds[i][j] >= _threshold)
        else:
          solver.Add(preds[i][j] <= _threshold - 1e-6)
        
      # calculate absolute error between predictions and measurements
      solver.Add(preds[i][j] - data["measurements"][_i][_j] <= errors[i][j])
      solver.Add(data["measurements"][_i][_j] - preds[i][j] <= errors[i][j])

  # optimization target: lowest absolute error
  total_cost = solver.Sum(errors[i][j] for j in range(k) for i in range(_n))
  solver.Minimize(total_cost)

  # solve model
  status = solver.Solve()

  # get solution values of all variables
  factor_vals = {data["parties"][j]: factors[j].solution_value() for j in range(k)}
  inc_val = inc_fact.solution_value()
  new_val = new_fact.solution_value()
  change_val = change_fact.solution_value()
  change6_val = change6_fact.solution_value()
  rownorm_vals = [rownorms[i].solution_value() for i in range(n)]
  error_vals = [[errors[i][j].solution_value() for j in range(k)] for i in range(n)]

  return {
    "factor": factor_vals,
    "incumbent": inc_val,
    "new": new_val,
    "change": change_val,
    "change6": change6_val,
    "errors": error_vals,
  }

def test_model(model, data):  
  k = len(data["parties"])

  c_fact = list(model["factor"].values())
  c_inc = model["incumbent"]
  c_new = model["new"]
  c_change = model["change"]
  c_change6 = model["change6"]

  # calculate predicted results for the test sample
  res_raw = [
    data["forecasts"][0][j] *
    (
      c_fact[j] +
      data["incumbent"][0][j] * c_inc +
      data["new"][0][j] * c_new +
      data["changes"][0][j] * c_change +
      data["changes6"][0][j] * c_change6
    )
    for j in range(k)
  ]
  rownorm = (1 - sum(res_raw)) / sum(data["forecasts"][0])
  res = [res_raw[j] + rownorm * data["forecasts"][0][j] for j in range(k)]

  # caculate turnout-adjusted prediction
  res_a = [r * (1 - data["other"][0]) for r in res]
  # calculate turnout-adjusted test measurement
  mes_a = [m * (1 - data["other"][0]) for m in data["measurements"][0]]

  # caculate prediction score
  res_err = sum(abs(res_a[j] - mes_a[j]) for j in range(k))

  # caculate madate distribution for prediction & measurement
  _m = data["mandates"][0]
  m_res = res_to_mandates(res_a, data["coalition"][0], _m)
  m_mes = res_to_mandates(mes_a, data["coalition"][0], _m)

  m_diff = [int(m_res[j] - m_mes[j]) for j in range(k)]

  return {
    "res_a": res_a,
    "mes_a": mes_a,
    "res_diff": [res_a[j] - mes_a[j] for j in range(k)],
    "res_err": res_err,
    "m_res": m_res,
    "m_mes": m_mes,
    "m_diff": m_diff,
    "m_score": sum(abs(x) for x in m_diff) / 2,
  }

def res_to_mandates(res: list, coalition: list, TOTAL: int) -> list:
  THRESH_P = 0.05
  THRESH_C = 0.07

  over_lim = {}
  party_mandates = [0 for r in res]
  for i, share in enumerate(res):
    if (share < THRESH_P) or (coalition[i] and share < THRESH_C):
      continue
    over_lim[i] = share
  total = sum(over_lim.values())
  quota = total / TOTAL
  for i, share in over_lim.items():
    mandates = int(np.floor(share / quota))
    party_mandates[i] = mandates
    remainder = share - (quota * mandates)
    over_lim[i] = remainder
  over_lim = sorted(over_lim.items(), key=lambda x: x[1], reverse=True)
  while sum(party_mandates) < TOTAL:
    if not len(over_lim):
      print(res, coalition, party_mandates)
    i, remainder = over_lim.pop(0)
    party_mandates[i] += 1
  return party_mandates

def load_data():
  skip_years = [
    "2024.ep",
  ]
  with open('loyalty_data.csv', 'r') as f:
    data = [r for r in csv.reader(f) if r[0] not in skip_years]

  year_col = [r[0] for r in data[1:]]
  years = [y for i, y in enumerate(year_col) if year_col.index(y) == i]

  party_col = [r[1] for r in data[1:]]
  parties = [p for i, p in enumerate(party_col) if party_col.index(p) == i]

  forecasts = [[0 for p in parties] for y in years]
  changes = [[0 for p in parties] for y in years]
  changes6 = [[0 for p in parties] for y in years]
  measurements = [[0 for p in parties] for y in years]
  incumbent = [[0 for p in parties] for y in years]
  turnout = [0 for y in years]
  coalition = [[0 for p in parties] for y in years]
  new = [[0 for p in parties] for y in years]
  other = [0 for y in years]

  for r in data[1:]:
    i = years.index(r[0])
    j = parties.index(r[1])
    incumbent[i][j] = int(r[2])
    turnout[i] = float(r[3])
    coalition[i][j] = int(r[4])
    new[i][j] = int(r[5])
    forecasts[i][j] = float(r[6])
    changes[i][j] = float(r[7])
    changes6[i][j] = float(r[8])
    measurements[i][j] = float(r[9])

  aliases = [[None for p in parties] for y in years]
  if USE_ALIAS:
    alias_names = [
      ('LP', 'LRLS'),
      ('LSDDP.LRP', 'LSDP'),
      ('VL', 'LVZS'),
      ('NA.', 'TT.LT'),
    ]
    alias_i = {parties.index(a[0]): parties.index(a[1]) for a in alias_names}
    
    for i in range(len(years)):
      for j in range(len(parties)):
        if forecasts[i][j] == 0 and j in alias_i:
          aliases[i][j] = alias_i[j]

  for i in range(len(years)):
    other[i] = (turnout[i] - sum(measurements[i])) / turnout[i]
    forecasts[i] = [f / sum(forecasts[i]) for f in forecasts[i]]
    measurements[i] = [m / sum(measurements[i]) for m in measurements[i]]

  mandates = [
    70 - 13 - 5, #2008
    70 - 7, #2012
    70, #2016
    70, #2020
    70, #2024
    11, #2024.ep
  ]

  return {
    "years": years,
    "parties": parties,
    "forecasts": forecasts,
    "measurements": measurements,
    "incumbent": incumbent,
    "new": new,
    "changes": changes,
    "changes6": changes6,
    "turnout": turnout,
    "coalition": coalition,
    "other": other,
    "mandates": mandates,
    "aliases": aliases,
  }

def get_sample_data(year, data, invert=False):
  years = data["years"]
  if invert:
    keep = [years.index(year)]
  else:
    keep = [years.index(y) for y in years if y != year]
  output = {}
  for key, value in data.items():
    if key == "parties":
      output[key] = value
      continue
    output[key] = [value[i] for i in keep]

  return output

def test(sample=True):
  data = load_data()

  results = {}

  for year in data["years"]:
    if sample:
      sample_data = get_sample_data(year, data)
      test_data  = get_sample_data(year, data, invert=True)
    else:
      sample_data = data
      test_data = get_sample_data(year, data, invert=True)

    model = optimize_factors(sample_data)

    test = test_model(model, test_data)

    results[year] = ["%.3f" % test["res_err"], test["m_score"]]

  return results

if __name__ == "__main__":
  print(test(sample=False))
  main()
