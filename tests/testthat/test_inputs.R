# Copyright (c) Facebook, Inc. and its affiliates.

# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

library(Robyn)

test_that("InputCollect part 1: data wrangling", {
  # saveRDS(InputCollect1, "tests/auxs/InputCollect1.RDS")
  InputCollect1 <- readRDS(system.file("/tests/auxs/InputCollect1.RDS", package = "Robyn"))
  expect_equal(suppressMessages(suppressWarnings(
    robyn_inputs(
      dt_input = Robyn::dt_simulated_weekly
      ,dt_holidays = Robyn::dt_prophet_holidays
      ,date_var = "DATE"
      ,dep_var = "revenue"
      ,dep_var_type = "revenue"
      ,prophet_vars = c("trend", "season", "holiday")
      ,prophet_signs = c("default","default", "default")
      ,prophet_country = "DE"
      ,context_vars = c("competitor_sales_B", "events")
      ,context_signs = c("default", "default")
      ,paid_media_vars = c("tv_S","ooh_S"	,	"print_S"	,"facebook_I"	,"search_clicks_P")
      ,paid_media_signs = c("positive", "positive","positive", "positive", "positive")
      ,paid_media_spends = c("tv_S","ooh_S",	"print_S"	,"facebook_S"	,"search_S")
      ,organic_vars = c("newsletter")
      ,organic_signs = c("positive")
      ,factor_vars = c("events")
      ,cores = 6
      ,window_start = "2016-11-23"
      ,window_end = "2018-08-22"
      ,adstock = "geometric"
      ,iterations = 100
      ,nevergrad_algo = "TwoPointsDE"
      ,trials = 2)
      )), InputCollect1)
})

# test_that("robyn_engineering", {
#   InputCollect1 <- readRDS("tests/auxs/InputCollect1.RDS")
#   robyn_engineering <- readRDS("tests/auxs/robyn_engineering.RDS")
#   actual <- robyn_engineering(InputCollect1)
#   robyn_engineering$plotNLSCollect <- NULL
#   actual$plotNLSCollect <- NULL
#   expect_equal(actual, robyn_engineering)
# })
