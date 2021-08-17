# Copyright (c) Facebook, Inc. and its affiliates.

# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

library(Robyn)

test_that("dt_prophet_holidays", {
  data(dt_prophet_holidays)
  df <- dt_prophet_holidays
  # Do we have all the data needed?
  expect_equal(colnames(df), c("ds", "holiday", "country", "year"))
  # Is there a new country? Are there less countries?
  expect_length(unique(df$country), 59)
})

test_that("dt_simulated_weekly", {
  data(dt_simulated_weekly)
  df <- dt_simulated_weekly
  # Do we have all the data needed?
  expect_equal(colnames(df), c(
    "DATE", "revenue", "tv_S", "ooh_S", "print_S", "facebook_I", "search_clicks_P",
    "search_S", "competitor_sales_B", "facebook_S", "events", "newsletter"
  ))
  expect_equal(nrow(df), 208)
})
