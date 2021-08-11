# Copyright (c) Facebook, Inc. and its affiliates.

# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

############# Auxiliary non-exported functions #############

check_datevar <- function(dt_input, date_var) {
  inputLen <- length(dt_input[, get(date_var)])
  inputLenUnique <- length(unique(dt_input[, get(date_var)]))
  if (is.null(date_var) | !(date_var %in% names(dt_input)) | length(date_var) > 1) {
    stop("Must provide correct only 1 date variable name for date_var")
  } else if (any(is.na(as.Date(as.character(dt_input[, get(date_var)]), "%Y-%m-%d")))) {
    stop("Date variable in date_var must have format '2020-12-31'")
  } else if (inputLen != inputLenUnique) {
    stop("Date variable has duplicated dates. Please clean data first")
  } else if (any(apply(dt_input, 2, function(x) any(is.na(x) | is.infinite(x))))) {
    stop("dt_input has NA or Inf. Please clean data first")
  }
  dt_input <- dt_input[order(as.Date(dt_input[, get(date_var)]))]
  dayInterval <- as.integer(difftime(
    as.Date(dt_input[, get(date_var)])[2],
    as.Date(dt_input[, get(date_var)])[1],
    units = "days"
  ))
  intervalType <- if (dayInterval == 1) {
    "day"
  } else
  if (dayInterval == 7) {
    "week"
  } else
  if (dayInterval %in% 28:31) {
    "month"
  } else {
    stop(paste(date_var, "data has to be daily, weekly or monthly"))
  }
  invisible(return(list(dayInterval = dayInterval, intervalType = intervalType)))
}

check_depvar <- function(dt_input, dep_var, dep_var_type) {
  if (is.null(dep_var) | !(dep_var %in% names(dt_input)) | length(dep_var) > 1) {
    stop("Must provide only 1 correct dependent variable name for dep_var")
  } else if (!(is.numeric(dt_input[, get(dep_var)]) | is.integer(dt_input[, get(dep_var)]))) {
    stop("'dep_var' must be a numeric or integer variable")
  } else if (!(dep_var_type %in% c("conversion", "revenue")) | length(dep_var_type) != 1) {
    stop("'dep_var_type' must be 'conversion' or 'revenue'")
  }
}

check_prophet <- function(dt_holidays, prophet_country, prophet_vars, prophet_signs) {
  if (is.null(prophet_vars)) {
    prophet_signs <- NULL
    prophet_country <- NULL
  } else if (!all(prophet_vars %in% c("trend", "season", "weekday", "holiday"))) {
    stop("Allowed values for 'prophet_vars' are: 'trend', 'season', 'weekday', and 'holiday'")
  } else if (is.null(prophet_country) | length(prophet_country) > 1) {
    stop(paste(
      "You must provide 1 country code in 'prophet_country' input.",
      dt_holidays[, uniqueN(country)], "countries are included:",
      paste(dt_holidays[, unique(country)], collapse = ", "),
      "\nIf your country is not available, please add it to the holidays.csv first."
    ))
  } else if (is.null(prophet_signs)) {
    prophet_signs <- rep("default", length(prophet_vars))
    message("'prophet_signs' is not provided. 'default' is used")
  } else if (length(prophet_signs) != length(prophet_vars) |!all(prophet_signs %in% c("positive", "negative", "default"))) {
    stop("'prophet_signs' must have same length as 'prophet_vars'. Allowed values are 'positive', 'negative', 'default'")
  }
}
