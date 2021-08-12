# Copyright (c) Facebook, Inc. and its affiliates.

# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# Includes function robyn_inputs(), hyper_names(), robyn_engineering()

####################################################################
#' Input data transformation
#'
#' Describe function.
#'
#' @param dt_input A data.table. Load simulated
#' dataset using \code{data("dt_simulated_weekly")}
#' @param dt_holidays A data.table. Load standard
#' Prophet holidays using \code{data("dt_prophet_holidays")}
#' @param date_var Character. Name of date variable. Daily, weekly
#' and monthly data supported. Weekly requires weekstart of Monday or Sunday.
#' date_var must have format "2020-01-01"
#' @param dep_var Character. Name of dependent variable. Only one allowed
#' @param dep_var_type Character. Type of dependent variable
#' as "revenue" or "conversion". Only one allowed and case sensitive.
#' @param prophet_vars Character vector. Include any of "trend",
#' "season", "weekday", "holiday". Are case-sensitive. Highly recommended
#' to use all for daily data and "trend", "season", "holiday" for
#' weekly and above cadence
#' @param prophet_signs Character vector. Choose any of
#' \code{c("default", "positive", "negative")}. Control
#' the signs of coefficients for prophet variables. Must have same
#' order and same length as \code{prophet_vars}.
#' @param prophet_country Character. Only one country allowed once.
#' Including national holidays for 59 countries, whose list can
#' be found loading \code{data("dt_prophet_holidays")}.
#' @param context_vars Character vector. Typically competitors,
#' price & promotion, temperature, unemployment rate, etc.
#' @param context_signs Character vector. Choose any of
#' \code{c("default", "positive", "negative")}. Control
#' the signs of coefficients for context_vars. Must have same
#' order and same length as \code{context_vars}.
#' @param paid_media_vars Character vector. Recommended to use exposure
#' level metrics (impressions, clicks, GRP etc) other than spend. Also
#' recommended to split media channel into sub-channels
#' (e.g. fb_retargeting, fb_prospecting etc.) to gain more variance.
#' paid_media_vars only accept numerical variable
#' @param paid_media_signs Character vector. Choose any of
#' \code{c("default", "positive", "negative")}. Control
#' the signs of coefficients for paid_media_vars. Must have same
#' order and same length as \code{paid_media_vars}.
#' @param paid_media_spends Character vector. When using exposure level
#' metrics (impressions, clicks, GRP etc) in paid_media_vars, provide
#' corresponding spends for ROAS calculation. For spend metrics in
#' paid_media_vars, use the same name. media_spend_vars must have same
#' order and same length as \code{paid_media_vars}.
#' @param organic_vars Character vector. Typically newsletter sendings,
#' push-notifications, social media posts etc. Compared to paid_media_vars
#' organic_vars are often  marketing activities without clear spends
#' @param organic_signs Character vector. Choose any of
#' \code{c("default", "positive", "negative")}. Control
#' the signs of coefficients for organic_signs. Must have same
#' order and same length as \code{organic_vars}.
#' @param factor_vars Character vector. Specify which of the provided
#' variables in organic_vars or context_vars should be forced as a factor
#' @param adstock Character. Choose any of \code{c("geometric", "weibull")}.
#' Weibull adtock is a two-parametric function and thus more flexible, but
#' takes longer time than the traditional geometric one-parametric function.
#' Time estimation: with geometric adstock, 2000 iterations * 5 trials on 8
#' cores, it takes less than 30 minutes. Weibull takes at least twice as
#' much time.
#' @param hyperparameters List containing hyperparameter lower and upper bounds.
#' Names of elements in list must be identical to output of \code{hyper_names()}
#' @param window_start Character. Set start date of modelling period.
#' Recommended to not start in the first date in dataset to gain adstock
#' effect from previous periods.
#' @param window_end Character. Set end date of modelling period. Recommended
#' to have ratio of independent variable: data points of 1:10.
#' @param cores Integer. Default to \code{parallel::detectCores()}
#' @param iterations Integer. Recommended 2000 for default
#' \code{nevergrad_algo = "TwoPointsDE"}
#' @param trials Integer. Recommended 5 for default
#' \code{nevergrad_algo = "TwoPointsDE"}
#' @param nevergrad_algo Character. Default to "TwoPointsDE". Options are
#' \code{c("DE","TwoPointsDE", "OnePlusOne", "DoubleFastGADiscreteOnePlusOne",
#' "DiscreteOnePlusOne", "PortfolioDiscreteOnePlusOne", "NaiveTBPSA",
#' "cGA", "RandomSearch")}
#' @param calibration_input A data.table. Optional provide experimental results.
#' @param InputCollect Default to NULL. \code{robyn_inputs}'s output when
#' \code{hyperparameters} are not yet set.
#' @examples
#' data("dt_simulated_weekly")
#' data("dt_prophet_holidays")
#'
#' ## Define model input using simulated dataset
#' InputCollect <- robyn_inputs(
#'   dt_input = dt_simulated_weekly
#'   ,dt_holidays = dt_prophet_holidays
#'
#'   ,date_var = "DATE"
#'   ,dep_var = "revenue"
#'   ,dep_var_type = "revenue"
#'
#'   ,prophet_vars = c("trend", "season", "holiday")
#'   ,prophet_signs = c("default","default", "default")
#'   ,prophet_country = "DE"
#'
#'   ,context_vars = c("competitor_sales_B", "events")
#'   ,context_signs = c("default", "default")
#'
#'   ,paid_media_vars = c("tv_S","ooh_S"	,	"print_S"	,"facebook_I"	,"search_clicks_P")
#'   ,paid_media_signs = c("positive", "positive","positive", "positive", "positive")
#'   ,paid_media_spends = c("tv_S","ooh_S",	"print_S"	,"facebook_S"	,"search_S")
#'
#'   ,organic_vars = c("newsletter")
#'   ,organic_signs = c("positive")
#'
#'   ,factor_vars = c("events")
#'
#'   ,window_start = "2016-11-23"
#'   ,window_end = "2018-08-22"
#'
#'   ,adstock = "geometric"
#'   ,iterations = 2000
#'   ,trials = 5
#' )
#'
#' ## Define hyperparameters
#' hyper_names(adstock = InputCollect$adstock, all_media = InputCollect$all_media)
#'
#' hyperparameters <- list(
#'   facebook_I_alphas = c(0.5, 3) # example bounds for alpha
#'   ,facebook_I_gammas = c(0.3, 1) # example bounds for gamma
#'   ,facebook_I_thetas = c(0, 0.3) # example bounds for theta
#'   #,facebook_I_shapes = c(0.0001, 2) # example bounds for shape
#'   #,facebook_I_scales = c(0, 0.1) # example bounds for scale
#'
#'   ,print_S_alphas = c(0.5, 3)
#'   ,print_S_gammas = c(0.3, 1)
#'   ,print_S_thetas = c(0.1, 0.4)
#'   #,print_S_shapes = c(0.0001, 2)
#'   #,print_S_scales = c(0, 0.1)
#'
#'   ,tv_S_alphas = c(0.5, 3)
#'   ,tv_S_gammas = c(0.3, 1)
#'   ,tv_S_thetas = c(0.3, 0.8)
#'   #,tv_S_shapes = c(0.0001, 2)
#'   #,tv_S_scales= c(0, 0.1)
#'
#'   ,search_clicks_P_alphas = c(0.5, 3)
#'   ,search_clicks_P_gammas = c(0.3, 1)
#'   ,search_clicks_P_thetas = c(0, 0.3)
#'   #,search_clicks_P_shapes = c(0.0001, 2)
#'   #,search_clicks_P_scales = c(0, 0.1)
#'
#'   ,ooh_S_alphas = c(0.5, 3)
#'   ,ooh_S_gammas = c(0.3, 1)
#'   ,ooh_S_thetas = c(0.1, 0.4)
#'   #,ooh_S_shapes = c(0.0001, 2)
#'   #,ooh_S_scales = c(0, 0.1)
#'
#'   ,newsletter_alphas = c(0.5, 3)
#'   ,newsletter_gammas = c(0.3, 1)
#'   ,newsletter_thetas = c(0.1, 0.4)
#'   #,newsletter_shapes = c(0.0001, 2)
#'   #,newsletter_scales = c(0, 0.1)
#' )
#'
#' ## Add hyperparameters into robyn_inputs()
#' InputCollect <- robyn_inputs(InputCollect = InputCollect, hyperparameters = hyperparameters)
#' @return List object
#' @export
robyn_inputs <- function(dt_input = NULL
                         ,dt_holidays = NULL
                         ,date_var = "auto"
                         ,dep_var = NULL
                         ,dep_var_type = NULL
                         ,prophet_vars = NULL
                         ,prophet_signs = NULL
                         ,prophet_country = NULL
                         ,context_vars = NULL
                         ,context_signs = NULL
                         ,paid_media_vars = NULL
                         ,paid_media_signs = NULL
                         ,paid_media_spends = NULL
                         ,organic_vars = NULL
                         ,organic_signs = NULL
                         ,factor_vars = NULL
                         ,adstock = NULL
                         ,hyperparameters = NULL
                         ,window_start = NULL
                         ,window_end = NULL
                         ,cores = parallel::detectCores()
                         ,iterations = 2000
                         ,trials = 5
                         ,nevergrad_algo = "TwoPointsDE"
                         ,calibration_input = NULL
                         ,InputCollect = NULL
) {

  ### Use case 1: running robyn_inputs() for the first time
  if (is.null(InputCollect)) {

    ## check date input (and set dayInterval and intervalType)
    date_input <- check_datevar(dt_input, date_var)
    date_var <- date_input$date_var # when date_var = "auto"
    dayInterval <- date_input$dayInterval
    intervalType <- date_input$intervalType

    ## check dependent var
    check_depvar(dt_input, dep_var, dep_var_type)

    ## check prophet
    check_prophet(dt_holidays, prophet_country, prophet_vars, prophet_signs)

    ## check baseline variables (and maybe transform context_signs)
    context <- check_context(dt_input, context_vars, context_signs)
    context_signs <- context$context_signs

    ## check paid media variables (set mediaVarCount and maybe transform paid_media_signs)
    paidmedia <- check_paidmedia(dt_input, paid_media_vars, paid_media_signs, paid_media_spends)
    paid_media_signs <- paidmedia$paid_media_signs
    mediaVarCount <- paidmedia$mediaVarCount
    exposureVarName <- paid_media_vars[!(paid_media_vars == paid_media_spends)]

    ## check organic media variables (and maybe transform organic_signs)
    organic <- check_organicvars(dt_input, organic_vars, organic_signs)
    organic_signs <- organic$organic_signs

    ## check factor_vars
    check_factorvars(factor_vars, context_vars, organic_vars)

    ## check all vars
    all_media <- c(paid_media_vars, organic_vars)
    all_ind_vars <- c(prophet_vars, context_vars, all_media)
    check_allvars(all_ind_vars)

    ## check data dimension
    check_datadim(dt_input, all_ind_vars, rel = 10)

    ## check window_start & window_end (and transform parameters/data)
    windows <- check_windows(dt_input, date_var, all_media, window_start, window_end)
    dt_input <- windows$dt_input
    window_start <- windows$window_start
    rollingWindowStartWhich <- windows$rollingWindowStartWhich
    refreshAddedStart <- windows$refreshAddedStart
    window_end <- windows$window_end
    rollingWindowEndWhich <- windows$rollingWindowEndWhich
    rollingWindowLength <- windows$rollingWindowLength

    ## check adstock
    check_adstock(adstock)

    ## check hyperparameters (if passed)
    check_hyperparameters(hyperparameters, adstock, all_media)

    ## check calibration and iters/trials
    calibration_input <- check_calibration(dt_input, date_var, calibration_input, dayInterval)

    ## collect input
    InputCollect <- list(dt_input=dt_input
                         ,dt_holidays=dt_holidays
                         ,dt_mod=NULL
                         ,dt_modRollWind=NULL
                         ,xDecompAggPrev = NULL
                         ,date_var=date_var
                         ,dayInterval=dayInterval
                         ,intervalType=intervalType

                         ,dep_var=dep_var
                         ,dep_var_type=dep_var_type

                         ,prophet_vars=prophet_vars
                         ,prophet_signs=prophet_signs
                         ,prophet_country=prophet_country

                         ,context_vars=context_vars
                         ,context_signs=context_signs

                         ,paid_media_vars=paid_media_vars
                         ,paid_media_signs=paid_media_signs
                         ,paid_media_spends=paid_media_spends
                         ,mediaVarCount=mediaVarCount
                         ,exposureVarName=exposureVarName
                         ,organic_vars=organic_vars
                         ,organic_signs=organic_signs
                         ,all_media=all_media
                         ,all_ind_vars=all_ind_vars

                         ,factor_vars=factor_vars

                         ,cores=cores

                         ,window_start=window_start
                         ,rollingWindowStartWhich=rollingWindowStartWhich
                         ,window_end=window_end
                         ,rollingWindowEndWhich=rollingWindowEndWhich
                         ,rollingWindowLength=rollingWindowLength
                         ,refreshAddedStart=refreshAddedStart

                         ,adstock=adstock
                         ,iterations=iterations

                         ,nevergrad_algo=nevergrad_algo
                         ,trials=trials

                         ,hyperparameters = hyperparameters
                         ,calibration_input = calibration_input)


    ### Use case 1: running robyn_inputs() for the first time
    if (is.null(hyperparameters)) {

      ### conditional output 1.1
      ## running robyn_inputs() for the 1st time & no 'hyperparameters' yet
      invisible(return(InputCollect))

    } else {

      ### conditional output 1.2
      ## running robyn_inputs() for the 1st time & 'hyperparameters' provided --> run robyn_engineering()
      check_iteration(calibration_input, iterations, trials)
      outFeatEng <- robyn_engineering(InputCollect = InputCollect, refresh = FALSE)
      invisible(return(outFeatEng))
    }

  } else {

    ### Use case 2: adding 'hyperparameters' and/or 'calibration_input' using robyn_inputs()

    ## check calibration and iters/trials
    calibration_input <- check_calibration(
      InputCollect$dt_input
      ,InputCollect$date_var
      ,calibration_input
      ,InputCollect$dayInterval)

    ## update calibration_input
    if (!is.null(calibration_input)) {
      InputCollect$calibration_input <- calibration_input
    }

    if (is.null(InputCollect$hyperparameters) & is.null(hyperparameters)) {

      ### conditional output 2.1
      ## when 'hyperparameters' is still not yet provided
      invisible(return(InputCollect))

    } else {

      ### conditional output 2.2
      ## 'hyperparameters' provided --> run robyn_engineering()

      ## update & check hyperparameters
      if (is.null(InputCollect$hyperparameters)) {
        InputCollect$hyperparameters <- hyperparameters
      }
      check_hyperparameters(InputCollect$hyperparameters, InputCollect$adstock, InputCollect$all_media)
      check_iteration(InputCollect$calibration_input, InputCollect$iterations, InputCollect$trials)

      outFeatEng <- robyn_engineering(InputCollect = InputCollect, refresh = FALSE)
      invisible(return(outFeatEng))
    }
  }
}

####################################################################
#' Get hyperparameter names
#'
#' Describe function.
#'
#' @param adstock Default to \code{InputCollect$adstock}
#' @param all_media Default to \code{InputCollect$all_media}
#' @export
hyper_names <- function(adstock, all_media) {
  check_adstock(adstock)
  global_name <- c("thetas",  "shapes",  "scales",  "alphas",  "gammas",  "lambdas")
  if (adstock == "geometric") {
    local_name <- sort(apply(expand.grid(all_media, global_name[global_name %like% 'thetas|alphas|gammas']), 1, paste, collapse="_"))
  } else if (adstock == "weibull") {
    local_name <- sort(apply(expand.grid(all_media, global_name[global_name %like% 'shapes|scales|alphas|gammas']), 1, paste, collapse="_"))
  }
  return(local_name)
}

####################################################################
#' Transform input data
#'
#' Describe function.
#'
#' @param InputCollect Default to \code{InputCollect}
#' @param refresh Default to FALSE. TRUE when using in robyn_refresh()
#' @export
robyn_engineering <- function(InputCollect, refresh = FALSE) {

  check_InputCollect(InputCollect)

  dt_input <- InputCollect$dt_input
  paid_media_vars <- InputCollect$paid_media_vars
  paid_media_spends <- InputCollect$paid_media_spends
  context_vars <- InputCollect$context_vars
  organic_vars <- InputCollect$organic_vars
  all_ind_vars <- InputCollect$all_ind_vars
  date_var <- InputCollect$date_var
  dep_var <- InputCollect$dep_var
  rollingWindowStartWhich <- InputCollect$rollingWindowStartWhich
  rollingWindowEndWhich <- InputCollect$rollingWindowEndWhich
  mediaVarCount <- InputCollect$mediaVarCount
  factor_vars <- InputCollect$factor_vars
  prophet_vars <- InputCollect$prophet_vars
  prophet_signs <- InputCollect$prophet_signs
  prophet_country <- InputCollect$prophet_country
  intervalType <- InputCollect$intervalType
  dt_holidays <- InputCollect$dt_holidays

  # dt_inputRollWind
  dt_inputRollWind <- dt_input[rollingWindowStartWhich:rollingWindowEndWhich,]

  # dt_transform
  dt_transform <- dt_input
  colnames(dt_transform)[colnames(dt_transform) == date_var] <- "ds"
  colnames(dt_transform)[colnames(dt_transform) == dep_var] <- "dep_var"
  dt_transform <- dt_transform[order(dt_transform$ds),]

  # dt_transformRollWind
  dt_transformRollWind <- dt_transform[rollingWindowStartWhich:rollingWindowEndWhich,]

  ################################################################
  #### model exposure metric from spend

  mediaCostFactor <- colSums(subset(dt_inputRollWind, select = paid_media_spends), na.rm = TRUE) /
    colSums(subset(dt_inputRollWind, select = paid_media_vars), na.rm = TRUE)

  costSelector <- paid_media_spends != paid_media_vars
  names(costSelector) <- paid_media_vars

  if (any(costSelector)) {

    modNLSCollect <- list()
    yhatCollect <- list()
    plotNLSCollect <- list()

    for (i in 1:mediaVarCount) {

      if (costSelector[i]) {

        # run models (NLS and/or LM)
        dt_spendModInput <- subset(dt_inputRollWind, select = c(paid_media_spends[i], paid_media_vars[i]))
        results <- runmodels(dt_spendModInput, mediaCostFactor[i], paid_media_vars[i])
        mod <- results$res
        # compare NLS & LM, takes LM if NLS fits worse
        costSelector[i] <- if (is.null(mod$rsq_nls)) FALSE else mod$rsq_nls > mod$rsq_lm
        # create plot
        dt_plotNLS <- data.table(channel = paid_media_vars[i],
                                 yhatNLS = if (costSelector[i]) results$yhatNLS else results$yhatLM,
                                 yhatLM = results$yhatLM,
                                 y = results$data$exposure,
                                 x = results$data$spend)
        dt_plotNLS <- melt.data.table(dt_plotNLS, id.vars = c("channel", "y", "x"),
                                      variable.name = "models", value.name = "yhat")
        dt_plotNLS[, models:= str_remove(tolower(models), "yhat")]
        yhatCollect[[paid_media_vars[i]]] <- dt_plotNLS
        models_plot <- ggplot(
          dt_plotNLS, aes(x=.data$x, y=.data$y, color = .data$models)) +
          geom_point() +
          geom_line(aes(y=.data$yhat, x=.data$x, color = .data$models)) +
          labs(caption = paste0(
            "y=", paid_media_vars[i], ", x=", paid_media_spends[i],
            "\nnls: aic=", round(AIC(if(costSelector[i]) results$modNLS else results$modLM), 0),
            ", rsq=", round(if(costSelector[i]) mod$rsq_nls else mod$rsq_lm, 4),
            "\nlm: aic= ", round(AIC(results$modLM),0), ", rsq=", round(mod$rsq_lm, 4)),
            title = "Models fit comparison",
            x = "Spend", y = "Exposure", color = "Model") +
          theme_minimal() +
          theme(legend.position = 'top', legend.justification = "left")

        # save results into modNLSCollect and plotNLSCollect
        modNLSCollect[[paid_media_vars[i]]] <- mod
        plotNLSCollect[[paid_media_vars[i]]] <- models_plot
      }
    }

    modNLSCollect <- rbindlist(modNLSCollect)
    yhatNLSCollect <- rbindlist(yhatCollect)
    yhatNLSCollect$ds <- rep(dt_transformRollWind$ds, nrow(yhatNLSCollect)/nrow(dt_transformRollWind))

  } else {
    modNLSCollect <- plotNLSCollect <- yhatNLSCollect <- NULL
  }

  getSpendSum <- colSums(subset(dt_input, select = paid_media_spends), na.rm = TRUE)
  getSpendSum <- data.frame(rn = names(getSpendSum), spend = getSpendSum, row.names = NULL)

  ################################################################
  #### clean & aggregate data

  ## transform all factor variables
  if (length(factor_vars) > 0) {
    dt_transform[, (factor_vars):= lapply(.SD, as.factor), .SDcols = factor_vars]
  }

  ################################################################
  #### Obtain prophet trend, seasonality and change-points

  if (!is.null(prophet_vars) ) {

    check_prophet(dt_holidays, prophet_country, prophet_vars, prophet_signs)

    recurrance <- subset(dt_transform, select = c("ds", "dep_var"))
    colnames(recurrance)[2] <- "y"

    holidays <- set_holidays(dt_transform, dt_holidays, intervalType)
    use_trend <- any(str_detect("trend", prophet_vars))
    use_season <- any(str_detect("season", prophet_vars))
    use_weekday <- any(str_detect("weekday", prophet_vars))
    use_holiday <- any(str_detect("holiday", prophet_vars))

    if (!is.null(factor_vars)) {
      dt_regressors <- cbind(recurrance, subset(dt_transform, select = c(context_vars, paid_media_vars)))
      modelRecurrance <- prophet(
        holidays = if (use_holiday) holidays[country == prophet_country] else NULL
        ,yearly.seasonality = use_season
        ,weekly.seasonality = use_weekday
        ,daily.seasonality = FALSE)
      dt_ohe <- as.data.table(model.matrix(y ~., dt_regressors[, c("y",factor_vars), with =FALSE])[,-1])
      ohe_names <- names(dt_ohe)
      for (addreg in ohe_names) modelRecurrance <- add_regressor(modelRecurrance, addreg)
      dt_ohe <- cbind(dt_regressors[, !factor_vars, with = FALSE], dt_ohe)
      mod_ohe <- fit.prophet(modelRecurrance, dt_ohe)
      dt_forecastRegressor <- predict(mod_ohe, dt_ohe)
      forecastRecurrance <- dt_forecastRegressor[, str_detect(
        names(dt_forecastRegressor), "_lower$|_upper$", negate = TRUE), with = FALSE]
      for (aggreg in factor_vars) {
        oheRegNames <- na.omit(str_extract(names(forecastRecurrance), paste0("^",aggreg, ".*")))
        forecastRecurrance[, (aggreg):=rowSums(.SD), .SDcols=oheRegNames]
        get_reg <- forecastRecurrance[, get(aggreg)]
        dt_transform[, (aggreg):= scale(get_reg, center = min(get_reg), scale = FALSE)]
      }

    } else {
      modelRecurrance <- prophet(
        holidays = if (use_holiday) holidays[country == prophet_country] else NULL
        ,yearly.seasonality = use_season
        ,weekly.seasonality = use_weekday
        ,daily.seasonality = FALSE)
      forecastRecurrance <- predict(modelRecurrance, dt_transform$ds)
    }

    # use trend, season, weekday, holiday data
    dt_transform <- feats_forecast(dt_transform, recurrance, prophet_vars, forecastRecurrance)

  }

  ################################################################
  #### Finalize input

  dt_transform <- dt_transform[, c("ds", "dep_var", all_ind_vars), with = FALSE]

  InputCollect$dt_mod <- dt_transform
  InputCollect$dt_modRollWind <- dt_transform[rollingWindowStartWhich:rollingWindowEndWhich,]
  InputCollect$dt_inputRollWind <- dt_inputRollWind

  InputCollect[['modNLSCollect']] <- modNLSCollect
  InputCollect[['plotNLSCollect']] <- plotNLSCollect
  InputCollect[['yhatNLSCollect']] <- yhatNLSCollect
  InputCollect[['costSelector']] <- costSelector
  InputCollect[['mediaCostFactor']] <- mediaCostFactor

  return(InputCollect)
}

runmodels <- function(dt_spendModInput, mediaCostFactor, paid_media_vars) {

  if (ncol(dt_spendModInput) != 2) stop("Pass only 2 columns")
  colnames(dt_spendModInput) <- c("spend", "exposure")

  # remove spend == 0 to avoid DIV/0 error
  dt_spendModInput$spend[dt_spendModInput$spend == 0] <- 0.01
  # adapt exposure with avg when spend == 0
  dt_spendModInput$exposure <- ifelse(
    dt_spendModInput$exposure == 0, dt_spendModInput$spend / mediaCostFactor,
    dt_spendModInput$exposure)

  # Model 1: Michaelis-Menten model Vmax * spend/(Km + spend)
  tryCatch({
    nlsStartVal <- list(Vmax = dt_spendModInput[, max(exposure)],
                        Km = dt_spendModInput[, max(exposure)/2])

    modNLS <- nlsLM(exposure ~ Vmax * spend/(Km + spend),
                    data = dt_spendModInput,
                    start = nlsStartVal,
                    control = nls.control(warnOnly = TRUE))
    yhatNLS <- predict(modNLS)
    modNLSSum <- summary(modNLS)
    rsq_nls <- get_rsq(true = dt_spendModInput$exposure, predicted = yhatNLS)

    # # QA nls model prediction: check
    # yhatNLSQA <- modNLSSum$coefficients[1,1] * dt_spendModInput$spend / (modNLSSum$coefficients[2,1] + dt_spendModInput$spend) #exposure = v  * spend / (k + spend)
    # identical(yhatNLS, yhatNLSQA)
  },

  error = function(cond) {
    message("Michaelis-Menten fitting for ", paid_media_vars," out of range. Using lm instead")
    modNLS <- yhatNLS <- modNLSSum <- rsq_nls <- NULL
  })

  # build lm comparison model
  modLM <- lm(exposure ~ spend-1, data = dt_spendModInput)
  yhatLM <- predict(modLM)
  modLMSum <- summary(modLM)
  rsq_lm <- get_rsq(true = dt_spendModInput$exposure, predicted = yhatLM)
  if (is.na(rsq_lm)) stop("Please check if ", paid_media_vars, " contains only 0s")

  output <- list(res = data.table(
    channel = paid_media_vars,
    Vmax = if (!is.null(modNLS)) modNLSSum$coefficients[1,1] else NA,
    Km =  if (!is.null(modNLS)) modNLSSum$coefficients[2,1] else NA,
    aic_nls = if (!is.null(modNLS)) AIC(modNLS) else NA,
    aic_lm = AIC(modLM),
    bic_nls = if (!is.null(modNLS)) BIC(modNLS) else NA,
    bic_lm = BIC(modLM),
    rsq_nls = if (!is.null(modNLS)) rsq_nls else 0,
    rsq_lm = rsq_lm,
    coef_lm = coef(modLMSum)[1]),
    yhatNLS = yhatNLS,
    modNLS = modNLS,
    yhatLM = yhatLM,
    modLM = modLM,
    data = dt_spendModInput)

  return(output)
}

set_holidays <- function(dt_transform, dt_holidays, intervalType) {

  opts <- c("day","week","month")
  if (!intervalType %in% opts) {
    stop("Pass a valid 'intervalType'. Any of: ", paste(opts, collapse = ", "))
  }

  if (intervalType == "day") {
    holidays <- dt_holidays
  }

  if (intervalType == "week") {
    weekStartInput <- lubridate::wday(dt_transform$ds[1], week_start = 1)
    if (!weekStartInput %in% c(1, 2)) stop("Week start has to be Monday or Sunday")
    dt_holidays$dsWeekStart <- floor_date(dt_holidays$ds, unit = "week", week_start = 1)
    holidays <- dt_holidays[, .(ds=dsWeekStart, holiday, country, year)]
    holidays <- holidays[, lapply(.SD, paste0, collapse="#"), by = c("ds", "country", "year"), .SDcols = "holiday"]
  }

  if (intervalType == "month") {
    monthStartInput <- all(day(dt_transform[, ds]) == 1)
    if (!monthStartInput) {
      stop("Monthly data should have first day of month as datestampe, e.g.'2020-01-01'")
    }
    dt_holidays[, dsMonthStart:= cut(as.Date(ds), intervalType)]
    holidays <- dt_holidays[, .(ds=dsMonthStart, holiday, country, year)]
    holidays <- holidays[, lapply(.SD, paste0, collapse = "#"), by = c("ds", "country", "year"), .SDcols = "holiday"]
  }

  return(holidays)

}

feats_forecast <- function(dt_transform, recurrance, prophet_vars, forecastRecurrance) {

  use_trend <- any(str_detect("trend", prophet_vars))
  use_season <- any(str_detect("season", prophet_vars))
  use_weekday <- any(str_detect("weekday", prophet_vars))
  use_holiday <- any(str_detect("holiday", prophet_vars))

  if (use_trend) {
    dt_transform$trend <- forecastRecurrance$trend[1:nrow(recurrance)]
  }
  if (use_season) {
    dt_transform$season <- forecastRecurrance$yearly[1:nrow(recurrance)]
  }
  if (use_weekday) {
    dt_transform$weekday <- forecastRecurrance$weekly[1:nrow(recurrance)]
  }
  if (use_holiday) {
    dt_transform$holiday <- forecastRecurrance$holidays[1:nrow(recurrance)]
  }
  return(dt_transform)
}
