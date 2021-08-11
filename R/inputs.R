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
#' \dontrun{
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
#' }
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
    context_signs <- organic$context_signs

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

    ## get all hyper names
    local_name <- hyper_names(adstock, all_media)

    ## check hyperparameters
    check_hyperparameters(hyperparameters, adstock, all_media)

    ## check calibration
    check_calibration(dt_input, date_var, calibration_input, dayInterval, iterations, trials)

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
                         ,local_name=local_name
                         ,calibration_input=calibration_input)

    invisible(return(InputCollect))

  } else {
    # when adding hyperparameters
    check_hyperparameters(hyperparameters, InputCollect$adstock, InputCollect$all_media)
    InputCollect$hyperparameters <- hyperparameters
    # when all provided inputs are valid and have everything needed -> robyn_engineering()
    outFeatEng <- robyn_engineering(InputCollect = InputCollect, refresh = FALSE)
    invisible(return(outFeatEng))
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

  paid_media_vars <- InputCollect$paid_media_vars
  paid_media_spends <- InputCollect$paid_media_spends
  context_vars <- InputCollect$context_vars
  organic_vars <- InputCollect$organic_vars
  all_media <- InputCollect$all_media
  all_ind_vars <- InputCollect$all_ind_vars

  dt_input <- copy(InputCollect$dt_input) # dt_input <- copy(InputCollect$dt_input)

  dt_inputRollWind <- dt_input[InputCollect$rollingWindowStartWhich:InputCollect$rollingWindowEndWhich]

  dt_transform <- copy(InputCollect$dt_input) # dt_transform <- copy(InputCollect$dt_input)
  setnames(dt_transform, InputCollect$date_var, "ds", skip_absent = TRUE)
  dt_transform <- dt_transform[, ':='(ds= as.Date(ds))][order(ds)]
  dt_transformRollWind <- dt_transform[InputCollect$rollingWindowStartWhich:InputCollect$rollingWindowEndWhich]

  setnames(dt_transform, InputCollect$dep_var, "dep_var", skip_absent = TRUE)

  ################################################################
  #### model exposure metric from spend

  mediaCostFactor <- unlist(dt_inputRollWind[, lapply(.SD, sum), .SDcols = paid_media_spends] / dt_inputRollWind[, lapply(.SD, sum), .SDcols = paid_media_vars])
  names(mediaCostFactor) <- paid_media_vars
  costSelector <- !(paid_media_spends == paid_media_vars)
  names(costSelector) <- paid_media_vars

  if (any(costSelector)) {
    modNLSCollect <- list()
    yhatCollect <- list()
    plotNLSCollect <- list()
    for (i in 1:InputCollect$mediaVarCount) {
      if (costSelector[i]) {
        dt_spendModInput <- dt_inputRollWind[, c(paid_media_spends[i],paid_media_vars[i]), with =FALSE]
        setnames(dt_spendModInput, names(dt_spendModInput), c("spend", "exposure"))
        #dt_spendModInput <- dt_spendModInput[spend !=0 & exposure != 0]

        # scale 0 spend and exposure to a tiny number
        dt_spendModInput[, spend:=as.numeric(spend)][spend==0, spend:=0.01] # remove spend == 0 to avoid DIV/0 error
        dt_spendModInput[, exposure:=as.numeric(exposure)][exposure==0, exposure:=spend / mediaCostFactor[i]] # adapt exposure with avg when spend == 0

        tryCatch(
          {
            #dt_spendModInput[, exposure:= rep(0,(nrow(dt_spendModInput)))]
            nlsStartVal <- list(Vmax = dt_spendModInput[, max(exposure)], Km = dt_spendModInput[, max(exposure)/2])
            suppressWarnings(modNLS <- nlsLM(exposure ~ Vmax * spend/(Km + spend), #Michaelis-Menten model Vmax * spend/(Km + spend)
                                             data = dt_spendModInput,
                                             start = nlsStartVal
                                             ,control = nls.control(warnOnly = TRUE)))

            yhatNLS <- predict(modNLS)
            modNLSSum <- summary(modNLS)

            # QA nls model prediction
            yhatNLSQA <- modNLSSum$coefficients[1,1] * dt_spendModInput$spend / (modNLSSum$coefficients[2,1] + dt_spendModInput$spend) #exposure = v  * spend / (k + spend)
            identical(yhatNLS, yhatNLSQA)

            rsq_nls <- get_rsq(true = dt_spendModInput$exposure, predicted = yhatNLS)
          },

          error=function(cond) {
            message("michaelis menten fitting for ", paid_media_vars[i]," out of range. using lm instead")
          }
        )
        if (!exists("modNLS")) {modNLS <- NULL; yhatNLS <- NULL; modNLSSum <- NULL; rsq_nls <- NULL}
        # build lm comparison model
        modLM <- lm(exposure ~ spend-1, data = dt_spendModInput)
        yhatLM <- predict(modLM)
        modLMSum <- summary(modLM)
        rsq_lm <- get_rsq(true = dt_spendModInput$exposure, predicted = yhatLM)
        if (is.na(rsq_lm)) {stop("please check if ",paid_media_vars[i]," constains only 0")}

        # compare NLS & LM, takes LM if NLS fits worse
        costSelector[i] <- if(is.null(rsq_nls)) {FALSE} else {rsq_nls > rsq_lm}

        modNLSCollect[[paid_media_vars[i]]] <- data.table(channel = paid_media_vars[i],
                                                          Vmax = if (!is.null(modNLS)) {modNLSSum$coefficients[1,1]} else {NA},
                                                          Km =  if (!is.null(modNLS)) {modNLSSum$coefficients[2,1]} else {NA},
                                                          aic_nls = if (!is.null(modNLS)) {AIC(modNLS)} else {NA},
                                                          aic_lm = AIC(modLM),
                                                          bic_nls = if (!is.null(modNLS)) {BIC(modNLS)} else {NA},
                                                          bic_lm = BIC(modLM),
                                                          rsq_nls = if (!is.null(modNLS)) {rsq_nls} else {0},
                                                          rsq_lm = rsq_lm,
                                                          coef_lm = coef(modLMSum)[1]
        )

        dt_plotNLS <- data.table(channel = paid_media_vars[i],
                                 yhatNLS = if(costSelector[i]) {yhatNLS} else {yhatLM},
                                 yhatLM = yhatLM,
                                 y = dt_spendModInput$exposure,
                                 x = dt_spendModInput$spend)
        dt_plotNLS <- melt.data.table(dt_plotNLS, id.vars = c("channel", "y", "x"), variable.name = "models", value.name = "yhat")
        dt_plotNLS[, models:= str_remove(tolower(models), "yhat")]

        yhatCollect[[paid_media_vars[i]]] <- dt_plotNLS

        # create plot
        plotNLSCollect[[paid_media_vars[i]]] <- ggplot(dt_plotNLS, aes(x=x, y=y, color = models)) +
          geom_point() +
          geom_line(aes(y=yhat, x=x, color = models)) +
          labs(subtitle = paste0("y=",paid_media_vars[i],", x=", paid_media_spends[i],
                                 "\nnls: aic=", round(AIC(if(costSelector[i]) {modNLS} else {modLM}),0), ", rsq=", round(if(costSelector[i]) {rsq_nls} else {rsq_lm},4),
                                 "\nlm: aic= ", round(AIC(modLM),0), ", rsq=", round(rsq_lm,4)),
               x = "spend",
               y = "exposure"
          ) +
          theme(legend.position = 'bottom')

      }
    }

    modNLSCollect <- rbindlist(modNLSCollect)
    yhatNLSCollect <- rbindlist(yhatCollect)
    yhatNLSCollect[, ds:= rep(dt_transformRollWind$ds, nrow(yhatNLSCollect)/nrow(dt_transformRollWind))]

  } else {
    modNLSCollect <- NULL
    plotNLSCollect <- NULL
    yhatNLSCollect <- NULL
  }

  getSpendSum <- dt_input[, lapply(.SD, sum), .SDcols=paid_media_spends]
  names(getSpendSum) <- paid_media_vars
  getSpendSum <- suppressWarnings(melt.data.table(getSpendSum, measure.vars= paid_media_vars, variable.name = "rn", value.name = "spend"))

  ################################################################
  #### clean & aggregate data

  ## transform all factor variables
  factor_vars <- InputCollect$factor_vars
  if (length(factor_vars)>0) {
    dt_transform[, (factor_vars):= lapply(.SD, as.factor), .SDcols = factor_vars ]
  }

  ################################################################
  #### Obtain prophet trend, seasonality and changepoints

  if ( !is.null(InputCollect$prophet_vars) ) {

    if(length(InputCollect$prophet_vars) != length(InputCollect$prophet_signs)) {stop("InputCollect$prophet_vars and InputCollect$prophet_signs have to be the same length")}
    if(any(length(InputCollect$prophet_vars)==0, length(InputCollect$prophet_signs)==0)) {stop("InputCollect$prophet_vars and InputCollect$prophet_signs must be both specified")}
    if(!(InputCollect$prophet_country %in% InputCollect$dt_holidays$country)) {stop("InputCollect$prophet_country must be already included in the holidays.csv and as ISO 3166-1 alpha-2 abbreviation")}

    recurrance <- dt_transform[, .(ds = ds, y = dep_var)]
    use_trend <- any(str_detect("trend", InputCollect$prophet_vars))
    use_season <- any(str_detect("season", InputCollect$prophet_vars))
    use_weekday <- any(str_detect("weekday", InputCollect$prophet_vars))
    use_holiday <- any(str_detect("holiday", InputCollect$prophet_vars))

    if (InputCollect$intervalType == "day") {

      holidays <- InputCollect$dt_holidays

    } else if (InputCollect$intervalType == "week") {

      weekStartInput <- wday(dt_transform[1, ds])
      weekStartMonday <- if(weekStartInput==2) {TRUE} else if (weekStartInput==1) {FALSE} else {stop("week start has to be Monday or Sunday")}
      InputCollect$dt_holidays[, dsWeekStart:= cut(as.Date(ds), breaks = InputCollect$intervalType, start.on.monday = weekStartMonday)]
      holidays <- InputCollect$dt_holidays[, .(ds=dsWeekStart, holiday, country, year)]
      holidays <- holidays[, lapply(.SD, paste0, collapse="#"), by = c("ds", "country", "year"), .SDcols = "holiday"]

    } else if (InputCollect$intervalType == "month") {

      monthStartInput <- all(day(dt_transform[, ds]) == 1)
      if (monthStartInput == FALSE) {
        stop("Monthly data should have first day of month as datestampe, e.g.'2020-01-01'")
      }
      InputCollect$dt_holidays[, dsMonthStart:= cut(as.Date(ds), InputCollect$intervalType)]
      holidays <- InputCollect$dt_holidays[, .(ds=dsMonthStart, holiday, country, year)]
      holidays <- holidays[, lapply(.SD, paste0, collapse = "#"), by = c("ds", "country", "year"), .SDcols = "holiday"]

    }


    if (!is.null(factor_vars)) {
      dt_regressors <- cbind(recurrance, dt_transform[, c(context_vars, paid_media_vars), with =FALSE])
      modelRecurrance <- prophet(holidays = if(use_holiday) {holidays[country==InputCollect$prophet_country]} else {NULL}
                                 ,yearly.seasonality = use_season
                                 ,weekly.seasonality = use_weekday
                                 ,daily.seasonality= FALSE)
      # for (addreg in factor_vars) {
      #   modelRecurrance <- add_regressor(modelRecurrance, addreg)
      # }

      dt_ohe <- as.data.table(model.matrix(y ~., dt_regressors[, c("y",factor_vars), with =FALSE])[,-1])
      ohe_names <- names(dt_ohe)
      for (addreg in ohe_names) {
        modelRecurrance <- add_regressor(modelRecurrance, addreg)
      }
      dt_ohe <- cbind(dt_regressors[, !factor_vars, with=FALSE], dt_ohe)
      mod_ohe <- fit.prophet(modelRecurrance, dt_ohe)
      # prophet::regressor_coefficients(mxxx)
      dt_forecastRegressor <- predict(mod_ohe, dt_ohe)
      # prophet::prophet_plot_components(mod_ohe, dt_forecastRegressor)

      forecastRecurrance <- dt_forecastRegressor[, str_detect(names(dt_forecastRegressor), "_lower$|_upper$", negate = TRUE), with =FALSE]
      for (aggreg in factor_vars) {
        oheRegNames <- na.omit(str_extract(names(forecastRecurrance), paste0("^",aggreg, ".*")))
        forecastRecurrance[, (aggreg):=rowSums(.SD), .SDcols=oheRegNames]
        get_reg <- forecastRecurrance[, get(aggreg)]
        dt_transform[, (aggreg):= scale(get_reg, center = min(get_reg), scale = FALSE)]
        #dt_transform[, (aggreg):= get_reg]
      }
      # modelRecurrance <- fit.prophet(modelRecurrance, dt_regressors)
      # forecastRecurrance <- predict(modelRecurrance, dt_transform[, c("ds",context_vars, paid_media_vars), with =FALSE])
      # prophet_plot_components(modelRecurrance, forecastRecurrance)

    } else {
      modelRecurrance<- prophet(recurrance
                                ,holidays = if(use_holiday) {holidays[country==InputCollect$prophet_country]} else {NULL}
                                ,yearly.seasonality = use_season
                                ,weekly.seasonality = use_weekday
                                ,daily.seasonality= FALSE
                                #,changepoint.range = 0.8
                                #,seasonality.mode = 'multiplicative'
                                #,changepoint.prior.scale = 0.1
      )

      #futureDS <- make_future_dataframe(modelRecurrance, periods=1, freq = InputCollect$intervalType)
      forecastRecurrance <- predict(modelRecurrance, dt_transform[, "ds", with =FALSE])

    }

    # plot(modelRecurrance, forecastRecurrance)
    # prophet_plot_components(modelRecurrance, forecastRecurrance, render_plot = TRUE)

    if (use_trend) {
      fc_trend <- forecastRecurrance$trend[1:NROW(recurrance)]
      dt_transform[, trend := fc_trend]
      # recurrance[, trend := scale(fc_trend, center = min(fc_trend), scale = FALSE) + 1]
      # dt_transform[, trend := recurrance$trend]
    }
    if (use_season) {
      fc_season <- forecastRecurrance$yearly[1:NROW(recurrance)]
      dt_transform[, season := fc_season]
      # recurrance[, seasonal := scale(fc_season, center = min(fc_season), scale = FALSE) + 1]
      # dt_transform[, season := recurrance$seasonal]
    }
    if (use_weekday) {
      fc_weekday <- forecastRecurrance$weekly[1:NROW(recurrance)]
      dt_transform[, weekday := fc_weekday]
      # recurrance[, weekday := scale(fc_weekday, center = min(fc_weekday), scale = FALSE) + 1]
      # dt_transform[, weekday := recurrance$weekday]
    }
    if (use_holiday) {
      fc_holiday <- forecastRecurrance$holidays[1:NROW(recurrance)]
      dt_transform[, holiday := fc_holiday]
      # recurrance[, holidays := scale(fc_holiday, center = min(fc_holiday), scale = FALSE) + 1]
      # dt_transform[, holiday := recurrance$holidays]
    }
  }

  ################################################################
  #### Finalize input

  #dt <- dt[, all_name, with = FALSE]
  dt_transform <- dt_transform[, c("ds", "dep_var", all_ind_vars), with = FALSE]

  InputCollect$dt_mod <- dt_transform
  InputCollect$dt_modRollWind <- dt_transform[InputCollect$rollingWindowStartWhich:InputCollect$rollingWindowEndWhich]
  InputCollect$dt_inputRollWind <- dt_inputRollWind
  InputCollect$all_media <- all_media

  InputCollect[['modNLSCollect']] <- modNLSCollect
  InputCollect[['plotNLSCollect']] <- plotNLSCollect
  InputCollect[['yhatNLSCollect']] <- yhatNLSCollect
  InputCollect[['costSelector']] <- costSelector
  InputCollect[['mediaCostFactor']] <- mediaCostFactor

  return(InputCollect)
}
