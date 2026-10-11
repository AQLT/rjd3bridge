#' @rdname as_rjd2
#' @export
as_rjd2.JD3_X13_SPEC <- function(x, context = NULL, ...) {
  basic_spec <- x$regarima$basic
  transform_spec <- x$regarima$transform # OK
  outlier_spec <- x$regarima$outlier
  outlier_spec_span <- as_rjd2(outlier_spec$span)
  if (length(outlier_spec$outliers) == 0) { # different TS
    outliers.enabled <- FALSE
    outliers_types <- NULL
  } else {
    outliers.enabled <- TRUE
    outliers_types <- sapply(outlier_spec$outliers, function(x) x[["type"]])
  }

  automodel_spec <- x$regarima$automodel #OK
  arima_spec <- as_rjd2(x$regarima$arima, automdl = automodel_spec$enabled) #OK
  regression_spec <- x$regarima$regression
  ud_outlier <- as_rjd2_ud_outliers(regression_spec$outliers)
  ud_var <- as_rjd2_ud_var(regression_spec, context = context)
  td_spec <- regression_spec$td # OK
  easter_spec <- regression_spec$easter #OK
  estimate_spec <- x$regarima$estimate #OK
  estimate_span <- as_rjd2(estimate_spec$span) #OKs


  x11_spec <- x$x11 #OK
  benchmarking_spec <- x$benchmarking #OK

  RJDemetra::x13_spec(
    preliminary.check = basic_spec$preliminaryCheck,
    estimate.from = estimate_span$from,
    estimate.to = estimate_span$to,
    estimate.first = estimate_span$first,
    estimate.last = estimate_span$last,
    estimate.exclFirst = estimate_span$exclFirst,
    estimate.exclLast = estimate_span$exclLast,
    estimate.tol = estimate_spec$tol,
    transform.function = switch2(
      char = c("Auto", "None", "Log"),
      match = transform_spec$fn
    ),
    transform.adjust = switch2(
      char = c("None", "LeapYear", "LengthOfPeriod"),
      match = transform_spec$adjust
    ),
    transform.aicdiff = transform_spec$aicdiff,
    usrdef.outliersEnabled = ud_outlier$enabled,
    usrdef.outliersType = ud_outlier$type,
    usrdef.outliersDate = ud_outlier$date,
    usrdef.outliersCoef = ud_outlier$coef,
    usrdef.varEnabled = ud_var$varEnabled,
    usrdef.var = ud_var$var,
    usrdef.varType = ud_var$varType,
    usrdef.varCoef = ud_var$varCoef,
    tradingdays.option = switch(
      EXPR = td_spec$td,
      "TD2" = "WorkingDays",
      "TD7" = "TradingDays",
      "TD_NONE" = ifelse(td_spec$holidays == "" & length(td_spec$users)>0,"UserDefined", "None"),
      NA
    ),
    tradingdays.autoadjust = td_spec$autoadjust,
    tradingdays.leapyear = switch2(
      char = c("LeapYear", "LengthOfPeriod", "None"),
      match = td_spec$lp
    ),
    tradingdays.stocktd = ifelse(td_spec$w>0, td_spec$w, NA_integer_),
    tradingdays.test = switch2(
      char = c("Remove", "Add", "None"),
      match = td_spec$test
    ),
    easter.enabled = easter_spec$type == "STANDARD",
    easter.julian = easter_spec$type == "JULIAN", # different tRAMO
    easter.duration = easter_spec$duration,
    easter.test = switch2(
      char = c("Add", "Remove", "None"),
      match = transform_spec$adjust
    ),
    outlier.enabled = outliers.enabled,
    outlier.from = outlier_spec_span$from,
    outlier.to = outlier_spec_span$to,
    outlier.first = outlier_spec_span$first,
    outlier.last = outlier_spec_span$last,
    outlier.exclFirst = outlier_spec_span$exclFirst,
    outlier.exclLast = outlier_spec_span$exclLast,
    outlier.ao = "AO" %in% outliers_types,
    outlier.tc = "TC" %in% outliers_types,
    outlier.ls = "LS" %in% outliers_types,
    outlier.so = "SO" %in% outliers_types,
    outlier.usedefcv = outlier_spec$defva == 0, # outlier_spec$va TS
    outlier.cv = ifelse(outlier_spec$defva == 0, NA_integer_, outlier_spec$defva), # outlier_spec$va TS
    outlier.method = switch2(
      char = c("AddOne", "AddAll"),
      match = outlier_spec$method
    ),
    outlier.tcrate = outlier_spec$monthlytcrate, # outlier_spec$tcrate TS
    automdl.enabled = automodel_spec$enabled,
    automdl.acceptdefault = automodel_spec$acceptdef,
    automdl.cancel = automodel_spec$cancel,
    # automdl.ub1 = automodel_spec$ub1,
    # automdl.ub2 = automodel_spec$ub2,
    automdl.mixed = automodel_spec$mixed,
    automdl.balanced = automodel_spec$balanced,
    automdl.armalimit = automodel_spec$tsig,
    automdl.reducecv = automodel_spec$predcv,
    automdl.ljungboxlimit = automodel_spec$ljungbox,
    automdl.ubfinal = automodel_spec$ubfinal,
    arima.mu = arima_spec$mu,
    arima.p = arima_spec$p,
    arima.d = arima_spec$d,
    arima.q = arima_spec$q,
    arima.bp = arima_spec$bp,
    arima.bd = arima_spec$bd,
    arima.bq = arima_spec$bq,
    arima.coefEnabled = arima_spec$coefEnabled,
    arima.coef = arima_spec$coef,
    arima.coefType = arima_spec$coefType,
    fcst.horizon = NA_integer_,
    x11.mode = switch2(
      char = c("Undefined", "Additive", "Multiplicative", "LogAdditive",
               "PseudoAdditive"),
      match = x11_spec$mode
    ),
    x11.seasonalComp = x11_spec$seasonal,
    x11.lsigma = x11_spec$lsig,
    x11.usigma = x11_spec$usig,
    x11.trendAuto = x11_spec$henderson == 0,
    x11.trendma = ifelse(x11_spec$henderson == 0, NA_integer_, x11_spec$henderson),
    x11.seasonalma = switch2(
      char = c("Msr", "Stable", "X11Default", "S3X1", "S3X3", "S3X5", "S3X9", "S3X15"),
      match = x11_spec$sfilters
    ),
    x11.fcasts = x11_spec$nfcasts,
    x11.bcasts = x11_spec$nbcasts,
    x11.calendarSigma = switch2(
      char = c("None", "All", "Signif", "Select"),
      match = x11_spec$sigma
    ),
    x11.sigmaVector = vect2NA(x11_spec$vsigmas),
    x11.excludeFcasts = x11_spec$excludefcasts,
    benchmarking.enabled = benchmarking_spec$enabled,
    benchmarking.target = switch2(
      char = c("Original", "CalendarAdjusted"),
      match = benchmarking_spec$target
    ),
    benchmarking.useforecast = benchmarking_spec$forecast,
    benchmarking.rho = benchmarking_spec$rho,
    benchmarking.lambda = benchmarking_spec$lambda
  )
}
#' @rdname as_rjd2
#' @export
as_rjd2.JD3_REGARIMA_SPEC <- function(x, context = NULL, ...) {
  basic_spec <- x$basic
  transform_spec <- x$transform # OK
  outlier_spec <- x$outlier
  outlier_spec_span <- as_rjd2(outlier_spec$span)
  if (length(outlier_spec$outliers) == 0) { # different TS
    outliers.enabled <- FALSE
    outliers_types <- NULL
  } else {
    outliers.enabled <- TRUE
    outliers_types <- sapply(outlier_spec$outliers, function(x) x[["type"]])
  }

  automodel_spec <- x$automodel #OK
  arima_spec <- as_rjd2(x$arima, automdl = automodel_spec$enabled) #OK
  regression_spec <- x$regression
  ud_outlier <- as_rjd2_ud_outliers(regression_spec$outliers)
  ud_var <- as_rjd2_ud_var(regression_spec, context = context)
  td_spec <- regression_spec$td # OK
  easter_spec <- regression_spec$easter #OK
  estimate_spec <- x$estimate #OK
  estimate_span <- as_rjd2(estimate_spec$span) #OKs


  x11_spec <- x$x11 #OK
  benchmarking_spec <- x$benchmarking #OK

  RJDemetra::regarima_spec_x13(
    preliminary.check = basic_spec$preliminaryCheck,
    estimate.from = estimate_span$from,
    estimate.to = estimate_span$to,
    estimate.first = estimate_span$first,
    estimate.last = estimate_span$last,
    estimate.exclFirst = estimate_span$exclFirst,
    estimate.exclLast = estimate_span$exclLast,
    estimate.tol = estimate_spec$tol,
    transform.function = switch2(
      char = c("Auto", "None", "Log"),
      match = transform_spec$fn
    ),
    transform.adjust = switch2(
      char = c("None", "LeapYear", "LengthOfPeriod"),
      match = transform_spec$adjust
    ),
    transform.aicdiff = transform_spec$aicdiff,
    usrdef.outliersEnabled = ud_outlier$enabled,
    usrdef.outliersType = ud_outlier$type,
    usrdef.outliersDate = ud_outlier$date,
    usrdef.outliersCoef = ud_outlier$coef,
    usrdef.varEnabled = ud_var$varEnabled,
    usrdef.var = ud_var$var,
    usrdef.varType = ud_var$varType,
    usrdef.varCoef = ud_var$varCoef,
    tradingdays.option = switch(
      EXPR = td_spec$td,
      "TD2" = "WorkingDays",
      "TD7" = "TradingDays",
      "TD_NONE" = ifelse(td_spec$holidays == "" & length(td_spec$users)>0,"UserDefined", "None"),
      NA
    ),
    tradingdays.autoadjust = td_spec$autoadjust,
    tradingdays.leapyear = switch2(
      char = c("LeapYear", "LengthOfPeriod", "None"),
      match = td_spec$lp
    ),
    tradingdays.stocktd = ifelse(td_spec$w>0, td_spec$w, NA_integer_),
    tradingdays.test = switch2(
      char = c("Remove", "Add", "None"),
      match = td_spec$test
    ),
    easter.enabled = easter_spec$type == "STANDARD",
    easter.julian = easter_spec$type == "JULIAN", # different tRAMO
    easter.duration = easter_spec$duration,
    easter.test = switch2(
      char = c("Add", "Remove", "None"),
      match = easter_spec$test
    ),
    outlier.enabled = outliers.enabled,
    outlier.from = outlier_spec_span$from,
    outlier.to = outlier_spec_span$to,
    outlier.first = outlier_spec_span$first,
    outlier.last = outlier_spec_span$last,
    outlier.exclFirst = outlier_spec_span$exclFirst,
    outlier.exclLast = outlier_spec_span$exclLast,
    outlier.ao = "AO" %in% outliers_types,
    outlier.tc = "TC" %in% outliers_types,
    outlier.ls = "LS" %in% outliers_types,
    outlier.so = "SO" %in% outliers_types,
    outlier.usedefcv = outlier_spec$defva == 0, # outlier_spec$va TS
    outlier.cv = ifelse(outlier_spec$defva == 0, NA_integer_, outlier_spec$defva), # outlier_spec$va TS
    outlier.method = switch2(
      char = c("AddOne", "AddAll"),
      match = outlier_spec$method
    ),
    outlier.tcrate = outlier_spec$monthlytcrate, # outlier_spec$tcrate TS
    automdl.enabled = automodel_spec$enabled,
    automdl.acceptdefault = automodel_spec$acceptdef,
    automdl.cancel = automodel_spec$cancel,
    # automdl.ub1 = automodel_spec$ub1,
    # automdl.ub2 = automodel_spec$ub2,
    automdl.mixed = automodel_spec$mixed,
    automdl.balanced = automodel_spec$balanced,
    automdl.armalimit = automodel_spec$tsig,
    automdl.reducecv = automodel_spec$predcv,
    automdl.ljungboxlimit = automodel_spec$ljungbox,
    automdl.ubfinal = automodel_spec$ubfinal,
    arima.mu = arima_spec$mu,
    arima.p = arima_spec$p,
    arima.d = arima_spec$d,
    arima.q = arima_spec$q,
    arima.bp = arima_spec$bp,
    arima.bd = arima_spec$bd,
    arima.bq = arima_spec$bq,
    arima.coefEnabled = arima_spec$coefEnabled,
    arima.coef = arima_spec$coef,
    arima.coefType = arima_spec$coefType,
    fcst.horizon = NA_integer_
  )
}
#' @rdname as_rjd2
#' @export
as_rjd2.JD3_X11_SPEC <- function(x, context = NULL, ...) {
  x11_spec <- x
  RJDemetra::x13_spec(
    spec = "X11",
    x11.mode = switch2(
      char = c("Undefined", "Additive", "Multiplicative", "LogAdditive",
               "PseudoAdditive"),
      match = x11_spec$mode
    ),
    x11.seasonalComp = x11_spec$seasonal,
    x11.lsigma = x11_spec$lsig,
    x11.usigma = x11_spec$usig,
    x11.trendAuto = x11_spec$henderson == 0,
    x11.trendma = ifelse(x11_spec$henderson == 0, NA_integer_, x11_spec$henderson),
    x11.seasonalma = switch2(
      char = c("Msr", "Stable", "X11Default", "S3X1", "S3X3", "S3X5", "S3X9", "S3X15"),
      match = x11_spec$sfilters
    ),
    x11.fcasts = x11_spec$nfcasts,
    x11.bcasts = x11_spec$nbcasts,
    x11.calendarSigma = switch2(
      char = c("None", "All", "Signif", "Select"),
      match = x11_spec$sigma
    ),
    x11.sigmaVector = vect2NA(x11_spec$vsigmas),
    x11.excludeFcasts = x11_spec$excludefcasts
  )
}
#' @rdname as_rjd2
#' @export
as_rjd2.JD3_X13_OUTPUT <- function(x, context = NULL, specification = c("estimation_spec", "result_spec"), ...) {
 specification <- match.arg(tolower(specification)[1], c("estimation_spec", "result_spec"))
 new_spec <- as_rjd2(x[[specification]], context = context)
 y <- x$result$preadjust$a1
 RJDemetra::x13(
   series = y,
   spec = new_spec
 )
}
#' @rdname as_rjd2
#' @export
as_rjd2.JD3_REGARIMA_OUTPUT<- function(x, context = NULL, specification = c("estimation_spec", "result_spec"), y = NULL, ...) {
  specification <- match.arg(tolower(specification)[1], c("estimation_spec", "result_spec"))
  new_spec <- as_rjd2(x[[specification]], context = context)
  if (is.null(y)){
    y <- ts(x$result$estimation$y, frequency = x$result_spec$basic$frequency)
  }
  RJDemetra::regarima(
    series = y,
    spec = new_spec
  )
}
