#' @rdname as_rjd2
#' @export
as_rjd2.JD3_TRAMOSEATS_SPEC <- function(x, context = NULL, ...) {
  basic_spec <- x$tramo$basic
  transform_spec <- x$tramo$transform
  outlier_spec <- x$tramo$outlier
  outlier_spec_span <- as_rjd2(outlier_spec$span)

  automodel_spec <- x$tramo$automodel
  arima_spec <- as_rjd2(x$tramo$arima, automdl = automodel_spec$enabled)
  regression_spec <- x$tramo$regression
  ud_outlier <- as_rjd2_ud_outliers(regression_spec$outliers)
  ud_var <- as_rjd2_ud_var(regression_spec, context = context)
  td_spec <- regression_spec$td
  easter_spec <- regression_spec$easter
  estimate_spec <- x$tramo$estimate
  estimate_span <- as_rjd2(estimate_spec$span)


  seats_spec <- x$seats
  benchmarking_spec <- x$benchmarking

  RJDemetra::tramoseats_spec(
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
    transform.fct = transform_spec$fct,
    usrdef.outliersEnabled = ud_outlier$enabled,
    usrdef.outliersType = ud_outlier$type,
    usrdef.outliersDate = ud_outlier$date,
    usrdef.outliersCoef = ud_outlier$coef,
    usrdef.varEnabled = ud_var$varEnabled,
    usrdef.var = ud_var$var,
    usrdef.varType = ud_var$varType,
    usrdef.varCoef = ud_var$varCoef,
    tradingdays.mauto = switch2(
      char = c("Unused", "FTest", "WaldTest"),
      match = td_spec$auto
    ),
    tradingdays.pftd = td_spec$ptest,
    tradingdays.option = switch(
      EXPR = td_spec$td,
      "TD2" = "WorkingDays",
      "TD7" = "TradingDays",
      "TD_NONE" = ifelse(td_spec$holidays == "" & length(td_spec$users)>0,"UserDefined", "None"),
      NA
    ),
    tradingdays.leapyear = td_spec$lp == "LEAPYEAR",
    tradingdays.stocktd = ifelse(td_spec$w>0, td_spec$w, NA_integer_),
    tradingdays.test = switch2(
      char = c("Separate_T", "Joint_F", "None"),
      match = td_spec$test
    ),
    easter.type = switch2(
      char = c("Unused", "Standard", "IncludeEaster", "IncludeEasterMonday"),
      match = easter_spec$type
    ),
    easter.julian = easter_spec$julian,
    easter.duration = easter_spec$duration,
    easter.test = easter_spec$test,
    outlier.enabled = outlier_spec$enabled,
    outlier.from = outlier_spec_span$from,
    outlier.to = outlier_spec_span$to,
    outlier.first = outlier_spec_span$first,
    outlier.last = outlier_spec_span$last,
    outlier.exclFirst = outlier_spec_span$exclFirst,
    outlier.exclLast = outlier_spec_span$exclLast,
    outlier.ao = outlier_spec$ao,
    outlier.tc = outlier_spec$tc,
    outlier.ls = outlier_spec$ls,
    outlier.so = outlier_spec$so,
    outlier.usedefcv = outlier_spec$va == 0,
    outlier.cv = ifelse(outlier_spec$va == 0, NA_integer_, outlier_spec$va),
    outlier.tcrate = outlier_spec$tcrate,
    automdl.enabled = automodel_spec$enabled,
    automdl.acceptdefault = automodel_spec$acceptdef,
    automdl.cancel = automodel_spec$cancel,
    # automdl.ub1 = automodel_spec$ub1,
    # automdl.ub2 = automodel_spec$ub2,
    automdl.armalimit = automodel_spec$tsig,
    automdl.reducecv = automodel_spec$pc,
    automdl.ljungboxlimit = automodel_spec$pcr,
    automdl.compare = automodel_spec$amicompare,
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
    seats.predictionLength = seats_spec$nfcasts,
    seats.approx = switch2(
      char = c("None", "Legacy", "Noisy"),
      match = seats_spec$approximation
    ),
    seats.trendBoundary = seats_spec$rmod,
    seats.seasdBoundary = seats_spec$sbound,
    seats.seasdBoundary1 = seats_spec$sboundatpi,
    seats.seasTol = seats_spec$epsphi,
    seats.maBoundary = seats_spec$xl,
    seats.method = switch2(
      char = c("Burman", "KalmanSmoother", "McElroyMatrix"),
      match = seats_spec$algorithm
    ),
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
as_rjd2.JD3_TRAMO_SPEC <- function(x, context = NULL, ...) {
  basic_spec <- x$basic
  transform_spec <- x$transform
  outlier_spec <- x$outlier
  outlier_spec_span <- as_rjd2(outlier_spec$span)

  automodel_spec <- x$automodel
  arima_spec <- as_rjd2(x$arima, automdl = automodel_spec$enabled)
  regression_spec <- x$regression
  ud_outlier <- as_rjd2_ud_outliers(regression_spec$outliers)
  ud_var <- as_rjd2_ud_var(regression_spec, context = context)
  td_spec <- regression_spec$td
  easter_spec <- regression_spec$easter
  estimate_spec <- x$estimate
  estimate_span <- as_rjd2(estimate_spec$span)


  RJDemetra::regarima_spec_tramoseats(
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
    transform.fct = transform_spec$fct,
    usrdef.outliersEnabled = ud_outlier$enabled,
    usrdef.outliersType = ud_outlier$type,
    usrdef.outliersDate = ud_outlier$date,
    usrdef.outliersCoef = ud_outlier$coef,
    usrdef.varEnabled = ud_var$varEnabled,
    usrdef.var = ud_var$var,
    usrdef.varType = ud_var$varType,
    usrdef.varCoef = ud_var$varCoef,
    tradingdays.mauto = switch2(
      char = c("Unused", "FTest", "WaldTest"),
      match = td_spec$auto
    ),
    tradingdays.pftd = td_spec$ptest,
    tradingdays.option = switch(
      EXPR = td_spec$td,
      "TD2" = "WorkingDays",
      "TD7" = "TradingDays",
      "TD_NONE" = ifelse(td_spec$holidays == "" & length(td_spec$users)>0,"UserDefined", "None"),
      NA
    ),
    tradingdays.leapyear = td_spec$lp == "LEAPYEAR",
    tradingdays.stocktd = ifelse(td_spec$w>0, td_spec$w, NA_integer_),
    tradingdays.test = switch2(
      char = c("Separate_T", "Joint_F", "None"),
      match = td_spec$test
    ),
    easter.type = switch2(
      char = c("Unused", "Standard", "IncludeEaster", "IncludeEasterMonday"),
      match = easter_spec$type
    ),
    easter.julian = easter_spec$julian,
    easter.duration = easter_spec$duration,
    easter.test = easter_spec$test,
    outlier.enabled = outlier_spec$enabled,
    outlier.from = outlier_spec_span$from,
    outlier.to = outlier_spec_span$to,
    outlier.first = outlier_spec_span$first,
    outlier.last = outlier_spec_span$last,
    outlier.exclFirst = outlier_spec_span$exclFirst,
    outlier.exclLast = outlier_spec_span$exclLast,
    outlier.ao = outlier_spec$ao,
    outlier.tc = outlier_spec$tc,
    outlier.ls = outlier_spec$ls,
    outlier.so = outlier_spec$so,
    outlier.usedefcv = outlier_spec$va == 0,
    outlier.cv = ifelse(outlier_spec$va == 0, NA_integer_, outlier_spec$va),
    outlier.tcrate = outlier_spec$tcrate,
    automdl.enabled = automodel_spec$enabled,
    automdl.acceptdefault = automodel_spec$acceptdef,
    automdl.cancel = automodel_spec$cancel,
    # automdl.ub1 = automodel_spec$ub1,
    # automdl.ub2 = automodel_spec$ub2,
    automdl.armalimit = automodel_spec$tsig,
    automdl.reducecv = automodel_spec$pc,
    automdl.ljungboxlimit = automodel_spec$pcr,
    automdl.compare = automodel_spec$amicompare,
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
as_rjd2.JD3_TRAMOSEATS_OUTPUT <- function(x, context = NULL, specification = c("estimation_spec", "result_spec"), ...) {
  specification <- match.arg(tolower(specification)[1], c("estimation_spec", "result_spec"))
  new_spec <- as_rjd2(x[[specification]], context = context)
  y <- x$result$final$series$data
  RJDemetra::tramoseats(
    series = y,
    spec = new_spec
  )
}
#' @rdname as_rjd2
#' @export
as_rjd2.JD3_TRAMO_OUTPUT<- function(x, context = NULL, specification = c("estimation_spec", "result_spec"), y = NULL, ...) {
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
