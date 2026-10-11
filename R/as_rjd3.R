#' Convert 'RJDemetra' (v2) Objects to 'rjd3' (v3) Format
#'
#' @description
#' Converts seasonal adjustment models, outputs, or specification objects
#' from legacy 'RJDemetra' (JDemetra+ v2) to the modern 'rjd3' ecosystem
#' (such as packages `rjd3x13` or `rjd3tramoseats` for JDemetra+ v3).
#'
#' @param x An object from the 'RJDemetra' package (v2, e.g., of class `SA`, `SA_spec`).
#' @param new_spec An optional specification object from the 'rjd3' ecosystem (v3) to be used as a base for conversion.
#' If `NULL`, a default specification will be created based on the type of model being converted.
#' @param ... Additional arguments passed to specific methods.
#'
#' @return An equivalent object compatible with the 'rjd3' ecosystem (v3 structure).
#'
#' @details
#' The parameters `ub1` and `ub2` are not translated since it seems to have a different convention between versions.
#'
#' @returns A list with two elements:
#' - `context`: An object of class [rjd3toolkit::modelling_context()] containing any user-defined variables.
#' - `result`: The converted object in the 'rjd3' format.
#'
#' @examplesIf rjd3jars::check_java_version(silent = TRUE)
#' \donttest{
#' y <- RJDemetra::ipi_c_eu[, "FR"]
#' model_v2 <- RJDemetra::x13(y, spec = "RSA5c")
#' model_v3 <- as_rjd3(model_v2)
#'
#' # Convert only a specification object
#' spec_v2 <- RJDemetra::regarima_spec_tramoseats()
#' spec_v3 <- as_rjd3(spec_v2)
#' }
#' @export
as_rjd3 <- function(x, new_spec = NULL,...){
  UseMethod("as_rjd3")
}
#' @rdname as_rjd3
#' @export
as_rjd3.SA_spec <- function(x, new_spec = NULL, ...){
  if (inherits(x, "X13")) {
    is_x11 <- all(lengths(x$regarima) == 0)
    if (is_x11 & is.null(new_spec)){
      new_spec <- rjd3x13::x11_spec()
    } else if (is.null(new_spec)) {
      new_spec <- rjd3x13::x13_spec()
    }
  } else {
    is_x11 <- FALSE
    if (is.null(new_spec)) {
      new_spec <- rjd3tramoseats::tramoseats_spec()
    }
  }
  regarima <- x$regarima
  benchmark <- x$benchmarking
  context <- rjd3toolkit::modelling_context()

  # Preprocessing
  if (!is_x11) {
    res <- as_rjd3(
      regarima,
      new_spec
    )
    new_spec <- res$result
    context <- res$context
  }

  # Decomposition
  if (inherits(x, "X13")) {
    new_spec <- as_rjd3(
        x$x11,
        new_spec
      )
  } else {
    new_spec <- as_rjd3(
        x$seats,
        new_spec
      )
  }

  # Benchmarking
  new_spec <- as_rjd3(
    benchmark,
    new_spec
  )
  list(
    context = context,
    result = new_spec
  )
}
#' @rdname as_rjd3
#' @export
as_rjd3.regarima <- function(x, new_spec = NULL, ...){
  is_x13 <- inherits(x, "X13")
  if (is_x13 & is.null(new_spec)) {
    new_spec <- rjd3x13::regarima_spec()
  } else if (is.null(new_spec)) {
    new_spec <- rjd3tramoseats::tramo_spec()
  }
  regarima_spec <- x$specification
  res <- as_rjd3(
    regarima_spec,
    new_spec
  )
  new_spec <- res$result
  context <- res$context

  effects <- x$model$effects
  if (x$model$spec_rslt["Log transformation"]) {
    y <- effects[,"y_lin"] + effects[,"tde"] + effects[,"ee"] + effects[,"omhe"] + effects[,"out"]
  } else {
    y <- exp(effects[,"y_lin"] + effects[,"tde"] + effects[,"ee"] + effects[,"omhe"] + effects[,"out"])
  }

  if (is_x13) {
    model <- rjd3x13::regarima(y, new_spec, context = context)
  } else {
    model <- rjd3tramoseats::tramoseats(y, new_spec, context = context)
  }
  list(
    context = context,
    result = model
  )
}
#' @rdname as_rjd3
#' @importFrom rjd3x13 regarima_spec
#' @importFrom rjd3tramoseats tramo_spec
#' @export
as_rjd3.SA <- function(x, new_spec = NULL, ...){
  is_x13 <- inherits(x, "X13")
  if (is_x13) {
    is_x11 <- is.null(x$regarima$arma)
    if (is_x11 & is.null(new_spec)){
      new_spec <- rjd3x13::x11_spec()
    } else if (is.null(new_spec)){
      new_spec <- rjd3x13::x13_spec()
    }
    decomp_spec_class <- "X11_spec"
  } else {
    is_x11 <- FALSE
    new_spec <- rjd3tramoseats::tramoseats_spec()
    decomp_spec_class <- "seats_spec"
  }
  regarima_spec <- add_class(x$regarima$specification, "regarima_spec")
  benchmark_spec <- add_class(x$benchmarking$specification, "benchmarking_spec")
  decomposition_spec <- add_class(x$decomposition$specification, decomp_spec_class)
  context <- rjd3toolkit::modelling_context()

  # Preprocessing
  if (!is_x11) {
    res <- as_rjd3(
      regarima_spec,
      new_spec
    )
    new_spec <- res$result
    context <- res$result
  }

  # Decomposition
  if (is_x13) {
    new_spec <- as_rjd3(
      decomposition_spec,
      new_spec
    )
  } else {
    new_spec <- as_rjd3(
      decomposition_spec,
      new_spec
    )
  }

  # Benchmarking
  new_spec <- as_rjd3(
    benchmark_spec,
    new_spec
  )

  y <- x$final$series[, "y"]

  if (is_x13) {
    if (is_x11) {
      model <- rjd3x13::x11(y, new_spec)
    } else {
      model <- rjd3x13::x13(y, new_spec, context = context)
    }
  } else {
    model <- rjd3tramoseats::tramoseats(y, new_spec, context = context)
  }
  list(
    context = context,
    result = model
  )
}
#' @rdname as_rjd3
#' @export
as_rjd3.regarima_spec <- function(x, new_spec = NULL, ...){
  if (is.null(new_spec)) {
    if (inherits(x, "X13")) {
      new_spec <- rjd3x13::regarima_spec()
    } else {
      new_spec <- rjd3tramoseats::tramo_spec()
    }
  }
  regarima <- x
  span <- regarima$span #OK
  estimate <- get_final(regarima$estimate) #OK
  outliers <- get_final(regarima$outliers)#OK
  automdl <- get_final(regarima$arima$specification) #OK
  arimacoef <- get_final(regarima$arima["coefficients"]) #OK
  if (all (is.na(arimacoef))){
    arimacoef <- list(Value = NA, coef.type = NA)
  }

  forecast <- get_final(regarima$forecast)
  udef.out <- get_final(regarima$regression$userdef["outliers"])
  udef.var <- get_final(regarima$regression$userdef["variables"])
  udef.spec <- get_final(regarima$regression$userdef$specification)

  td <- get_final(regarima$regression$trading.days)#OK
  easter <- get_final(regarima$regression$easter) #OK
  transform <- get_final(regarima$transform) #OK


  new_spec <- rjd3toolkit::set_automodel(
    new_spec,
    enabled = automdl$enabled,
    acceptdefault = automdl$automdl.acceptdefault,
    cancel = automdl$automdl.cancel,
    # ub1 = automdl$automdl.ub1,
    # ub2 = automdl$automdl.ub2,
    reducecv = automdl$automdl.reducecv,
    ljungboxlimit = automdl$automdl.ljungboxlimit,
    tsig = automdl$automdl.armalimit,
    ubfinal = automdl$automdl.ubfinal,
    checkmu = NA,
    mixed = null2NA(automdl$automdl.mixed),
    balanced = null2NA(automdl$automdl.balanced), #X
    amicompare = null2NA(automdl$compare)
  ) #OK
  if (!automdl$enabled) {
    new_spec <- rjd3toolkit::set_arima(
      new_spec,
      mean = ifelse(automdl$arima.mu, NA, 0),
      p = automdl$arima.p,
      d = automdl$arima.d,
      q = automdl$arima.q,
      bp = automdl$arima.bp,
      bd = automdl$arima.bd,
      bq = automdl$arima.bq,
      coef = arimacoef$Value,
      coef.type = arimacoef$Type
    ) #OK
  }


  new_spec <- rjd3toolkit::set_basic(
    new_spec,
    preliminary.check = estimate$preliminary.check
  ) #OK

  new_spec <- rjd3toolkit::set_tradingdays(
    new_spec,
    option = td$option,
    autoadjust = null2NA(td$autoadjust),
    leapyear = td$leapYear,
    stocktd = td$stocktd,
    test = td$test,
    automatic = null2NA(td$automatic),
    pftd = null2NA(td$pftd)
  )#OK
  if (udef.spec$variables){
    if (any(udef.var$description$type == "Calendar")){
      def_cal <- udef.var$description[udef.var$description$type == "Calendar",]
      # user-def calendar
      nom_var_td <- rownames(def_cal)
      new_spec <- rjd3toolkit::set_tradingdays(
        new_spec,
        uservariable = paste("r.", nom_var_td)
      )
      if (!all(is.na(def_cal$coeff))){
        new_spec <- rjd3toolkit::set_tradingdays(
          new_spec,
          coef = def_cal$coeff,
          coef.type = "fixed"
        )
      }
    }
    if (any(udef.var$description$type != "Calendar")){
      # user-def regressors
      o_def_var <- udef.var$description[udef.var$description$type != "Calendar",]
      new_spec <- rjd3toolkit::add_usrdefvar(
        new_spec,
        name = rownames(o_def_var),
        coef = coeff_na(o_def_var$coeff),
        regeffect = o_def_var$type
      )

    }

  }


  new_spec <- rjd3toolkit::set_easter(
    new_spec,
    enabled = null2NA(easter$enabled),
    julian = easter$julian,
    duration = easter$duration,
    test = easter$test,
    type = null2NA(easter$type)
  )

  new_spec <- rjd3toolkit::set_estimate(
    new_spec,
    type = span["estimate","type"],
    d0 = span_d(span["estimate", "d0"]),
    d1 = span_d(span["estimate", "d1"]),
    n0 = span["estimate", "n0"],
    n1 = span["estimate", "n1"],
    tol = estimate$tolerance,
    exact.ml = null2NA(estimate$exact_ml),
    unit.root.limit = null2NA(estimate$urfinal)
  )
  new_spec <- rjd3toolkit::set_outlier(
    new_spec,
    span.type = span["outlier","type"],
    d0 = span_d(span["outlier", "d0"]),
    d1 = span_d(span["outlier", "d1"]),
    n0 = span["outlier", "n0"],
    n1 = span["outlier", "n1"],
    critical.value = outliers$cv,
    outliers.type = c("AO", "TC", "LS", "SO")[unlist(outliers[,c("ao", "tc", "ls", "so")])],
    tc.rate = outliers$tcrate,
    method = null2NA(outliers$method),
    eml.est = null2NA(outliers$eml)
  ) #OK
  if (!outliers$enabled) {
    new_spec <- rjd3toolkit::set_outlier(
      new_spec,
      outliers.type = NULL
    )
  }

  new_spec <- rjd3toolkit::set_transform(
    new_spec,
    fun = transform$tfunction,
    adjust = null2NA(transform$adjust),
    aicdiff = null2NA(transform$aicdiff)
  )#OK

  if (!all(is.na(udef.out))){
    # User-defined outliers
    new_spec <- rjd3toolkit::add_outlier(
      new_spec,
      type = udef.out$type,
      date = udef.out$date,
      coef = coeff_na(udef.out$coef)
    )
  }

  if (all(is.na(udef.var$series))){
    context <- rjd3toolkit::modelling_context()
  } else {
    context <- rjd3toolkit::modelling_context(
      variables = list(r = udef.var$series)
    )
  }
  return(list(context = context, result = new_spec))
}
#' @noRd
#' @keywords internal
#' @export
as_rjd3.X11_spec <- function(x, new_spec= NULL, ...){
  if (is.null(new_spec)){
    new_spec <- rjd3x13::x11_spec()
  }
  decomposition <- get_final(x)
  new_spec <- rjd3x13::set_x11(
    new_spec,
    mode = decomposition$x11.mode,
    seasonal.comp = decomposition$x11.seasonalComp,
    seasonal.filter = decomposition$x11.seasonalma,
    henderson.filter = ifelse(
      decomposition$x11.trendAuto,
      0,
      decomposition$x11.trendma
    ),
    lsigma = decomposition$x11.lsigma,
    usigma = decomposition$x11.usigma,
    fcasts = decomposition$x11.fcasts,
    bcasts = decomposition$x11.bcasts,
    calendar.sigma = decomposition$x11.calendarSigma,
    sigma.vector = decomposition$x11.sigmaVector,
    exclude.forecast = decomposition$x11.excludeForecast
  )
  new_spec
}
#' @noRd
#' @keywords internal
#' @export
as_rjd3.seats_spec <- function(x, new_spec= NULL, ...){
  if (is.null(new_spec)){
    new_spec <- rjd3tramoseats::spec_tramoseats()
  }
  decomposition <- get_final(x)
  new_spec <- rjd3tramoseats::set_seats(
    new_spec,
    approximation = decomposition$seats.approx,
    trend.boundary = decomposition$seats.trendBoundary,
    seas.boundary = decomposition$seats.seasdBoundary,
    seas.boundary.unique = decomposition$seats.seasdBoundary1,
    seas.tolerance = decomposition$seats.seasTol,
    ma.boundary = decomposition$seats.maBoundary,
    fcasts = decomposition$seats.predictionLength,
    bcasts = NA,
    algorithm = decomposition$seats.method,
    bias = NA
  )
  new_spec
}
#' @noRd
#' @keywords internal
#' @export
as_rjd3.benchmarking_spec <- function(x, new_spec, ...){
  benchmark <- get_final(x)
  new_spec <- rjd3toolkit::set_benchmarking(
    new_spec,
    enabled = benchmark$benchmarking.enabled,
    target = benchmark$benchmarking.target,
    rho = benchmark$benchmarking.rho,
    lambda = benchmark$benchmarking.lambda,
    forecast = benchmark$benchmarking.useforecast
  )
  new_spec
}

# as_rjd3_preprocessing_spec <- function(x, new_spec = NULL){
#   if (is.null(new_spec)) {
#     if (inherits(x, "X13")) {
#       new_spec <- rjd3x13::regarima_spec()
#     } else {
#       new_spec <- rjd3tramoseats::tramo_spec()
#     }
#   }
#   regarima <- x
#   span <- regarima$span #OK
#   estimate <- get_final(regarima$estimate) #OK
#   outliers <- get_final(regarima$outliers)#OK
#   automdl <- get_final(regarima$arima$specification) #OK
#   arimacoef <- get_final(regarima$arima["coefficients"]) #OK
#   if (all (is.na(arimacoef))){
#     arimacoef <- list(Value = NA, coef.type = NA)
#   }
#
#   forecast <- get_final(regarima$forecast)
#   udef.out <- get_final(regarima$regression$userdef["outliers"])
#   udef.var <- get_final(regarima$regression$userdef["variables"])
#   udef.spec <- get_final(regarima$regression$userdef$specification)
#
#   td <- get_final(regarima$regression$trading.days)#OK
#   easter <- get_final(regarima$regression$easter) #OK
#   transform <- get_final(regarima$transform) #OK
#
#
#   new_spec <- rjd3toolkit::set_automodel(
#     new_spec,
#     enabled = automdl$enabled,
#     acceptdefault = automdl$automdl.acceptdefault,
#     cancel = automdl$automdl.cancel,
#     # ub1 = automdl$automdl.ub1,
#     # ub2 = automdl$automdl.ub2,
#     reducecv = automdl$automdl.reducecv,
#     ljungboxlimit = automdl$automdl.ljungboxlimit,
#     tsig = automdl$automdl.armalimit,
#     ubfinal = automdl$automdl.ubfinal,
#     checkmu = NA,
#     mixed = null2NA(automdl$automdl.mixed),
#     balanced = null2NA(automdl$automdl.balanced), #X
#     amicompare = null2NA(automdl$compare)
#   ) #OK
#   if (!automdl$enabled) {
#     new_spec <- rjd3toolkit::set_arima(
#       new_spec,
#       mean = ifelse(automdl$arima.mu, NA, 0),
#       p = automdl$arima.p,
#       d = automdl$arima.d,
#       q = automdl$arima.q,
#       bp = automdl$arima.bp,
#       bd = automdl$arima.bd,
#       bq = automdl$arima.bq,
#       coef = arimacoef$Value,
#       coef.type = arimacoef$Type
#     ) #OK
#   }
#
#
#   new_spec <- rjd3toolkit::set_basic(
#     new_spec,
#     preliminary.check = estimate$preliminary.check
#   ) #OK
#
#   new_spec <- rjd3toolkit::set_tradingdays(
#     new_spec,
#     option = td$option,
#     autoadjust = null2NA(td$autoadjust),
#     leapyear = td$leapYear,
#     stocktd = td$stocktd,
#     test = td$test,
#     automatic = null2NA(td$automatic),
#     pftd = null2NA(td$pftd)
#   )#OK
#   if (udef.spec$variables){
#     if (any(udef.var$description$type == "Calendar")){
#       def_cal <- udef.var$description[udef.var$description$type == "Calendar",]
#       # user-def calendar
#       nom_var_td <- rownames(def_cal)
#       new_spec <- rjd3toolkit::set_tradingdays(
#         new_spec,
#         uservariable = paste("r.", nom_var_td)
#       )
#       if (!all(is.na(def_cal$coeff))){
#         new_spec <- rjd3toolkit::set_tradingdays(
#           new_spec,
#           coef = def_cal$coeff,
#           coef.type = "fixed"
#         )
#       }
#     }
#     if (any(udef.var$description$type != "Calendar")){
#       # user-def regressors
#       o_def_var <- udef.var$description[udef.var$description$type != "Calendar",]
#       new_spec <- rjd3toolkit::add_usrdefvar(
#         new_spec,
#         name = rownames(o_def_var),
#         coef = coeff_na(o_def_var$coeff),
#         regeffect = o_def_var$type
#       )
#
#     }
#
#   }
#
#   new_spec <- rjd3toolkit::set_easter(
#     new_spec,
#     enabled = null2NA(easter$enabled),
#     julian = easter$julian,
#     duration = easter$duration,
#     test = easter$test,
#     type = null2NA(easter$type)
#   )
#
#   new_spec <- rjd3toolkit::set_estimate(
#     new_spec,
#     type = span["estimate","type"],
#     d0 = span_d(span["estimate", "d0"]),
#     d1 = span_d(span["estimate", "d1"]),
#     n0 = span["estimate", "n0"],
#     n1 = span["estimate", "n1"],
#     tol = estimate$tolerance,
#     exact.ml = null2NA(estimate$exact_ml),
#     unit.root.limit = null2NA(estimate$urfinal)
#   )
#   new_spec <- rjd3toolkit::set_outlier(
#     new_spec,
#     span.type = span["outlier","type"],
#     d0 = span_d(span["outlier", "d0"]),
#     d1 = span_d(span["outlier", "d1"]),
#     n0 = span["outlier", "n0"],
#     n1 = span["outlier", "n1"],
#     critical.value = outliers$cv,
#     outliers.type = c("AO", "TC", "LS", "SO")[unlist(outliers[,c("ao", "tc", "ls", "so")])],
#     tc.rate = outliers$tcrate,
#     method = null2NA(outliers$method),
#     eml.est = null2NA(outliers$eml)
#   ) #OK
#   if (!outliers$enabled) {
#     new_spec <- rjd3toolkit::set_outlier(
#       new_spec,
#       outliers.type = NULL,
#     )
#   }
#
#   new_spec <- rjd3toolkit::set_transform(
#     new_spec,
#     fun = transform$tfunction,
#     adjust = transform$adjust,
#     aicdiff = null2NA(transform$aicdiff),
#   )#OK
#
#   if (!all(is.na(udef.out))){
#     # User-defined outliers
#     new_spec <- rjd3toolkit::add_outlier(
#       new_spec,
#       type = udef.out$type,
#       date = udef.out$date,
#       coef = coeff_na(udef.out$coef)
#     )
#   }
#   if (all(is.na(udef.var$series))){
#     context <- rjd3toolkit::modelling_context()
#   } else {
#     context <- rjd3toolkit::modelling_context(
#       variables = list(r = udef.var$series)
#     )
#   }
#   return(list(context = context, spec = new_spec))
# }

