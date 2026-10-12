#' Convert 'rjd3' Objects to 'RJDemetra' (v2) Format
#'
#' @description
#' Converts seasonal adjustment models, outputs, or specifications from the
#' modern 'rjd3' ecosystem ('JDemetra+' v3) back to legacy 'RJDemetra' ('JDemetra+' v2)
#' objects for backward compatibility and legacy reporting workflows.
#'
#' @param x An object from the 'rjd3' ecosystem (e.g., from packages `rjd3x13`
#' or `rjd3tramoseats`).
#' @param context Optional [rjd3toolkit::modelling_context()]  containing user-defined variables.
#' @param y Input time series data.
#' Needed for `as_rjd2.JD3_REGARIMA_OUTPUT()` and `as_rjd2.JD3_TRAMO_OUTPUT()`.
#' If not provided, the default year will by default be equal to 1.
#' @param specification The specification of the seasonal adjustment model to use for the conversion.
#' Can be `"estimation_spec"` (the default, specification used to estimate the model),
#' or `"result_spec"` (the specification used to produce the final results).
#' @inheritParams as_rjd3
#'
#' @details
#' The parameters `ub1` and `ub2` are not translated since it seems to have a different convention between versions.
#'
#'
#' @return An equivalent object compatible with the 'RJDemetra' package (v2 structure).
#'
#'
#' @importFrom RJDemetra regarima_spec_x13
#' @importFrom RJDemetra regarima_spec_tramoseats
#' @examplesIf rjd3jars::check_java_version(silent = TRUE)
#' spec_v3 <- rjd3x13::x13_spec()
#' spec_v2 <- as_rjd2(spec_v3)
#'
#' # Specify the modelling context with user-defined variables
#' regs_td <- rjd3toolkit::td(
#'   s = y,
#'   groups = c(1, 2, 0, 4, 5, 6, 3),
#'   contrasts = TRUE
#' )
#'
#' variables <- list(r = list(
#'   Monday = regs_td[, 1],
#'   Tuesday = regs_td[, 2],
#'   Wednesday = regs_td[, 3],
#'   Thursday = regs_td[, 4],
#'   Friday = regs_td[, 5],
#'   Saturday = regs_td[, 6]
#' ))
#' context <- rjd3toolkit::modelling_context(variables = variables)
#' spec_v3 <- rjd3tramoseats::tramoseats_spec() |>
#'   rjd3toolkit::set_tradingdays(
#'     option = "UserDefined",
#'     uservariable = c("r.Monday", "r.Tuesday", "r.Wednesday",
#'                      "r.Thursday", "r.Friday", "r.Saturday"),
#'     test = "None"
#'   )
#' spec_v2 <- as_rjd2(spec_v3, context = context)
#' \donttest{
#' # For preprocessing models (RegARIMA or TRAMO),
#' # the input time series `y` should be provided
#' # to avoid defaulting the start date of the time series to year 1
#' y <- rjd3toolkit::ABS$X0.2.09.10.M
#' model_v3 <- rjd3tramoseats::tramo(y)
#' as_rjd2(model_v3)
#' as_rjd2(model_v3, y = y)
#' }
#' @export
as_rjd2 <- function(x, ...) {
  UseMethod("as_rjd2")
}

#' @noRd
#' @keywords internal
#' @exportS3Method
as_rjd2.JD3_SARIMA_ESTIMATION <- function(x, automdl = TRUE, ...) {
  if (automdl){
    return(
      list(
        mu = NA,
        p = NA_integer_,
        d = NA_integer_,
        q = NA_integer_,
        bp = NA_integer_,
        bd = NA_integer_,
        bq = NA_integer_,
        coefEnabled = NA,
        coef = NA,
        coefType = NA
      )
    )
  }
  m <- x
  if (!is.null(m$phi)) {
    p <- dim(m$phi)[2]
  } else {
    p <- 0
  }
  if (!is.null(m$theta)) {
    q <- dim(m$theta)[2]
  } else {
    q <- 0
  }
  if (!is.null(m$bphi)) {
    bp <- dim(m$bphi)[2]
  } else {
    bp <- 0
  }
  if (!is.null(m$btheta)) {
    bq <- dim(m$btheta)[2]
  } else {
    bq <- 0
  }
  names <- NULL
  if (p > 0) {
    names <- c(names, paste0("phi(", 1:p, ")"))
  }
  if (q > 0) {
    names <- c(names, paste0("theta(", 1:q, ")"))
  }
  if (bp > 0) {
    names <- c(names, paste0("bphi(", 1:bp, ")"))
  }
  if (bq > 0) {
    names <- c(names, paste0("btheta(", 1:bq, ")"))
  }
  if (!is.null(names)) {
    all <- t(cbind(m$phi, m$theta, m$bphi, m$btheta))
    fr <- as.data.frame(all, row.names = names)
    for (i in colnames(fr)) {
      fr[, i] <- unlist(fr[, i])
    }
    coef <- fr[,1]
    coefType <- fr[,2]
    coefEnabled <- any(paste0(coef, coefType) != "0UNDEFINED")
    if (!coefEnabled){
      coef <- coefType <- NA
    }
  } else {
    coef <- coefType <- coefEnabled <- NA
  }


  list(
    mu = NA,
    p = p,
    d = m$d,
    q = q,
    bp = bp,
    bd = m$bd,
    bq = bq,
    coefEnabled = coefEnabled,
    coef = coef,
    coefType = coefType
  )
}
#' @noRd
#' @keywords internal
#' @exportS3Method
as_rjd2.JD3_SPAN <- function(x, ...) {
  span <- x
  res <- list(
    from = NA_character_,
    to = NA_character_,
    first = NA_integer_,
    last = NA_integer_,
    exclFirst = NA_integer_,
    exclLast = NA_integer_
  )
  if (span$type == "ALL") {
    res
  } else if (span$type == "FROM") {
    res$from <- span$d0
  } else if (span$type == "TO") {
    res$to <- span$d1
  } else if (span$type == "BETWEEN") {
    res$from <- span$d0
    res$to <- span$d1
  } else if (span$type == "FIRST") {
    res$first <- span$n0
  } else if (span$type == "LAST") {
    res$last <- span$n1
  } else if (span$type == "EXCLUDING") {
    res$exclFirst <- span$n0
    res$exclLast <- span$n1
  }

  res
}

#' @noRd
#' @keywords internal
as_rjd2_ud_outliers <- function(x, ...) {
  ud_outlier <- x
  if (is.null(ud_outlier)) {
    list(
      enabled = NA,
      type = NA,
      date = NA,
      coef = NA
    )
  } else{
    list(
      enabled = TRUE,
      type = sapply(ud_outlier, function(x) x$code),
      date = sapply(ud_outlier, function(x) as.character(x$pos)),
      coef = sapply(ud_outlier, function(x) null2NA(x$coef$value))
    )
  }
}

#' @noRd
#' @keywords internal
as_rjd2_ud_var <- function(x, context = NULL, ...) {
  regression_spec <- x
  ud_variables <- regression_spec$users
  td_spec <- regression_spec$td
  has_td_var <- all.equal(td_spec$holidays, "") && length(td_spec$users) > 0
  has_ud_var <- length(ud_variables) > 0
  if (is.null(context) | ! (has_ud_var | has_td_var)) {
    return(list(
      varEnabled = FALSE,
      var = NA,
      varType = NA,
      varCoef = NA
    ))
  }

  if (has_td_var) {
    varCoef <- td_spec$tdcoefficients
    if (is.null(varCoef)){
      varCoef <- NA
    } else {
      varCoef <- zero2NA(unlist(td_spec$tdcoefficients["value",]))
    }

    table_td_var <- data.frame(
      var = td_spec$users,
      varCoef = varCoef,
      varType = "Calendar"
    )
  } else {
    table_td_var <- NULL
  }

  if (has_ud_var) {
    table_ud_var <- do.call(rbind, lapply(ud_variables, function(x){
      if (is.null(x$coef)){
        varCoef <- NA
      } else {
        varCoef <- x$coef$value
      }
      data.frame(var = x$id, varCoef = varCoef, varType = x$regeffect)
    }))
  } else {
    table_ud_var <- NULL

  }
  table_var <- rbind(
    table_td_var,
    table_ud_var
  )
  var_present <- var_in_context(table_var$var, context)
  if (!any(var_present)){
    return(list(
      varEnabled = FALSE,
      var = NA,
      varType = NA,
      varCoef = NA
    ))
  }
  if (!all(var_present)){
    warning("Some user-defined variables are not present in the context and will be ignored.")
    table_var <- table_var[var_present, ]
  }
  varCoef <- table_var$varCoef
  if (all(is.na(varCoef))){
    varCoef <- NA
  }
  list(
    varEnabled = TRUE,
    var = get_var_in_context(table_var$var, context),
    varType = table_var$varType,
    varCoef = varCoef
  )
}

#' @noRd
#' @keywords internal
var_in_context <- function(var, context = NULL){
  if (is.null(context)){
    return(NULL)
  }
  context_var <- context$variables
  split <- strsplit(var, split = ".", fixed = TRUE)
  sapply(split, function(x){
    !is.null(context_var[[x[1]]][[x[2]]])
  })
}
#' @importFrom stats ts.union ts
#' @noRd
#' @keywords internal
get_var_in_context <- function(var, context = NULL){
  if (is.null(context)){
    return(NULL)
  }
  context_var <- context$variables
  split <- strsplit(var, split = ".", fixed = TRUE)
  all_vars <-  lapply(split, function(x){
    context_var[[x[1]]][[x[2]]]
  })
  all_vars <- all_vars[lengths(all_vars) >0]
  if (length(all_vars) == 0){
    NA
  } else if (length(all_vars) == 1){
    all_vars[[1]]
  } else {
    all_vars <- do.call(ts.union, all_vars)
    colnames(all_vars) <- var
    all_vars
  }
}


