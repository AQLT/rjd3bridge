
#' @noRd
#' @keywords internal
span_d <- function(x){
  if (is.na(x)){
    NULL
  } else {
    x
  }
}
#' @noRd
#' @keywords internal
coeff_na <- function(x){
  x[is.na(x)] <- 0
  x
}
#' @noRd
#' @keywords internal
zero2NA <- function(x){
  x[x==0] <- NA
  x
}
#' @noRd
#' @keywords internal
null2NA <- function(x){
  if (is.null(x)){
    NA
  } else {
    x
  }
}
#' @noRd
#' @keywords internal
get_final <- function(x){
  if (is.data.frame(x)){
    if (nrow(x) == 1) {
      x
    } else {
      x["Final", ]
    }
  } else if (is.list(x)){
    if (!is.list(x[[1]]) || is.null(x[[1]]$Final)) {
      x[[1]]
    } else {
      x[[1]]$Final
    }
  }
}
#' @noRd
#' @keywords internal
add_class <- function(x, class){
  if (!is.null(class(x))) {
    class(x) <- c(class, class(x))
  }
  x
}

#' @noRd
#' @keywords internal
vect2NA <- function(x){
  if (length(x) == 0){
    return(NA)
  }
  x
}
#' @noRd
#' @keywords internal
switch2 <- function(char, match, default = NA){
  match <- toupper(match)
  match <- gsub("^.*_", "", match)
  if (any(match == "UNKNOWN")) {
    match[match == "UNKNOWN"] <- "UNDEFINED"
  }
  if (any(match == "NO")) {
    match[match == "NO"] <- "NONE"
  }

  names(char) <- toupper(char)
  i_match <- match %in% names(char)
  match[i_match] <- char[match[i_match]]
  match[!i_match] <- default
  match
}
