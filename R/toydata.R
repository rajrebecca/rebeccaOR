


#' Title Odds Ratios and 95% Confidence Intervals
#'
#' @param coef beta coefficient
#' @param se standard error
#' @param siglevel significance level
#' @param roundto round to 2 decimal places
#'
#' @return
#' @export
#'
#' @examples
OR_95CI <- function(coef, se, siglevel = 0.05, roundto = 2) {
  q <- 1 - siglevel / 2
  OR <- exp(coef)
  ORlcl <- exp(coef - qnorm(q) * se)
  ORucl <- exp(coef + qnorm(q) * se)
  ORresult <- paste0(format(round(OR, roundto), nsmall = roundto),
                     " (",
                     format(round(ORlcl, roundto), nsmall = roundto),
                     ", ",
                     format(round(ORucl, roundto), nsmall = roundto),
                     ")")
  return(ORresult)
}

