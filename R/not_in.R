#' \%not in\%
#'
#' This is the inverse of \%in\%
#' It is a convenience function for use in this package
#'
#' @export
#' @param x a vector to compare
#' @param table list of elements we want to exclude from x

`%not in%` <- function (x, table){

 is.na(match(x, table, nomatch=NA_integer_))

}
