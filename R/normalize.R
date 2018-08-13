#' normalie
#'
#' Sorts first in each row of a matrix the entries by value and secondly the rows by first, second etc. entry
#'
#' @param mat a numeric input matrix
#'
#' @return a sorted matrix
#' @export
#'
#' @examples
#' x <- matrix(round(5*runif(15)), ncol=3)
#' normalize(x)
normalize <- function(mat) {
  df <- as.data.frame(t(apply(mat, 1, function(x) { sort(x) })))
  as.matrix(df[do.call(order, df),])
}