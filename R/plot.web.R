#' plot.web
#'
#' @param x a web
#' @param ... further parameters (unused)
#'
#' @export
#' @importFrom graphics identify lines plot text title
#' @importFrom utils str
#'
#' @examples
#' xy <- cbind(runif(30), runif(30))
#' g  <- fishbone(8)
#' plot(g)
#' gf <- web(g, xy)
#' plot(gf)
plot.web <- function(x, ...) {
  col  <- c("blue", "black")
  colv <- col[1+is.na(x$map)]
  plot(x$vertices, pch=19, col=colv, asp=TRUE)
  for (i in 1:nrow(x$edges)) {
    f <- which(x$edges[i,1]==x$map)
    t <- which(x$edges[i,2]==x$map)
    lines(x$vertices[c(f,t),1], x$vertices[c(f,t),2], col="blue")
  }
  ind <- (colv=="blue")
  text(x$vertices[ind,], labels=x$map[ind], pos=1, col=col[1])
  text(x$vertices, pos=3, col=col[2])
  title(main=sprintf("error = %.0f", evaluate(x)))
}