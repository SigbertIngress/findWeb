#' plot.web
#'
#' Plots a web. Use \code{...} further plotting, parameters see \code{\link[graphics]{plot}}. Note that \code{x}, \code{y} and \code{col} will be overwritten.
#' 
#' Blue lines represent intersection free links, whereas red lines represent links which have interscetions.
#' Blue numbers represent the portal numbers of the target link structure, 
#' black numbers the portal number of the given portal set. 
#'
#' @param x a web
#' @param black logical: display the black numbers (default: TRUE)
#' @param blue logical: display the blue numbers (default: TRUE)
#' @param ... further parameters for the plot (defaults: \code{pch=19}, \code{asp=TRUE}, \code{xlab='x'}, \code{ylab='y'}). 
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
#' plot(gf, xlim=c(0.5,1), ylim=c(0,1))
plot.web <- function(x, blue=TRUE, black=TRUE, ...) {
  col  <- c("blue", "black")
  colv <- col[1+is.na(x$map)]
  args <- list(...)
  if (is.null(args$pch)) args$pch <- 19
  if (is.null(args$asp)) args$asp <- TRUE
  if (is.null(args$xlab)) args$xlab <- 'x'
  if (is.null(args$ylab)) args$ylab <- 'y'  
  args$x   <- x$vertices[,1]
  args$y   <- x$vertices[,2]
  args$col <- colv
  do.call(plot, args)
  colline <- c("blue", "red")
  for (i in 1:nrow(x$edges)) {
    f <- which(x$edges[i,1]==x$map)
    t <- which(x$edges[i,2]==x$map)
    lines(x$vertices[c(f,t),1], x$vertices[c(f,t),2], col=colline[1+(x$error$Intersection[i]>0)])
  }
  ind <- (colv=="blue")
  if (blue) text(x$vertices[ind,], labels=x$map[ind], pos=1, col=col[1])
  if (black) text(x$vertices, pos=3, col=col[2])
  title(main=sprintf("error = %.0f", evaluate(x)))
  if (!is.null(x$plan)) {
    plan <- by(x$plan[,-1], x$plan[,1], function(x){x}, simplify=FALSE)
    tf <- !is.na(x$map)
    gv <- x$vertices[tf,]
    gv <- gv[order(x$map[tf]),]
    for (i in 1:length(plan)) {
      lines(gv[plan[[i]][,1],], col="#FF00FF", lwd=2)
      args$x   <- gv[plan[[i]][1,1],1]
      args$y   <- gv[plan[[i]][1,1],2]
      args$col <- "#FF00FF"
      do.call(points, args)
    }
  }
}