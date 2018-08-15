## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ---- echo = FALSE, fig.width = 7, fig.height = 7------------------------
  library("findWeb")
  g <- fishbone(8)
  plot(g)

## ----createportal--------------------------------------------------------
  library("findWeb")
  set.seed(1)
  n  <- 20
  xy <- cbind(runif(n), runif(n))
  xy

## ------------------------------------------------------------------------
  g <- fishbone(6)
  plot(g)

## ---- fig.width = 7, fig.height = 7--------------------------------------
  g0 <- web(g, xy)
  plot(g0)

## ---- fig.width = 7, fig.height = 7--------------------------------------
  g1 <- optimizeWeb(g0)

## ------------------------------------------------------------------------
  g11 <- optimizeWeb(g0)

## ----echo=FALSE----------------------------------------------------------
g2 <- swap(g1, c(18,12), c(4,19))

## ----eval=FALSE----------------------------------------------------------
#    g2 <- swapi(g1)  # swap 18-4, 12-19
#    plot(g2)

## ---- , fig.width = 7, fig.height = 7, echo=FALSE------------------------
  plot(g2)

## ------------------------------------------------------------------------
  library("findWeb")
  data(leisepark)
  head(leisepark)

## ------------------------------------------------------------------------
  xy <- ll2xy(leisepark$lon, leisepark$lat)
  head(xy)

## ---- fig.width = 7, fig.height = 7--------------------------------------
  g <- homogeneous(4)
  plot(g)

## ---- fig.width = 7, fig.height = 7--------------------------------------
  g0 <- web(g, xy)
  plot(g0)

## ---- fig.width = 7, fig.height = 7--------------------------------------
  g1 <- optimizeWeb(g0)

## ----eval=FALSE----------------------------------------------------------
#    g2 <- swapi(g1)

