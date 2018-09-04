## ---- echo=FALSE---------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library("findWeb")

## ------------------------------------------------------------------------
  library("findWeb")
  data(leisepark)
  head(leisepark)

## ------------------------------------------------------------------------
  xy <- ll2xy(leisepark$lon, leisepark$lat)
  head(xy)

## ---- echo = FALSE, fig.width = 7, fig.height = 7------------------------
  g <- fishbone(8)
  plot(g)

## ---- fig.width = 7, fig.height = 7--------------------------------------
  g0 <- web(g, xy)
  plot(g0)

## ---- fig.width = 7, fig.height = 7--------------------------------------
  set.seed(1)
  g1 <- optimizeWeb(g0)

## ------------------------------------------------------------------------
  set.seed(0)
  g11 <- optimizeWeb(g0)

## ----echo=FALSE----------------------------------------------------------
  g2 <- swap(g1, 42, 16)

## ----eval=FALSE----------------------------------------------------------
#    g2 <- swapi(g1)  # swap 42-16
#    plot(g2)

## ---- , fig.width = 7, fig.height = 7, echo=FALSE------------------------
  plot(g2)

## ------------------------------------------------------------------------
  set.seed(0)
  g12 <- optimizeWeb(g0, maxit=1000000) # default: 100000

## ---- fig.width = 7, fig.height = 7--------------------------------------
  g02 <- web(g, xy, rot=32) # default: 16
  plot(g02)

## ------------------------------------------------------------------------
  g2 <- linkPlan1(g1)
  # agent comes from -1.877423 radian or -107 degree or south south west
  plot(g2)

## ----eval=FALSE----------------------------------------------------------
#    getPlan(g2, quiet=TRUE)

## ------------------------------------------------------------------------
  g2 <- linkPlan1(g1, dir=-pi)
  # 2.215663 rad approx. 127 degree, 
  # agent walks comes approxmately from direction north west  
  plot(g2)

## ------------------------------------------------------------------------
 g3 <- linkPlan1(g1)
 plot(g3)
 getPlan(g3, wait=0)
 getPlan(g3, names=leisepark$shortname, wait=0)

## ----echo=FALSE----------------------------------------------------------
set.seed(0)
n <- 17
x <- cbind(rnorm(n), rnorm(n))
i <- which.min(rowSums(x^2))
r <- max(rowSums(x^2))
x <- scale(x, center=x[i,], scale=FALSE)
plot(x, pch=19, asp=TRUE, axes=FALSE, xlab="", ylab="", col=1+((1:n)==i))
box()
a <- atan2(x[,2], x[,1])
qa <- quantile(a, c(0.5-1/3, 0.5, 0.5+1/3))
for(i in 1:3) lines(c(0, r*cos(qa[i])), c(0, r*sin(qa[i])), col="red")

## ------------------------------------------------------------------------
g <- cobweb(3)
plot(g)

## ------------------------------------------------------------------------
g0 <- web(g, xy)
set.seed(1)
g1 <- optimizeWeb(g0)
plot(g1)

## ------------------------------------------------------------------------
g2 <- linkPlanN(g1, agents=3)
plot(g2)

## ------------------------------------------------------------------------
getPlan(g2, wait=1) 

## ------------------------------------------------------------------------
g4 <- linkPlan1(g1, direction=-pi)
getPlan(g4, wait=1) 

## ------------------------------------------------------------------------
plot(g4, blue=FALSE) # do not plot the blue numbers
plot(g4, black=FALSE) # do not plot the black numbers
plot(g4, links=FALSE) # do not plot the links
plot(g4, pathes=FALSE) # do not plot the agent pathes

## ----eval=FALSE----------------------------------------------------------
#  g0 <- fishbone(8)
#  g1 <- web(g0, xy)
#  g2 <- optimizeWeb(g1)
#  g3 <- linkPlan1(g2)
#  plot(g3, blue=FALSE)

## ------------------------------------------------------------------------
library("magrittr") # for %>% operator
fishbone(8) %>% web(xy) %>% optimizeWeb() %>% linkPlan1() %>% plot(blue=FALSE)

## ----eval=FALSE----------------------------------------------------------
#  g <- fishbone(8) %>% web(xy) %>% optimizeWeb() %>% linkPlan1()
#  plot(g, blue=FALSE)

