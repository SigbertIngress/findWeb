% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/icpdist.R
\name{icpdist}
\alias{icpdist}
\title{icpdist}
\usage{
icpdist(par, query, data, full = FALSE)
}
\arguments{
\item{par}{numeric(6): the transformation parameters}

\item{query}{matrix: positions of 2D points to match in \code{data}}

\item{data}{matrix: positions of 2D points}

\item{full}{logical: return only summarized distances of full information (default: \code{FALSE})}
}
\value{
summarized distances or detailled list
}
\description{
Computes the point distance between to set of 2D points \code{query} and \code{data}. 
Each point in \code{query}, transformed with \code{par}, is linked to a (different) point \code{data}.
if \code{full=FALSE} then only the summarized distances between the transformed and the matched 
points are returned. Otherwise for each point in \code{query} a list is returned with transformed
points \code{xy}, the index of the "nearest" point in data and the distance.
}
\examples{
n     <- 10
data  <- cbind(runif(n),runif(n))
query <- cbind(runif(3), runif(3))
icpdist(par=c(1,0,0,1,0,0), data=data, query=query)
icpdist(par=c(1,0,0,1,0,0), data=data, query=query, full=TRUE)
}
