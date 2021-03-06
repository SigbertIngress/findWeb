% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/linkBack.R
\name{linkBack}
\alias{linkBack}
\title{linkBack}
\usage{
linkBack(gv, edges, faces, pts, f, t)
}
\arguments{
\item{gv}{numerich matrix: position of portals}

\item{edges}{numerich matrix: matrix of links in a link configuration}

\item{faces}{numerich matrix: matrix of fields in a link configuration}

\item{pts}{numerich vector: vector of portals contained in each field}

\item{f}{numeric: index of portal to link from}

\item{t}{numeric vector: index of portals to link to}
}
\value{
a link plan
}
\description{
Links back portal \code{f} to all portals in \code{t} if possible. 
If necessary links are ordered such that multilayers will be generated.
}
\examples{
\dontrun{
"no example yet"
}
}
