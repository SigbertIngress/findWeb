#' Leisepark
#'
#' The dataset contains the position (longitude, latitude) of 43 portals in the Leisepark in Berlin, Germany (\href{https://www.ingress.com/intel?ll=52.529436,13.422475&z=17}{Intel map}). 
#'
#' @docType data
#'
#' @usage data(leisepark)
#' 
#' @examples
#' data(leisepark)
#' head(leisepark)
#' plot(leisepark$lat, leisepark$lon)
#' text(leisepark$lat, leisepark$lon, labels=leisepark$shortname)
"leisepark"