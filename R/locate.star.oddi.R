#' @title Locate Star Oddi Probe Data
#' 
#' @description Functions to locate Star Oddi probe data from various projects and surveys.
#' 
#' @param x Data object.
#' @param year Study or survey year.
#' @param remove Character string vector specifying a list of key words, which if found in the data path or file name, 
#'               are removed from the search results.
#' @param tow.id Character string(s) specifying a snow crab survey tow identifier(s) (e.g. \sQuote{GP354F}).
#' @param probe Character string specifying probe type or location. For example \sQuote{headline} specifies the Star Oddi 
#'              probe attached to the trawl headline on snow crab surveys.
#' 
#' @examples 
#' locate.star.oddi(2020, probe = "headline") 
#' locate.star.oddi(2020, probe = "tilt", tow.id = "GP354F")

#' @export locate.star.oddi
locate.star.oddi <- function(x, ...) UseMethod("locate.star.oddi")

#' @describeIn locate.star.oddi Default method for locating Star Oddi probe data files.
#' @rawNamespace S3method(locate.star.oddi,default)
locate.star.oddi.default <- function(x, ...) return(locate.probe(x, probe = "star.oddi", ...))

#' @describeIn locate.star.oddi \code{scsset} method for locating Star Oddi data files.
#' @rawNamespace S3method(locate.star.oddi,scsset)
locate.star.oddi.scsset <- function(x, ...) return(locate.star.oddi(year = as.numeric(substr(gulf.utils::date(x), 1, 4)), tow.id = x$tow.id, ...))
