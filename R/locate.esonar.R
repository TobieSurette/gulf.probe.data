#' Locate Star Oddi Probe Data
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
#' locate.esonar(2020) # Find snow crab survey eSonar data from 2020.
#' locate.esonar(2020, tow.id = "GP354F")

#' @export locate.esonar
locate.esonar <- function(x, ...) UseMethod("locate.esonar")

#' @describeIn locate.esonar Default method for locating eSonar data files.
#' @rawNamespace S3method(locate.esonar,default)
locate.esonar.default <- function(x, ...) return(locate.probe(x, probe = "esonar", remove = c("test", "use raw"), ...))

#' @describeIn locate.esonar \code{scsset} method for locating eSonar data files.
#' @rawNamespace S3method(locate.esonar,scsset)
locate.esonar.scsset <- function(x, ...) return(locate.esonar(year = unique(year(x)), tow.id = unique(x$tow.id, ...)))

