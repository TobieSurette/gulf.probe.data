#' Locate Netmind Trawl Acoustic Data
#' 
#' @description Functions to locate Netmind probe data from different projects and surveys.
#' 
#' @param x Data object.
#' @param year Study or survey year.
#' @param remove Character string vector specifying a list of key words, which if found in the data path or file name, 
#'               are removed from the search results.
#' @param tow.id Character string(s) specifying a snow crab survey tow identifier(s) (e.g. \sQuote{GP354F}).
#' 
#' @examples 
#' # Global searches:
#' locate.netmind()     # Find all available Netmind data files.
#' locate.netmind(1999) # Find Netmind data files for the 1990 snow crab survey.
#' locate.netmind(1999:2004) # Find Netmind data files for the 1990-1994 snow crab survey.
#' 
#' # Specific searches:
#' locate.netmind(1999, tow.id = 100)
#' locate.netmind(1999, tow.id = "100")
#' locate.netmind(1999, tow.id = "S90100")
#' locate.netmind(tow.id = "355")

#' @export locate.netmind
locate.netmind <- function(x, ...) UseMethod("locate.netmind")

#' @describeIn locate.netmind Default method for locating Netmind acoustic data files.
#' @rawNamespace S3method(locate.netmind,default)
locate.netmind.default <- function(x, ...)  return(locate.probe(x, probe = "netmind", remove = c("reject", "test", "invalid"), ...))
