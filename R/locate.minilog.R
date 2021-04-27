#' @title Locate Minilog Probe Data
#' 
#' @description Functions to locate Minilog probe data from different projects and surveys.
#' 
#' @param x Data object.
#' @param path Data search path.
#' @param year Study or survey year.
#' @param project Study project identifier. See \code{\link[gulf.metadata]{project}}.
#' @param remove Character string vector specifying a list of key words, which if found in the data path or file name, 
#'               are removed from the search results.
#' @param tow.id Character string(s) specifying a snow crab survey tow identifier(s) (e.g. \sQuote{GP354F}).
#' 
#' @examples 
#' # Global searches:
#' locate.minilog()     # Find all available Minilog data files.
#' locate.minilog(1997) # Find Minilog data files for the 1997 snow crab survey.
#' locate.minilog(1997:1999) # Find Minilog data files for the 1997-1997 snow crab survey.

#' @export locate.minilog
locate.minilog <- function(x, ...) UseMethod("locate.minilog")

#' @describeIn locate.minilog Default method for locating Minilog probe data files.
#' @rawNamespace S3method(locate.minilog,default)
locate.minilog.default <- function(x, ...) return(locate.probe(x, probe = "minilog", remove = c("reject", "test", "invalid", "DS_Store"), ...))

#' @describeIn locate.minilog Locate Minilog associated with snow crab survey tow data.
#' @rawNamespace S3method(locate.minilog,scsset)
locate.minilog.scsset <- function(x, ...) return(locate.minilog(year = gulf.utils::year(x), tow.id = gulf.data::tow.id(x), ...))

