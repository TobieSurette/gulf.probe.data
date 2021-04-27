#' Locate Scanmar Trawl Acoustic Data
#' 
#' @description Functions to locate Scanmar probe data from different projects and surveys.
#' 
#' @param x Data object.
#' @param year Study or survey year.
#' @param remove Character string vector specifying a list of key words, which if found in the data path or file name, 
#'               are removed from the search results.
#' @param tow.id Character string(s) specifying a snow crab survey tow identifier(s) (e.g. \sQuote{GP354F}).
#' 
#' @examples 
#' # Global searches:
#' locate.scanmar()     # Find all available Scanmar data files.
#' locate.scanmar(1990) # Find Scanmar data files for the 1990 snow crab survey.
#' locate.scanmar(1990:1994) # Find Scanmar data files for the 1990-1994 snow crab survey.
#' 
#' # Specific searches:
#' locate.scanmar(1990, tow.id = 100)
#' locate.scanmar(1990, tow.id = "100")
#' locate.scanmar(1990, tow.id = "S90100")
#' locate.scanmar(tow.id = "S90100")

#' @export locate.scanmar
locate.scanmar <- function(x, ...) UseMethod("locate.scanmar")

#' @describeIn locate.scanmar Default method for locating Scanmar acoustic data files.
#' @rawNamespace S3method(locate.scanmar,default)
locate.scanmar.default <- function(x, ...) return(locate.probe(x, probe = "scanmar", remove = c("reject", "test", "invalid"), ...))
