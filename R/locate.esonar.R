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
locate.esonar.default <- function(x, year, tow.id, remove = c("test", "use raw"), ...){
   # Parse 'x' argument:
   if (!missing(x)){
      if (is.numeric(x)) year <- x
      if (is.character(x)){
         if (any(file.exists(x))) return(x[file.exists(x)])
         tow.id <- x
      }
   }

   # Load set of file names:
   files <- locate(file = "*.csv", keywords = "esonar", ...)
   
   # Target year:
   if (!missing(year)){
      if (!is.numeric(year)) stop("'year' must be a numeric integer.")
      year <- sort(year)
      index <- NULL
      for (i in 1:length(year)) index <- c(index, grep(year[i], files))
      files <- unique(files[index])
   }
   
   # Target year:
   if (!missing(year)){
      if (!is.numeric(year)) stop("'year' must be a numeric integer.")
      year <- sort(year)
      index <- NULL
      for (i in 1:length(year)) index <- c(index, grep(year[i], files))
      files <- unique(files[index])
   }

   # Target tow ID:
   if (!missing(tow.id)){
      tow.id <- as.character(tow.id)
      index <- NULL
      for (i in 1:length(tow.id)) index <- c(index, grep(tolower(tow.id[i]), tolower(files)))
      files <- unique(files[index])
   }

   # Remove files:
   remove <- remove[remove != "" & !is.na(remove)]
   if ((length(files) > 0) & (length(remove) > 0)) {
      ix <- NULL
      for (i in 1:length(remove)) ix <- c(ix, grep(tolower(remove[i]), tolower(files)))
      if (length(ix) > 0) files <- files[-ix]
   } 
   
   # Only keep unique file names:
   files <- unique(files)

   return(files)
}

#' @describeIn locate.esonar \code{scsset} method for locating eSonar data files.
#' @rawNamespace S3method(locate.esonar,scsset)
locate.esonar.scsset <- function(x, ...) return(locate.esonar(year = unique(year(x)), tow.id = unique(x$tow.id, ...)))

