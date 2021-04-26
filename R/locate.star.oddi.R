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
locate.star.oddi.default <- function(x, project = "scs", remove, ...){
   # Parse 'x' argument:
   if (!missing(x)) if (is.character(x)) if (any(file.exists(x))) return(x[file.exists(x)])

   # Locate Star Oddi files by project:
   project <- project(project)
   if (project == "scs")  files <- locate.star.oddi.scs(x, remove  = c("test", "lost", "NA"), ...)
   if (project == "nss")  files <- locate.star.oddi.nss(x, ...)  
   
   # Remove files:
   if (!missing(remove)){
      if (length(remove) == 1) if (remove == FALSE) remove <- NULL
      if (!missing(remove)){
         remove <- remove[remove != "" & !is.na(remove)]
         if ((length(files) > 0) & (length(remove) > 0)) {
            index <- NULL
            for (i in 1:length(remove)) index <- c(index, grep(tolower(remove[i]), tolower(files)))
            if (length(index) > 0) files <- files[-index]
         }  
      } 
   } 
   
   # Keep only unique file names:
   files <- unique(files)
   
   return(files)
}

#' @describeIn locate.star.oddi Locate Star Oddi data files from the snow crab survey.
#' @export locate.star.oddi.scs
locate.star.oddi.scs <- function(x, year, tow.id, probe = c("headline", "footrope", "tilt"), remove, ...){
   # Parse 'x' and 'year' arguments:
   if (!missing(x)) if (is.numeric(x)) year <- x
   
   # Parse 'probe' argument:
   probe <- tolower(gsub("[. ]", "", probe))
   probe <- probe[probe %in% c("headline", "footrope", "tilt")]

   # Locate candidate files:
   files <- NULL
   for (i in 1:length(probe)) files <- c(files, locate(package = "gulf.trawl.data", file = "*.DAT", keywords = c("scs", "star", "oddi", probe[i]), ...))
   
   # Remove files:
   if (length(remove) == 1) if (remove == FALSE) remove <- NULL
   remove <- remove[remove != "" & !is.na(remove)]
   if ((length(files) > 0) & (length(remove) > 0)){
      ix <- NULL
      for (i in 1:length(remove)) ix <- c(ix, grep(tolower(remove[i]), tolower(files)))
      if (length(ix) > 0) files <- files[-ix]
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

   # Only keep unique file names:
   files <- unique(files)

   return(files)
}

#' @describeIn locate.star.oddi \code{scsset} method for locating Star Oddi data files.
#' @rawNamespace S3method(locate.star.oddi,scsset)
locate.star.oddi.scsset <- function(x, ...) return(locate.star.oddi(year = as.numeric(substr(gulf.utils::date(x), 1, 4)), tow.id = x$tow.id, ...))

#' @describeIn locate.star.oddi Locate Star Oddi data files from the Northumberland Strait survey.
#' @export locate.star.oddi.scs
locate.star.oddi.nss <- function(x, year, remove, ...){
   # Parse 'x' and 'year' arguments:
   if (!missing(x)) if (is.numeric(x)) year <- x

   # Locate candidate files:
   files <- NULL
   for (i in 1:length(probe)) files <- c(files, locate(package = "gulf.trawl.data", file = "*.DAT", keywords = c("nss", "star", "oddi"), ...))
   
   # Remove files:
   if (!missing(remove)){
      if (length(remove) == 1) if (remove == FALSE) remove <- NULL
      remove <- remove[remove != "" & !is.na(remove)]
      if ((length(files) > 0) & (length(remove) > 0)){
         ix <- NULL
         for (i in 1:length(remove)) ix <- c(ix, grep(tolower(remove[i]), tolower(files)))
         if (length(ix) > 0) files <- files[-ix]
      }
   }
   
   # Target year:
   if (!missing(year)){
      if (!is.numeric(year)) stop("'year' must be a numeric integer.")
      year <- sort(year)
      index <- NULL
      for (i in 1:length(year)) index <- c(index, grep(year[i], files))
      files <- unique(files[index])
   }

   # Only keep unique file names:
   files <- unique(files)

   return(files)
}
