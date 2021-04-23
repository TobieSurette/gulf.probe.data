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
#' locate.star.oddi(2020, probe = "headline", source = 'ascii') 
#' locate.star.oddi(2020, probe = "footrope", source = 'ascii', tow.id = "GP354F")

#' @describeIn locate.probe Generic method for locating Star Oddi data files.
#' @export locate.star.oddi
locate.star.oddi <- function(x, ...) UseMethod("locate.star.oddi")

#' @describeIn locate.probe Default method for locating Star Oddi data files.
#' @rawNamespace S3method(locate.star.oddi,default)
locate.star.oddi.default <- function(x, year, tow.id, full.names = TRUE, probe, remove = c("test", "lost", "NA"), ...){
   # Parse 'x' argument:
   if (!missing(x)){
      if (is.numeric(x)) year <- x
      if (is.character(x)){
         if (any(file.exists(x))) return(x[file.exists(x)])
         tow.id <- x
      }
      if (is.data.frame(x)) if (("tow.id" %in% names(x)) & missing(tow.id)) tow.id <- x$tow.id
      if (is.data.frame(x)) if (("year" %in% names(x)) & missing(year)) year <- sort(unique(x$year))
   }
   
   # Parse 'probe' argument:
   if (missing(probe)) probe <- c("headline", "footrope")
   probe <- tolower(probe)
   if (!all(probe %in% c("headline", "tilt", "footrope"))) 
      stop("'probe' must be either 'headline', 'tilt', 'footrope'.")
   if ("tilt" %in% probe) probe <- unique(c("footrope", probe))
   if ("footrope" %in% probe) probe <- unique(c("tilt", probe))
   
   # Load set of file names:
   files <- NULL
   for (i in 1:length(probe)){
      files <- unique(c(files, locate(pattern = "*.DAT", keywords = c("star oddi", probe[i]), ...)))
   }
   
   # Search Shared drive:
   if (length(files) == 0){
      path <- paste0(options()$gulf.path$snow.crab, "/Offshore Crab Common/Fishing Year ", year, "/Trawl Data/South Western Gulf/Star Oddi")
      if (!missing(location)) path <- paste0(path, "/", location)
      if (file.exists(options()$gulf.path$snow.crab)) files <- locate(pattern = "*.DAT", path = path)
      
      # Remove redundant files:
      fn <- unlist(lapply(strsplit(files, "/"), function(x) x[length(x)]))
      files <- files[setdiff(1:length(files), grep("^[0-9]-", fn))]
   }

   # Target year:
   if (!missing(year)){
      if (!is.numeric(year)) stop("'year' must be a numeric integer.")
      year <- sort(year)
      index <- NULL
      for (i in 1:length(year)) index <- c(index, grep(year[i], files))
   }

   # Target tow ID:
   if (!missing(tow.id)){
      tow.id <- as.character(tow.id)
      index <- NULL
      for (i in 1:length(tow.id)) index <- c(index, grep(tolower(tow.id[i]), tolower(files)))
      files <- unique(files[index])
   }

   # Remove path:
   if (!full.names) files <- unlist(lapply(strsplit(files, "/", fixed = TRUE), function(x) x[length(x)]))

   # Remove files:
   if (!missing(remove)) if (length(remove) == 1) if (remove == FALSE) remove <- NULL
   if (!missing(remove)) remove <- remove[remove != "" & !is.na(remove)]
   if ((length(files) > 0) & (length(remove) > 0)) {
      index <- NULL
      for (i in 1:length(remove)) index <- c(index, grep(tolower(remove[i]), tolower(files)))
      if (length(index) > 0) files <- files[-index]
   }

   # Only keep unique file names:
   files <- unique(files)

   return(files)
}

#' @describeIn locate.probe \code{scsset} method for locating Star Oddi data files.
#' @rawNamespace S3method(locate.star.oddi,scsset)
locate.star.oddi.scsset <- function(x, ...) return(locate.star.oddi(year = as.numeric(substr(gulf.utils::date(x), 1, 4)), tow.id = x$tow.id, ...))
