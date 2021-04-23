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
locate.netmind.default <- function(x, year, tow.id, remove = c("reject", "test", "invalid"), ...){
   # Parse 'x' argument:
   if (!missing(x)){
      if (is.numeric(x)) year <- x
      if (is.character(x)){
         if (any(file.exists(x))) return(x[file.exists(x)])
         tow.id <- x
      }
   }
   
   # Load set of file names:
   files <- locate(package = "gulf.trawl.data", keywords = c("netmind"), ...)
   ix <- unique(c(grep("[.]txt", tolower(files)), grep("[.]scd", tolower(files)), grep("[.]csv", tolower(files))))
   files <- files[ix]

   # Valid file names:
   valid <- c("pos", "s[0-9][0-9]", "sta")
   ix <- union(grep("pos", tolower(files)), grep("s[0-9][0-9]", tolower(files)))
   ix <- union(ix, grep("sta", tolower(files)))
   ix <- union(ix, grep(".csv$", tolower(files)))
   ix <- union(ix, grep(".txt$", tolower(files)))
   files <- files[ix]
   
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

