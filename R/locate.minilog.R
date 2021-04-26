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
locate.minilog.default <- function(x, project = "scs", remove, ...){
   # Parse 'x' argument:
   if (!missing(x)) if (is.character(x)) if (any(file.exists(x))) return(x[file.exists(x)])

   # Locate Minilog files by project:
   project <- project(project)
   if (project == "scs")  files <- locate.minilog.scs(x, remove = c("reject", "test", "invalid", "DS_Store"), ...)
   if (project == "alsi") files <- locate.minilog.alsi(x, ...)  
   
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

#' @describeIn locate.minilog Locate Minilog associated with snow crab survey data.
#' @export locate.minilog.scs
locate.minilog.scs <- function(x, year, tow.id, remove, ...){
   if (!missing(x)) if (is.numeric(x)) year <- x
      
   # Locate candidate files:
   files <- locate(package = "gulf.trawl.data", keywords = c("minilog"), ...)
   files <- files[union(grep("asc", tolower(files)), grep(".csv$", tolower(files)))]

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
      v <- read.minilog.header(files)
      
      y <- rep("", nrow(v)) # Result variable.

      # Parse study ID:
      ix <- grep("study.id", tolower(names(v)))
      if (length(ix) > 0){
         iv <- (y == "") & (v[, ix] != "") & !is.na(v[, ix])
         y[iv] <- v[iv, ix]
      }

      # Parse study description:
      ix <- grep("study.description", tolower(names(v)))
      if (length(ix) > 0){
         iv <- (y == "") & (v[, ix] != "") & !is.na(v[, ix])
         y[iv] <- v[iv, ix]
      }
      
      # Formatting adjustments:
      y <- gsub("[(].*[)]", "", y)
      y <- gulf.utils::deblank(toupper(y))
      
      # Spot corrections:
      y[which(y == "ZONE - F  385-S")] <- "GP385S"
      v <- y
      files <- files[grep(toupper(tow.id), toupper(v))]
   }
 
   files <- unique(files)
   
   return(files)
}

#' @describeIn locate.minilog Locate Minilog files from the Atlantic Lobster Settlement Index program.
#' @export locate.minilog.alsi
locate.minilog.alsi <- function(x, path, ...){
   if (missing(path)) stop("'path' must be specified.")
   v <- locate(file = ".csv", path = path)
   v <- unique(v)
   
   return(v)
}

#' @describeIn locate.minilog Locate Minilog associated with snow crab survey tow data.
#' @rawNamespace S3method(locate.minilog,scsset)
locate.minilog.scsset <- function(x, ...){
   v <- locate.minilog.scs(year = gulf.utils::year(x), tow.id = gulf.data::tow.id(x), ...)
   v <- unique(v)
   return(v)
}
