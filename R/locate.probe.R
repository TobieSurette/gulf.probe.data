locate.probe <- function(x, type, project, probe, remove, year, tow.id, ...){
   # Parse 'x' argument:
   if (!missing(x)){
      if (is.numeric(x)) year <- x
      if (is.character(x)){
         if (any(file.exists(x))) return(x[file.exists(x)])
         tow.id <- x
      }
   }
   
   # Minilog  ===========================================================================================
   
   # Locate Minilog files by project:
   project <- project(project)
   if (project == "scs")  files <- locate.minilog.scs(x, remove = c("reject", "test", "invalid", "DS_Store"), ...)
  
    # Locate candidate files:
   files <- locate(package = "gulf.trawl.data", keywords = c("minilog"), ...)
   files <- files[union(grep("asc", tolower(files)), grep(".csv$", tolower(files)))]
   
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
   
   # Star Oddi ===========================================================================================
   
   # Locate Star Oddi files by project:
   project <- project(project)
   
   if (project == "scs")  files <- locate.star.oddi.scs(x, remove  = c("test", "lost", "NA"), ...)
   if (project == "nss")  files <- locate.star.oddi.nss(x, ...)     
   
   
   # Parse 'probe' argument:
   probe <- tolower(gsub("[. ]", "", probe))
   probe <- probe[probe %in% c("headline", "footrope", "tilt")]
   
   # Locate candidate files:
   files <- NULL
   for (i in 1:length(probe)) files <- c(files, locate(package = "gulf.trawl.data", file = "*.DAT", keywords = c("scs", "star", "oddi", probe[i]), ...))

   # Scanmar ============================================================================================================

   # Load set of file names:
   files <- locate(package = "gulf.trawl.data", keywords = c("scanmar"), ...)
   ix <- unique(c(grep("[.]txt", tolower(files)), grep("[.]scd", tolower(files)), grep("[.]csv", tolower(files))))
   files <- files[ix]
   
   # Valid file names:
   valid <- c("pos", "s[0-9][0-9]", "sta")
   ix <- union(grep("pos", tolower(files)), grep("s[0-9][0-9]", tolower(files)))
   ix <- union(ix, grep("sta", tolower(files)))
   ix <- union(ix, grep(".csv$", tolower(files)))
   files <- files[ix]
   
   # Netmind ============================================================================================================

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
   
   # ESONAR ============================================================================================================
  
   # Load set of file names:
   files <- locate(file = "*.csv", keywords = "esonar", ...)
   
   # Post-Filtering ============================================================================================================
   
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
