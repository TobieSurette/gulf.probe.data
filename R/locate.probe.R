#' @title Locate Probe Data
#' 
#' @description Locate data files for various types of probes.
#' 
#' @param x Study year, data file name or tow identification tag.
#' @param probe Probe name. See \code{\link[gulf.data]{probe}} for available options.
#' @param project Project name. See \code{\link[gulf.data]{project}} for available options.
#' @param location Probe location. For trawl probes, options are \code{headline} and \code{footrope}.
#' @param remove Character string specifying terms to remove from search results.
#' @param year Study year.
#' @param tow.id Tow identification tag for trawl studies and surveys.
#' 
#' @examples 
#' locate.probe(2020, "esonar", project = "scs")                             # 2020 Snow crab survey eSonar data files.
#' locate.probe(2018, "minilog", project = "scs")                            # 2018 snow crab survey Minilog data files.
#' locate.probe(2018, "star.oddi", project = "scs", location = "headline")   # 2018 snow crab survey Star Oddi data files.
#' locate.probe(probe = "scanmar", project = "scs")                          # All snow crab survey Scanmar files.
#'
#' @seealso code{\link[gulf.data]{project}}, code{\link[gulf.data]{probe}}

locate.probe <- function(x, probe, project = "scs", location = "headline", remove, year, tow.id, ...){
   # Parse 'x' argument:
   if (!missing(x)){
      if (is.numeric(x)) year <- x
      if (is.character(x)){
         if (any(file.exists(x))) return(x[file.exists(x)])
         tow.id <- x
      }
   }
   
   #' Parse arguments:
   probe   <- probe(probe)
   project <- project(project)
   location <- match.arg(tolower(location), c("headline", "footrope", "tilt"))
   
   # Minilog 
   if (probe == "minilog"){
      if (project == "scs"){
         files <- locate(package = "gulf.trawl.data", keywords = c("minilog"), ...)
         files <- files[union(grep("asc", tolower(files)), grep(".csv$", tolower(files)))]    
      }
   }
   
   # Star Oddi:
   if (probe == "star.oddi"){
      remove  = c("test", "lost", "NA")
       
      # Parse 'location' argument:
      location <- tolower(gsub("[. ]", "", location))
      location <- location[location %in% c("headline", "footrope", "tilt")]
   
      # Locate candidate files:
      files <- NULL
      for (i in 1:length(location)){
         files <- c(files, locate(package = "gulf.trawl.data", file = "*.DAT", keywords = c(project, "star", "oddi", location[i]), ...))
      }
   }
   
   # Scanmar
   if (probe == "scanmar"){
      if (project == "scs"){
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
      }
   }
   
   # Netmind
   if (probe == "netmind"){
      if (project == "scs"){
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
      }
   }
   
   # eSonar:
   if (probe == "esonar"){
      if (project == "scs"){  
         # Load set of file names:
         files <- locate(package = "gulf.trawl.data", file = "*.csv", keywords = "esonar", ...)
      }
   }
   
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
      year <- sort(unique(year))
      ix <- NULL
      for (i in 1:length(year)) ix <- c(ix, grep(year[i], files))
      files <- unique(files[ix])
   }
   
   # Target tow ID:
   if (!missing(tow.id)){
      if (!is.null(tow.id)){
         if (project == "scs"){
            h <- read.minilog.header(files)
            y <- rep("", nrow(h)) # Result variable.
      
            # Parse study ID:
            ix <- grep("study.id", tolower(names(h)))
            if (length(ix) > 0){
               iv <- (y == "") & (h[, ix] != "") & !is.na(h[, ix])
               y[iv] <- h[iv, ix]
            }
      
            # Parse study description:
            ix <- grep("study.description", tolower(names(h)))
            if (length(ix) > 0){
               iv <- (y == "") & (h[, ix] != "") & !is.na(h[, ix])
               y[iv] <- h[iv, ix]
            }
      
            # Formatting adjustments:
            y <- gsub("[(].*[)]", "", y)
            y <- gulf.utils::deblank(toupper(y))
      
            # Spot corrections:
            y[which(y == "ZONE - F  385-S")] <- "GP385S"
            h <- y
            files <- files[toupper(h) %in% toupper(tow.id)]
            
            tow.id <- NULL
         }
      }
   }
   
   # Only keep unique file names:
   files <- unique(files)
   
   return(files)
}
