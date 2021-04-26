#' @title Find Minilog Tow ID
#' 
#' @description Find the tow identification tag associated with a Minilog object.
#' 
#' 
#' @param method Method used to determine the tow identification tag. Available options are \sQuote{observed}, \sQuote{file name},
#'               \sQuote{header}, and \sQuote{time}. 
#'               
#' @examples 
#' files <- locate.minilog(2018)
#' tow.id(read.minilog(files[200]))             
#' 

#' @rawNamespace S3method(tow.id,minilog)
tow.id.minilog <- function(x, method = "observed"){
   # method = "time"
   
   # Parse 'method' argument:
   if (missing(method)) method <- "observed"
   method <- match.arg(gsub(" +", ".", tolower(method)), c("observed", "file.name", "header", "time")) 
   
   # Extract tow ID from header information:
   if (method %in% c("observed", "header")){
      if (!is.null(gulf.metadata::header(x))) v <- as.data.frame(t(gulf.metadata::header(x))) else v <- x
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
   }
   
   # Determine tow ID from file name:
   if (method == "file.name"){
      ix <- grep("file.name", tolower(names(x)))
      if (length(ix) > 0){
         iy <- grep("GP[0-9]+", toupper(x[, ix]))
         v <- rep("", nrow(x))
         v[iy] <- toupper(x[iy, ix])
         v[iy] <- unlist(lapply(strsplit(v, "GP"), function(x) x[2]))
         v[iy] <- unlist(lapply(strsplit(v, "[.]"), function(x) x[1]))
         v[iy] <- paste0("GP", v[iy])
      }
   }
   
   if (method == "time"){
      # Load scs tow data:
      y <- read.scsset(year = unique(year(x)))
      x <- gulf.utils::expand(x)
      
      # Define grouping variables:
      if ("file.name" %in% names(x)) vars <- c("date", "file.name") else vars <- c("date", names(x)[grep("study", names(x))])
      
      # Find tow ID from survey tows using time match:
      ux <- unique(x[vars])
      v <- rep(NA, nrow(ux))
      for (i in 1:nrow(ux)){
         xx <- x[which(x[, vars[1]] == ux[i, vars[1]] & x[, vars[2]] == ux[i, vars[2]]), ]
         d <- abs(difftime(median(time(xx[xx$depth > 20, ])), time(y), units = "mins"))
         if (min(d) < 60) v[i] <- y$tow.id[which.min(d)]
      }
      
      ix <- match(x[vars], ux)
      v <- v[ix]
   }
   
   return(v)
}
