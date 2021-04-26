#' @title Find Minilog Tow ID
#' 
#' @description Find the tow identification tag associated with a Minilog object.
#' 
#' @param method Method used to determine the tow identification tag. Available options are \sQuote{observed}, \sQuote{file name},
#'               \sQuote{header}, and \sQuote{time}. 
#'               
#' @examples 
#' files <- locate.minilog(2018)
#' tow.id(read.minilog(files[1]))             
#' 

#' @rawNamespace S3method(tow.id,minilog)
tow.id.minilog <- function(x, method = "observed"){
   # method = "time"
   
   # Parse 'method' argument:
   if (missing(method)) method <- "observed"
   method <- match.arg(tolower(method), c("observed", "file name", "header", "time")) 
   
   if (method %in% c("observed", "header")){
      ix <- grep("study.id", tolower(names(x)))
      if (length(ix) > 0){
          v <- x[, ix]
          v <- gsub("[(].*[)]", "", v)
          v <- gulf.utils::deblank(toupper(v))
      
          # Tow ID fixes:
          v[which(v == "ZONE - F  385-S")] <- "GP385S"
      }else{
          v <- header(x)
          ix <- grep("study.id", tolower(names(v)))
          if (length(ix) == 0) ix <- grep("study.description", tolower(names(v)))
          if (length(ix) == 0) return("")
          v <- v[[ix]]
          v <- gsub("[(].*[)]", "", v)
          v <- gulf.utils::deblank(toupper(v))          
          v[which(v == "ZONE - F  385-S")] <- "GP385S"
      }
   }
   
   # Determine tow ID from file name:
   if (method == "file.name"){
      
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
