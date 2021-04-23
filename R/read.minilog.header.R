#' Read Minilog Header Information
#' 
#' @description Function that reads header information from Minilog data files.
#' 
#' @param x,file Minilog file names.
#' 
#' @examples:
#' files <- locate.minilog(year = 1999, project = "scs")
#' read.minilog.header(files)
#' 
#' @export read.minilog.header
read.minilog.header <- function(x, file, ...){
   # Define file(s) to be read:
   if (!missing(x) & missing(file)) if (is.character(x)) file = x
   if (missing(file)) file <- locate.esonar(x, ...)
   if (length(file) == 0) return(NULL)
   
   # Read multiple netmind files and concatenate them:
   if (length(file) == 0) return(NULL)
   if (length(file) > 1){
      for (i in 1:length(file)){
         #cat(paste(i, ") Reading: '", file[i], "'\n", sep = ""))
         header <- header.minilog(file[i])
         header <- as.data.frame(t(header), stringsAsFactors = FALSE)
         header["file.name"] <- unlist(lapply(strsplit(file[i], "/"), function(x) x[length(x)])[[1]])
         if (i == 1){
            x <- header
         }else{
            if (ncol(header) != ncol(x)){
               cat(paste(i, ") Problem reading: '", file[i], "'\n", sep = ""))
               header[, setdiff(names(x), names(header))] <- ""
               header <- header[, names(x)]
            }
            x <- rbind(x, header)
         } 
      }
      
      rownames(x) <- NULL
      
      return(x)
   }
   
   # Read and parse header info:
   warnings <- getOption("warn")
   options(warn = -1)
   y <- read.table(file = file, nrow = 10, colClasses = "character", sep = "\n")
   options(warn = warnings)
   y <- y[, 1]
   k <- grep("date", tolower(y)) 
   if (length(k) == 0) k <- (length(y) + 1)
   if (length(k) > 1)  k <- k[1]
   
   # Fix odd characters:
   y <- gsub('\xeb', " ", y)  
   y <- gsub('\xf8C', " ", y)
   y <- gsub('\xb0C', " ", y)
   y <- gsub('\xee', "i", y)  
   y <- gsub('\xfb', "u", y)  
   y <- gsub('\xce', "I", y) 
   y <- gsub('\xc9', "E", y) 
   y <- gsub('\xf4', "a", y) 
   y <- gsub('\xe0', "a", y) 
   y <- gsub('\xe9', "e", y)
   y <- gsub('\xe8', "e", y)  
   y <- gsub('\"+', " ", y)
   
   # Parse header information:
   header <- unlist(lapply(strsplit(y[1:(k-1)], "="), function(x) x[2]))
   names(header) <- gsub(" ", ".", tolower(gsub("^[*] ", "", unlist(lapply(strsplit(y[1:(k-1)], "="), function(x) x[1])))))
   header <- header[!is.na(header)] 
   
   return(header)
}
