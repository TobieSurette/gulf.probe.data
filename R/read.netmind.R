#' Read Netmind Acoustic Trawl Data.
#'
#' @description Functions to read Netmind acoustic trawl measurement data.
#'
#' @param x Survey year or file name.
#' @param file File name(s).
#' @param year Survey year(s).
#' @param survey Survey type, as determined by the \link{survey.scsset} function.
#' @param tow.id Numeric value or character string specifying the ID corresponding to a particular tow sampling station.
#' @param offset Numeric value specifying the offset time (in minutes) to include as a corrective in the data time stamps.
#' @param repeats Logical value specifying whether to keep or average out data records with identical time stamps.
#' @param ... Other parameters passed onto \code{locate} functions or used to subset data.
#' 
#' @examples 
#' # Read snow crab survey Netmind data from 1990 to 1994:
#' x <- read.scanmar(1990:1994)
#' 

#' @export read.netmind
read.netmind <- function(x, file, offset = 0, repeats = FALSE, ...){
   # Define file(s) to be read:
   if (!missing(x) & missing(file)) if (is.character(x)) file = x
   if (missing(file)) file <- locate.netmind(x, ...)
   if (length(file) == 0) return(NULL)
   
   # Read multiple Netmind files and concatenate them:
   if (length(file) == 0) return(NULL)
   if (length(file) > 1){
      x <- NULL
      for (i in 1:length(file)){
         cat(paste(i, ") Reading: '", file[i], "'\n", sep = ""))
         tmp <- read.netmind(file[i])
         if (nrow(tmp) > 0) tmp <- gulf.utils::expand(tmp)  # Attach attribute information as appended columns.
         if (!is.null(x) & nrow(tmp) > 0){
            # Create NA-valued columns if new variables appear:
            index <- setdiff(names(x), names(tmp))
            if (length(index) > 0) tmp[index] <- NA
            index <- setdiff(names(tmp), names(x))
            if (length(index) > 0) x[index] <- NA
            
            # Order new file properly:
            tmp <- tmp[names(x)]
         }
         
         if (nrow(tmp) > 0) x <- rbind(x, tmp)
      }
      
      return(x)
   }

   # Read file and clean up weird characters:
   warnings <- getOption("warn")
   options(warn = -1)
   y <- read.table(file = file, quote = "",  colClasses = "character", sep = "\n", blank.lines.skip = FALSE)[[1]]
   
   # Replace problem characters:
   y <- gsub('\xee', "i", y)  
   y <- gsub('\xfb', "u", y)  
   y <- gsub('\xce', "I", y) 
   y <- gsub('\xc9', "E", y) 
   y <- gsub('\xf4', "a", y) 
   y <- gsub('\xe0', "a", y) 
   y <- gsub('\xe9', "e", y)
   y <- gsub('\xe8', "e", y)  
   y <- gsub('\xeb', " ", y)  
   y <- gsub('\"+', " ", y)
   
   # Fix blanks and missing data lines:
   y <- tolower(gulf.utils::deblank(y))
   y <- y[y != ""]
   options(warn = warnings)

   comment <- gsub("comment[s]*[: ]*", "", y[grep("comment", y)])
   tow <- strsplit(y[grep("tow", y)], "tow")[[1]]
   tow <- tow[length(tow)]
  
   # Define header info:
   header <- c(file.name = lapply(strsplit(file, "/"), function(x) x[length(x)])[[1]], 
               comment = comment, 
               tow = tow)
   
   # Define header information:
   ix <- grep("date", tolower(y))
   fields <- strsplit(gsub(" +", " ", y[ix]), " ")[[1]]
      
   # Remove non-data lines:
   y <- y[-(1:ix)]
   y <- gsub("[nwse]", "", y)
   y <- gsub(" +", " ", y)
   
   if (length(y) == 0){
      v <- as.data.frame(t(data.frame(fields)))
      names(v) <- fields
      v <- v[-1, ]
      gulf.metadata::header(v) <- header
      v <- netmind(v)
      return(v)
   }
   
   # Parse data and time:
   date <- unlist(lapply(strsplit(y, " "), function(x) x[1]))
   prefix <- ""
   if (nchar(date[1]) == 6){
      if (as.numeric(substr(date[1], 1, 2)) > 80) prefix <- "19"
      if (as.numeric(substr(date[1], 1, 2)) < 50) prefix <- "20"
      date = paste0(prefix, substr(date, 1, 2), "-", substr(date, 3, 4), "-", substr(date, 5, 6))
   }else{
      date = paste0(substr(date, 1, 4), "-", substr(date, 5, 6), "-", substr(date, 7, 8))
   }
   time <- unlist(lapply(strsplit(y, " "), function(x) x[2]))
   time <- paste0(substr(time, 1, 2), ":", substr(time, 3, 4), ":", substr(time, 5, 6))
   
   # Parse coordinates:
   lat <- as.numeric(unlist(lapply(strsplit(y, " "), function(x) paste(x[3:4], collapse = ""))))
   lon <- as.numeric(unlist(lapply(strsplit(y, " "), function(x) paste(x[5:6], collapse = ""))))

   # Build data frame:
   v <- data.frame(date = date, time = time, 
                   longitude = -abs(gulf.spatial::dmm2deg(lon)),
                   latitude = gulf.spatial::dmm2deg(lat),
                   stringsAsFactors = FALSE)
     
   # Attach remaining fields:              
   fields <- fields[-(1:which(fields == "longitude"))]
   for (i in 1:length(fields)){
       tmp <- unlist(lapply(strsplit(y, " "), function(x) x[6+i]))
       ix <- grep("[*]", tmp)
       tmp <- gsub("[*]", "", tmp)
       tmp <- as.numeric(tmp)
       tmp[-ix] <- NA
       tmp <- data.frame(tmp)
       names(tmp) <- fields[i]
       v <- cbind(v, tmp)
    }
  
   # Modify time by specified offset:
   if (offset != 0){
      t <- time(v) +  offset * 60
      v$date <- unlist(lapply(strsplit(as.character(t), " "), function(x) x[1]))
      v$time <- unlist(lapply(strsplit(as.character(t), " "), function(x) x[2]))
   }

   # Add header information as attributes:
   gulf.metadata::header(v) <- header

   # Convert to netmind object:
   v <- netmind(v)

   return(v)
}
