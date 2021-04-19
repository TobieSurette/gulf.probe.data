#' @title Read Depth-temperature Probe Data
#'
#' @description Function for reading VEMCO Minilog depth-temperature probe data. This function reads the ASCII format Minilog data. 
#'              The file's header information is stored in the object's attributes, and maybe extracted using the 
#'              'header' function. If multiple file names are specified, the data are appended together and columns are 
#'              added which contain the header information from each file. 
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
#' # Read snow crab survey Minilog data from 1997:
#' x <- read.minilog(1997)

#' 
#' @export read.minilog
read.minilog <- function(x, survey = "sc", offset = 0, file, ...){
   # READ.MINILOG - Read a minilog file.

   # Define file(s) to be read:
   if (!missing(x) & missing(file)) if (is.character(x)) file = x
   if (missing(file)) file <- locate.minilog(x, ...)
   if (length(file) == 0) return(NULL)
   
   # Read multiple netmind files and concatenate them:
   if (length(file) == 0) return(NULL)
   if (length(file) > 1){
      x <- NULL
      for (i in 1:length(file)){
         cat(paste(i, ") Reading: '", file[i], "'\n", sep = ""))
         temp <- read.minilog(file[i])
         
         temp <- expand(temp)  # Attach attribute information as appended columns.
         
         if (!is.null(x)){
            # Create NA-valued columns if new variables appear:
            index <- setdiff(names(x), names(temp))
            if (length(index) > 0) temp[index] <- NA
            index <- setdiff(names(temp), names(x))
            if (length(index) > 0) x[index] <- NA
            
            # Order new file properly:
            temp <- temp[names(x)]
         }
         
         x <- rbind(x, temp)
      }
      temp <- attributes(x)
      temp <- temp[setdiff(names(temp), names(header(x)))]
      attributes(x) <- temp
      
      return(x)
   }

   # Read multiple minilog files and concatenate them:
   if (length(file) == 0) return(NULL)
   if (length(file) > 1){
      x <- NULL
      for (i in 1:length(file)){
          cat(paste(i, ") Reading: '", file[i], "'\n", sep = ""))
          temp <- read.minilog(file[i], ...)
          for (j in 1:length(header(temp))){
             temp[, names(header(temp))[j]] <- header(temp)[[names(header(temp))[j]]]
          }

          # Add columns if necessary:
          if ((length(setdiff(names(temp), names(x))) > 0) & (!is.null(x))){
             x[setdiff(names(temp), names(x))] <- NA
             x <- x[names(temp)]
          }

          # Append rows:
          x <- rbind(x, temp)
      }
      temp <- attributes(x)
      temp <- temp[setdiff(names(temp), names(header(x)))]
      attributes(x) <- temp
      if ("Study.ID" %in% names(x)) x$Study.ID <- unlist(lapply(strsplit(x$Study.ID, " "), function(x) x[[1]]))

      return(x)
   }

   # Read and parse header info:
   y <- read.table(file = file, nrow = 10, colClasses = "character", sep = "\n")
   y <- y[, 1]
   k <- max(which(substr(y, 1, 1) == "*"))
   
   # Fix odd characters:
   y <- gsub('\xeb', " ", y)  
   y <- gsub('\"+', " ", y)
   y <- gsub('\xf8C', " ", y)
   y <- gsub('\xb0C', " ", y)

   # Read minilog data:
   for (i in c(",", " ", "\t")){
      if (i == ","){
         x <- read.table(file = file, header = FALSE, skip = k, sep = i, colClasses = "character")
         if (ncol(x) > 1) sep <- i
      }
      if ((ncol(x) == 1) & (i != ",")){
         x <- read.table(file = file, header = FALSE, skip = k, sep = i, colClasses = "character")
         if (ncol(x) > 1) sep <- i
      }
   }

   # Rename fields:
   y[k] <- tolower(gsub("[*] ", "", y[k]))
   y[k] <- gsub("celsius", "temp", y[k])  
   y[k] <- gsub("meters", "depth", y[k])   
   y[k] <- gsub("temp[(] +[)]", "temp", y[k])
   fields <- tolower(unlist(strsplit(y[k], sep)[[1]]))
   fields[grep("date", fields)]  <- "date"
   fields[grep("time", fields)]  <- "time"
   fields[grep("atod", fields)] <- "depth"
   fields[grep("depth", fields)] <- "depth"
   fields[grep("temp", fields)]  <- "temperature"

   # Get date format:
   date.format <- tolower(unlist(strsplit(y[k], sep)[[1]]))[which(fields == "date")]
   date.format <- tolower(gsub("[)]", "", unlist(strsplit(date.format, "[(]"))[2]))
   
   # Parse header information:
   header <- unlist(lapply(strsplit(y[1:(k-1)], "="), function(x) x[2]))
   names(header) <- gsub(" ", ".", tolower(gsub("^[*] ", "", unlist(lapply(strsplit(y[1:(k-1)], "="), function(x) x[1])))))
   file.name <- lapply(strsplit(file, "/"), function(x) x[length(x)])[[1]]
   
   # Name variable fields:
   names(x) <- fields
   
   # Format numeric variables:
   if ("temperature" %in% names(x)) x$temperature <- as.numeric(x$temperature)
   if ("depth" %in% names(x))       x$depth <- as.numeric(x$depth)
   
   # Fix date formats:
   if (date.format == "yy-mm-dd"){
      ix <- as.numeric(substr(x$date, 1, 2)) < 20
      x$date[!ix] <- paste0("19", x$date[!ix])
      x$date[ix]  <- paste0("20", x$date[ix])
   }
   if (date.format == "dd-mm-yyyy") x$date <- paste0(substr(x$date, 7, 10), "-", substr(x$date, 4, 5), "-", substr(x$date, 1, 2))
   if (date.format == "mm-dd-yyyy") x$date <- paste0(substr(x$date, 7, 10), "-", substr(x$date, 1, 2), "-", substr(x$date, 4, 5))
   
   # Set header:
   header(x) <- header
   attr(x, "file.name") <- file.name
   
   # Modify time by specified offset:
   if (offset != 0){
      t <- as.matrix(as.POSIXlt(time(x) + offset * 60))
      x$year   <- t[, "year"] + 1900
      x$month  <- t[, "mon"] + 1
      x$day    <- t[, "mday"]
      x$hour   <- t[, "hour"]
      x$minute <- t[, "min"]
      x$second <- t[, "sec"]
   }

   # Convert to minilog object:
  # x <- minilog(x)

   return(x)
}
