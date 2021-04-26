#' Read Star Oddi Probe Data.
#'
#' @description Functions to read Star Oddi data, such as depth/temperature or acoustic trawl monitoring data.
#'
#' @param x Survey year or file name.
#' @param file File name(s).
#' @param year Survey year(s).
#' @param survey Survey type, as determined by the \link{survey.scsset} function.
#' @param tow.id Numeric value or character string specifying the ID corresponding to a particular tow sampling station.
#' @param offset Numeric value specifying the offset time (in minutes) to include as a corrective in the data time stamps.
#' @param repeats Logical value specifying whether to keep or average out data records with identical time stamps.
#' @param ... Other parameters passed onto \code{locate} functions or used to subset data.

#' @export read.star.oddi
read.star.oddi <- function(x, ...) UseMethod("read.star.oddi")

#' @describeIn read.star.oddi Read a Star Oddi data file header information.
#' @export read.star.oddi.header
read.star.oddi.header <- function(x, file, verbose = FALSE, ...){
   # Define file(s) to be read:
   if (!missing(x) & missing(file)) if (is.character(x)) file = x
   if (missing(file)){
      if (missing(x)) file <- locate.star.oddi(...) else file <- locate.star.oddi(x, ...)  
   }
   if (length(file) == 0) return(NULL)
   
   # Read multiple netmind files and concatenate them:
   if (length(file) == 0) return(NULL)
   if (length(file) > 1){
      for (i in 1:length(file)){
         if (verbose) cat(paste(i, ") Reading: '", file[i], "'\n", sep = ""))
         header <- read.star.oddi.header(file[i])
         header <- as.data.frame(t(header), stringsAsFactors = FALSE)
         header["file.name"] <- unlist(lapply(strsplit(file[i], "/"), function(x) x[length(x)])[[1]])
         if (i == 1){
            x <- header
         }else{
            if (!all(names(header) %in% names(x))) x[setdiff(names(header), names(x))] <- ""
            if (!all(names(x) %in% names(header))) header[setdiff(names(x), names(header))] <- ""
            header <- header[names(x)]
            x <- rbind(x, header)
         } 
      }
      
      rownames(x) <- NULL
      
      return(x)
   }
   
   # Empty file:
   if (length(file) == 0) return(NULL)

   # Read and parse header info:
   y <- read.table(file = file, nrow = 30, colClasses = "character", comment.char = "", sep = "\n", blank.lines.skip = FALSE, fileEncoding = "Windows-1252")[[1]]
   y <- gulf.utils::deblank(y)
   y <- gsub("\t", " ", y)
   ix <- grep("^#", y)
   y <- y[ix]
   y <- gsub("^#[0-9]* ", "", y)

   # Parse header:
   str <- strsplit(y, ":")
   header <- deblank(unlist(lapply(str, function(x) x[2])))
   names(header) <- unlist(lapply(str, function(x) x[1]))
   names(header) <- gsub("#", "", names(header))
   names(header) <- gulf.utils::deblank(names(header))
   
   return(header)
}

#' @export read.star.oddi
read.star.oddi <- function(x, file, offset = 0, repeats = FALSE, verbose = FALSE, ...){
   # Define file(s) to be read:
   if (!missing(x) & missing(file)) if (is.character(x)) file = x
   if (missing(file)){
      if (missing(x)) file <- locate.star.oddi(...) else file <- locate.star.oddi(x, ...)  
   }
   if (length(file) == 0) return(NULL)

   # Read multiple netmind files and concatenate them:
   if (length(file) == 0) return(NULL)
   if (length(file) > 1){
      x <- vector(mode = "list", length = length(file))
      k <- 0
      for (i in 1:length(file)){
         if (verbose) cat(paste(i, ") Reading: '", file[i], "'\n", sep = ""))
         x[i] <- list(expand(read.star.oddi(file[i])))
         k <- k + nrow(x[[i]])
      }

      # Standardize data frame formats:
      vars <- unique(unlist(lapply(x, names)))
      for (i in 1:length(x)){
         ix <- setdiff(vars, names(x[[i]]))
         if (length(ix) > 0){
            x[[i]][ix] <- ""
            x[[i]] <- x[[i]][vars]
         }
      }

      # Efficiently catenate data frames:
      while (length(x) >= 2){
         ix <- seq(2, length(x), by = 2)
         for (i in ix) x[i] <- list(rbind(x[[i-1]], x[[i]]))
         if (i < length(x)) ix <- c(ix, length(x))
         x <- x[ix]
      }
      x <- x[[1]]
      
      gulf.metadata::header(x) <- NULL
      return(x)
   }

   # Read file header information:
   header <- read.star.oddi.header(file)
   
   # Define variable names:
   fields <- header[grep("Channel ", names(header))]
   fields <- unlist(lapply(strsplit(fields, " "), function(x) x[1]))
   fields <- tolower(gsub("[(].+[)]", "", fields))
   fields <- tolower(gsub("[-]", ".", fields))
   if (length(fields) == 0){
      # Define variable names:
      fields <- names(header)[grep("Axis ", names(header))]
      fields <- unlist(lapply(strsplit(fields, "Axis [0-9]+ +"), function(x) x[2]))
      fields <- unlist(lapply(strsplit(fields, "[(] *"), function(x) x[1]))
      fields <- tolower(gsub("[(].+[)]", "", fields))
      fields <- tolower(gsub("[-]", ".", fields)) 
   }
   
   # Read E-Sonar data:
   x <- read.table(file = file, header = FALSE, skip = length(header), dec = ",", sep = "\t",
                   colClasses = c("numeric", "character", rep("numeric", length(fields))))
   fields <- c("record", "date", fields)
   names(x) <- fields

   # Parse date fields:
   date <- data.frame(date = paste0(paste0("20", substr(x$date, 7, 8)), "-", substr(x$date, 4, 5),  "-", substr(x$date, 1, 2)), stringsAsFactors = FALSE)
   time <- data.frame(time = unlist(lapply(strsplit(x$date, "[ ,]"), function(x) x[2])), stringsAsFactors = FALSE)

   # Create result variable:
   v <- cbind(x["record"], date, time, x[setdiff(names(x), c("date", "record"))])

   # Modify time by specified offset:
   if (offset != 0){
      t <- time(v) +  offset * 60
      v$date <- unlist(lapply(strsplit(as.character(t), " "), function(x) x[1]))
      v$time <- unlist(lapply(strsplit(as.character(t), " "), function(x) x[2]))
   }
   
   # Convert to 'star.oddi' object:
   v <- star.oddi(v)
   header(v) <- header

   return(v)
}
