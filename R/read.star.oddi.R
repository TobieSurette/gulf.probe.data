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
read.star.oddi <- function(x, file, offset = 0, repeats = FALSE, ...){
   # Define file(s) to be read:
   if (!missing(x) & missing(file)) if (is.character(x)) file = x
   if (missing(file)) file <- locate.star.oddi(x, ...)
   if (length(file) == 0) return(NULL)

   # Read multiple netmind files and concatenate them:
   if (length(file) > 1){
      x <- NULL
      for (i in 1:length(file)){
          cat(paste(i, ") Reading: '", file[i], "'\n", sep = ""))
          temp <- read.star.oddi(file[i])
          information <- header(temp)
          for (j in 1:length(information)){
             temp[, names(information)[j]] <- information[[names(information)[j]]]
          }

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

   # Empty file:
   if (length(file) == 0) return(NULL)

   # Read and parse header info:
   y <- read.table(file = file, nrow = 30, colClasses = "character", comment.char = "", sep = "\n",
                   blank.lines.skip = FALSE, fileEncoding = "Windows-1252")[[1]]
   y <- deblank(y)
   y <- gsub("\t", " ", y)
   index <- grep("^#", y)
   k <- max(index)
   y <- y[index]
   y <- gsub("^#[0-9]* ", "",y)

   # Parse header:
   str <- strsplit(y, ":")
   header <- deblank(unlist(lapply(str, function(x) x[2])))
   names(header) <- deblank(unlist(lapply(str, function(x) x[1])))

   # Extract file name:
   file.name <- lapply(strsplit(file, "/"), function(x) x[length(x)])[[1]]

   # Define variable names:
   fields <- header[grep("Channel ", names(header))]
   fields <- unlist(lapply(strsplit(fields, " "), function(x) x[1]))

   # Read E-Sonar data:
   x <- read.table(file = file, header = FALSE, skip = k, dec = ",", sep = "\t",
                   colClasses = c("numeric", "character", rep("numeric", length(fields))))
   fields <- c("record", "date", fields)
   names(x) <- fields

   # Parse date fields:
   date <- data.frame(year = as.numeric(substr(x$date, 7, 8)),
                      month = as.numeric(substr(x$date, 4, 5)),
                      day = as.numeric(substr(x$date, 1, 2)),
                      stringsAsFactors = FALSE)

   x$time <- unlist(lapply(strsplit(x$date, "[ ,]"), function(x) x[2]))
   time <- data.frame(hour   = as.numeric(substr(x$time, 1, 2)),
                      minute = as.numeric(substr(x$time, 4, 5)),
                      second = as.numeric(substr(x$time, 7, 8)),
                      stringsAsFactors = FALSE)

   # Auto-correct date and time:
   index <- (date$year < 100)
   date$year[index] <- 2000 + date$year[index]
   index <- is.na(date$year)
   date$year[index] <- unique(date$year[!index])[1]
   index <- is.na(date$month)
   date$month[index] <- unique(date$month[!index])[1]
   index <- is.na(date$day)
   date$day[index] <- unique(date$day[!index])[1]

   # Create result variable:
   v <- cbind(x["record"], date, time, x[setdiff(names(x), c("date", "record"))])

   # Modify time by specified offset:
   if (offset != 0){
      t <- as.matrix(as.POSIXlt(time(v) + offset * 60))
      v$year   <- t[, "year"] + 1900
      v$month  <- t[, "mon"] + 1
      v$day    <- t[, "mday"]
      v$hour   <- t[, "hour"]
      v$minute <- t[, "min"]
      v$second <- t[, "sec"]
   }

   # Add tow ID to header:
   if (length(grep("GP[0-9][0-9][0-9]", file)) > 0){
      temp <- unlist(lapply(strsplit(file, "GP"), function(x) x[length(x)]))
      temp <- lapply(strsplit(temp, "/", fixed = TRUE), function(x) x[1])
      tow.id <- paste0("GP", unlist(temp))
      tow.id <- unlist(lapply(strsplit(tow.id, "[.]"), function(x) x[1]))
   }

   # Create 'star.oddi' object:
   v <- star.oddi(v, header = header, tow.id = tow.id, file.name = file.name, ...)

   # Define measurement units:
   index <- grep("[(]", names(v))
   vars <- names(v)[index]
   units <- strsplit(gsub("[)]", "", vars), "[(]")
   vars <- tolower(unlist(lapply(units, function(x) x[1])))
   units <- unlist(lapply(units, function(x) x[2]))
   units <- gsub("°", "degrees", units)
   str <- names(v)
   str[index] <- vars
   names(v) <- str
   names(units) <- vars
   gulf.metadata::units(v) <- units

   return(v)
}
