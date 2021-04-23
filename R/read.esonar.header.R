read.esonar.header <- function(x, file, ...){
   # Define file(s) to be read:
   if (!missing(x) & missing(file)) if (is.character(x)) file = x
   if (missing(file)) file <- locate.esonar(x, ...)
   if (length(file) == 0) return(NULL)

   # Read multiple netmind files and concatenate them:
   if (length(file) == 0) return(NULL)
   if (length(file) > 1){
      x <- NULL
      for (i in 1:length(file)){
         #cat(paste(i, ") Reading: '", file[i], "'\n", sep = ""))
         header <- header.esonar(file[i])
         header["file.name"] <- unlist(lapply(strsplit(file[i], "/"), function(x) x[length(x)])[[1]])
         #if (length(header) != 5) cat(paste(i, ") Reading: '", file[i], "'\n", sep = ""))
         x <- rbind(x, header)
      }

      x <- as.data.frame(x) 
      rownames(x) <- NULL
      
      return(x)
   }

   # Read and parse header info:
   y <- read.table(file = file, nrow = 10, colClasses = "character", sep = "\n", blank.lines.skip = FALSE)

   # Define header information:
   header <- NULL
   vars <- gsub(" ", "", strsplit(y[1, ], ",")[[1]])
   values <- strsplit(y[2, ], ",")[[1]]
   if (length(values) > length(vars)) values <- values[1:length(vars)]
   header[vars] <- values
   comment <- strsplit(y[4, ], ",")[[1]]
   if (length(comment) == 0) comment <- ""
   vars <- gsub(" ", "", strsplit(y[3, ], ",")[[1]])
   if (length(comment) > 1){
      vars <- vars[vars != ""]      
      comment <- paste(comment[comment != ""], collapse = ", ")
   }
   header[vars] <- comment
   header <- header[names(header) != ""]
   
   return(header)
}

