probe.scsset <- function(x, ...){

   # Minilog data:
   mf <- locate.minilog(year = year(x))
   mh <- read.minilog.header(mf)
   mh$tow.id <- toupper(mh$study.description)
   ix <- match(x$tow.id, mh$tow.id)
   mf <- mf[ix]
   mh <- mh[ix, ]

   # eSonar data:
   ef <- locate.esonar(year = year(x))
   eh <- read.esonar.header(ef)
   eh$tow.id <- gsub("[.].*", "", toupper(eh$file.name))
   ix <- match(x$tow.id, eh$tow.id)
   ef <- ef[ix]
   eh <- eh[ix, ]  
   
   # Star Oddi headline data:
   sf <- locate.star.oddi(year = year(x))
   sh <- read.star.oddi.header(sf)
   sh$tow.id <- gsub("[.].*", "", toupper(sh$file.name))
   ix <- match(x$tow.id, sh$tow.id)
   sf <- sf[ix]
   sh <- sh[ix, ]     
   
   # Star Oddi tilt data:
   tf <- locate.star.oddi(year = year(x), location = "tilt")
   th <- read.star.oddi.header(tf)
   th$tow.id <- gsub("[.].*", "", toupper(th$file.name))
   ix <- match(x$tow.id, th$tow.id)
   tf <- tf[ix]
   th <- th[ix, ]      
   
   vars <- c("date", "tow.id")
   v <- x[vars]
   
   m <- read.minilog(mf)
   m$tow.id <- tow.id(m)
   mr <- aggregate(list(minilog = m$temperature), by = m[vars], length)
   
   e <- read.esonar(ef)
   e$tow.id <- tow.id(e)
   er <- aggregate(list(esonar = m$temperature), by = e[vars], length)  
}
