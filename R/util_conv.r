
##' conversion functions
##' 
##' 
# In 2013 Mediapulse has ordered Kantar Media Audiances (KMA)to measure TV 
# audiance in Switzerland

# The TV Audiance Measurement (TAM) system run by Kantar Media Switzerland (KMS)
# is based on seconds level (KMA standard is minutes). Most data processing, 
# weighting and estimate calculating are based on dayly bases

# A basic viewing statement is defined by start and end point (in seconds) and 
# labeled by corresponding day, person and channel

# start end end points have the following properties:
# time is zero based: start=3 and end=7 gives a viewing duration of 5, this is: 
# duration = end - start + 1
# A day lasts from two o'clock till two o'clock of the next calender day, 
# this is: from '02:00:00' to '25:59:59' or 7200 to 93599 in seconds

# key parameters used in many functions:
# tmin <-  7200L # lowes possible value:        7200 seconds = t2s('02:00:00')
# tmax <- 93599L # highest possible value:     93599 seconds = t2s('25:59:59')
# t24h <- tmax - tmin # number of seconds in a day: 86400 seconds = t2s('24:00:00')
# dur <- function(x) end - start + 1

# functions to translate between different time/date representations 

# about performance:
# - no slowdown because of if(is.character(x)){...} of if(function argument){...}
# - no slowdown because of scientific notation e.g. 1e4 instead of 1000
# - slowdown if 1000L is used instead of 1000 (coercion to double)
# - as.integer() 2 x faster that trunc() 2 x faster than floor()
# - sprintf() is 3x faster than paste()
# - coercion to character is 30 times slower than adding hour, min, sec to an integer
# - if() is slightly faster than elseif()
# - as.Date 6x slower than returning the string date
# - temporay results to simplify following if else are slowing down (instead of
#   full code in each if else)

# see my question on stackoverflow:
# "Is it possible to get a performance advantage by using integers in R?"


#' kantar time (hhmmss or "hh:mm:ss") to seconds since 00:00:00
#' e.g: 110304 or "11:03:04" -> 39784
#' 
#' if(is.character(x)) x <- as.numeric(gsub(':','',x)) # no slowdown
#' hour <- trunc(x / 1e4)
#' min  <- trunc((x - hour * 1e4) / 1e2)
#' sec  <- x - hour * 1e4 - min * 1e2
#' as.integer(hour * 3600 + min * 60 + sec)
#' 
#' @rdname util_conv
#' @export
#' 
t2s <- function(x){
  x <- if(is.character(x)) as.integer(gsub(':','',x)) else as.integer(x)
  x %/% 1e4L * 3600L + (x %% 1e4L %/% 1e2L * 60L) + x %% 1e2L
}


#' inverse of t2s
#' e.g: 39784 -> 110304 or "11:03:04"
#' x = 44330
#' x = 27800.452 # x <- round(x) otherwise as = "t gives results with decimal x
#' x can be decimal, for example after dur * weight
#' @rdname util_conv
#' @export
#' 
s2t <- function(x, as = 't')
{ 
  x <- round(x)
  hour <- trunc(x / 3600)
  min  <- trunc((x - hour * 3600) / 60)
  sec  <- x - hour * 3600 - min * 60
  if(all(sec %% 1 == 0)) {  
    if(as == 'n')
      as.integer(hour * 1e4 + min * 1e2 + sec)
    else if(as == 't')
      sprintf('%02i:%02i:%02i', hour, min, sec)
    else if(as == 'm')
      sprintf('%04i:%02i', hour*60 + min, sec)
  } else {
    if(as == 'n')
      hour * 1e4 + min * 1e2 + sec
    else if(as == 't')
      sprintf('%02i:%02i:%07.4f', hour, min, round(sec,4))
    else if(as == 'm')
      sprintf('%04i:%07.4f', hour*60 + min, round(sec,4))
  }
}


#' @rdname util_conv
#' @export
#' 
i2d <- function(x, as = 'c'){
  # kantar date (ddmmyyyy) to date ("yyyy-mm-dd")
  # e.g: 22022016 -> "2016-02-22"
  day   <- trunc(x / 1e6)
  month <- trunc((x - day * 1e6) / 1e4)
  year  <- x - day * 1e6 - month * 1e4
  if(as == 'i') 
    as.integer(year * 1e4 + month * 1e2 + day)
  else if(as == 'c')         
    sprintf('%.4i-%.2i-%.2i', year, month, day)
  else if(as == 'd') 
    as.Date(sprintf('%.4i-%.2i-%.2i', year, month, day))
}


#' yyyymmdd   date as integer
#' yymmdd     date in filename
#' ddmmyyyy   date in data
#' 
#' @examples
#' date <- c("2014-02-28","2016-11-03")
#' x <- as.integer(gsub('-', '', date))
#' yy <- x %/% 1e4L
#' mm <- x %% 1e4L %/% 1e2L
#' dd <- x %% 1e2L
#' dd * 1e6L + mm * 1e4L + yy
#' 
#' @rdname util_conv
#' @export
#'  
d2i <- function(date, to = c('yyyymmdd','yymmdd','ddmmyyyy')[1]) 
{
  x <- as.integer(gsub('-', '', date))
  switch(to,
    'yyyymmdd' = x,
    'yymmdd'   = x %% 1e6L,
    'ddmmyyyy' = (x %% 1e2L) * 1e6L + (x %% 1e4L %/% 1e2L) * 1e4L + (x %/% 1e4L)
    )
}


#' date2file(c("2018-01-01","2018-01-02"), type = "dem")
#' @rdname util_conv
#' @export
#' 
date2file <- function(dates, type = c('dem','live','tsv','prg')) 
{
  d <- gsub("-", "", dates)
  fmt <- switch(type, 
                dem  = "%s/AU%s.dem", 
                live = "%s/AU%s.swo", 
                tsv  = "%s/PB%s.swd",
                prg  = "%s/DE%s.std"
                )
  sprintf(fmt, substr(d,1,4), substr(d,3,8))
}

#' file2date(c('2013/AU130225.dem','/AU130227.swo'), type = TRUE)
#' grep can handle case like 'std' and 'STD'
#' @rdname util_conv
#' @export
#' 
file2date <- function(path, type = TRUE, ext = c(dem='dem', swo='live', 
                      swd='tsv', std='program')){
  x <- as.integer(substr(basename(path), 3L, 8L))
  d <- sprintf('20%02i-%02i-%02i', x %/% 1e4, x %% 1e4L %/% 1e2L, x %% 1e2L)
  if(!type) d else {
    y <- substr(basename(path), 10L, 13L)
    paste(ext[match(y, names(ext))], d)
  }
}


#'
#' recode ascii/unicode to integer. 
#' In kantar's fixed width text files most variable have field witdth 1. 
#' Variable with more than codes 0 to 9 are written as ascii codes with 0 as
#' codepoint 48 (= character "0"). For variables with many levels like age, 
#' codes exceed the standard 127 ascii code range, e.g. 48+99=147, codes > 127
#' are returned by readr::read_fwf() as unicode: e.g. "<U+0082>".
#' to convert to integer age use: as.integer(charToRaw(x))-48
#' charToRaw is not vectorized, and returns NA as vector length > 1
#' below is the fastest code tested extensively by benchmarking in the 
#' framework of data.table as well as base-R with input vectors length 5 milion
#' @rdname util_conv
#' @export
#' 
asci2i <- function(x) {
  y <- rep.int(NA_integer_, length(x))
  y[!is.na(x)] <- 
    as.integer(vapply(x[!is.na(x)], charToRaw, raw(1L), USE.NAMES=FALSE)) - 48L
  y
}
