
#' Utility functions for the tv package
#'
#' These utility functions are very specific for the tv rawdata.


#' @rdname util_tv
#' @export
#' 
id.time <- function() {
  s <- seq(from = t2s("02:00:00"), to = t2s("25:59:59"))
  data.table(id = s2t(s, as = 'n'), label = s, key = 'id')
}

#' @details \code{id.tmb}  reshapes the timeband object in 
#' \code{setup(tmb = list('wholeday' = c(start = '02:00:00', end = '25:59:59')))}
#' (default) into a more compact form for use in \code{overlap.join()}. Gives 
#' labels for each \code{tmb} if not specified in \code{names(tmb)}.
#' Important: if 'end' is specified like in Instar, than 1 second has to be 
#' subtracted to match Instar results! 
#' E.g.: if specified in Instar: 08:03:07 - 12:04:25 the equivalent for this
#'       tv package is \code{c(start = 08:03:07, end = 12:04:24)}, that means: end-1
#'
#' @rdname util_tv
#' 
id.tmb <- function(x) 
{
  paste. <- function(x) sapply(x, paste0, collapse = '-')
  if(is.null(n <- names(x))) { names(x) <- paste.(x)
  } else if(length(e <- which(n == ''))) { names(x)[e] <- paste.(x[e])
  } else invisible()
  x <- t(sapply(x, t2s))
  data.table(start=x[,1L], end=x[,2L], tmb=rownames(x), key=c('start','end'))
}

#' @details \code{id.day} is a function to conveniently create a sequence of Dates.
#' For convenience there are three ways of specifying the days to analyse:
#' \enumerate{
#'   \item by entering a prespecified vector of dates in \code{'day'}.
#'   \item by specifying the start end end dates in \code{'day'} and \code{'to'} respectively.
#'   \item by specifying start date \code{'day'} and the length of the sequence of days
#'   in \code{'nday'}. If \code{'nday'} is negative the sequence will be created backwards.
#' }
#' day is a character vector of dates in the form '2015-12-31'
#' Time shifted viewing is limited to 7 days by definition.
#' The demografics file is usually loaded for the live dates but this is 
#' allowed to differ: Analysing the viewing of a fixed sample at one
#' day alows to circumvent the fact that otherwise weights change every day.
#' use class Date to correctly creating sequencies of dates but store dates as 
#' character, its much easier to program with and more efficient
#' 
#' @rdname util_tv
#' @export
#' 
id.day <- function(day, to, dem.day = NULL) 
{
  is.date <- grepl("^\\d{4}-\\d{2}-\\d{2}$", day[1])
  if(!is.date) stop(paste('\nDate', dQuote(day[1]), 'is not in the expected format, use format: "2013-12-31"'))
  
  seq.day.n <- function(x, n=1L)
    as.character(seq.Date(as.Date(x), length.out=abs(n), by=paste(sign(n),'day')))
  
  seq.day.to <- function(x, to) 
    as.character(seq.Date(as.Date(x), as.Date(to), by='day'))
  
  day.live <- 
    if(length(day) > 1) { 
      sort(as.character(day))
    } else {
      if(length(to) > 1) stop("'to' needs to be length 1 vector")
      if(is.numeric(to)) {
        sort(seq.day.n(day, to))
      } else {
        x <- sort(sapply(list(day, to), as.character)) # allows to older than day
        seq.day.to(x[1], x[2])
      }
    }
  
  # to load tsv data corresponding to live date, we have to import 7 tsv-files 
  # more than live (into the future). If the choosen dates reach into the future
  # cut the sequence.
  day.tsv <- sort(unique(unlist(lapply(day.live, seq.day.n, n=8L))))
  day.tsv <- day.tsv[day.tsv < Sys.Date()-1]
  # tsv data contains a column of the live broadcasting reference date. This id 
  # table is for fast labelling this column, which is crucial for later joins.
  # It contains 7 more dates than tsv (into the past)
  day.tsv.live <- unique(unlist(lapply(day.tsv, seq.day.n, n=-8L)))
  # no need to sort day.tsv.live (not nice: sort(NULL) gives a warning)
  # use date labels not integer id for day. This is critical for joining tsv. 
  # -> compare the dates for live, tsv and tsv.live for noncontinuous date input
  list(
    dem     = id.dt(if(is.null(dem.day)) day.live else as.character(dem.day), 'day'),
    live    = id.dt(day.live,     'day'),
    tsv     = id.dt(day.tsv,      'daytsv'),
    prg     = id.dt(day.live,     'day'),
    tsvlive = id.dt(day.tsv.live, 'daylive', d2i(day.tsv.live,'ddmmyyyy'))
  )
}


file.exist <- function(id.day, path) 
{ 
  x <- c("dem","live","tsv","prg")
  for(j in x) setDT(id.day[[j]])[, file := date2file(label, j)]
  for(j in x) setDT(id.day[[j]])[, exist := file.exists(paste0(path, file))]
  id.day
}

#' \code{setup.check} is a function to check the \code{id} object for consistency.
#' and is useful to be checked before importing data.
#'
#' @rdname util_tv
#' before loading the data: try to catch errors!
#' 
setup.check <- function(id) 
{ 
  missing.files <- function(dt){
    miss <- lapply(dt, function(x) x[(!exist)])
    miss <- miss[lapply(miss, nrow) > 0]
    if(length(miss)) {
      warning("not all files are available: ", immediate. = TRUE)
      print(miss)
    }
  }
  missing.files(dt = id$lab$day[names(which(id$read))])
}


#' \code{pin.type} checks if column \code{pin} in dt (usually \code{dem}) 
#' represent household IDs (\code{hh}, 4 digits) or individuals IDs (\code{hh+ind}, 6 digits).
#' @rdname util_tv
#' @export
#' 
pin.type <- function(dt)
{
  pin <- dt[, .N, k=pin][, pin]
  test1 <- all(c(pin[1L] > 1e2, pin[length(pin)] > 1e4))
  test2 <- all(floor((pin/100-floor(pin/100))/10) %in% c(0,1,5))
  if(all(test1, test2)) 'ind' else 'hh'
}


read.dem.xls <- function(path)
{
  library(data.table)
  suppressMessages(if(!require('readxl')) install.packages('readxl'))
  on.exit(if(d[, any(sapply(.SD, is.factor))]) warning(paste(x, 'has factors')))
  d <- setDT(readxl::read_excel(path))
}


file.dem <- function(path)
{
  d <- read.dem.xls(paste0(path, 'dem.xlsx'))
  
  # --- file structure --------------------------------------------------------
  
  fwf <- d[, .N, by = name][!is.na(name)]
  fwf <- d[fwf, on = 'name', mult = 'first'][,-c('label','label.original','N')]
  fwf <- fwf[value != 'empty'][value != 'continuous', value := "categorical"]
  fwf[, date := as.character(date)]
  setnames(fwf, 'name.original', 'description')
  if(na.omit(fwf, invert = TRUE)[, .N] > 0) warning('NAs in create_file_dem_csv')
  cols <- which(sapply(fwf, is.character))
  convert <- function(x) iconv(x, from = 'UTF-8', to = 'latin1')
  for(j in cols) set(fwf, j = j, value = convert(fwf[[j]]))
  # out
  fwrite(fwf, file = paste0(path, 'file_dem.csv'), sep = ";")
  
  # --- labels ----------------------------------------------------------------
  
  d[, i:=.I][, name := d[!is.na(name)][d, on='i', roll=TRUE, name]][, i:=NULL]
  label <- split(d[, .(id = value, label)], d$name)
  cols <- sapply(label, function(x) x$id[1] %in% c('continuous','empty'))
  cols <- setdiff(names(which(cols)), 'age')
  label[cols] <- NULL
  
  # --- age -------------------------------------------------------------------
  
  asci.table <- function(size = c(128L, 256L)[2L]){
    dec <- seq.int(size) - 1L
    raw <- as.raw(dec)
    df <- data.table(
      dec = dec,
      hex = as.character(raw),
      chr = rawToChar(raw, multiple = TRUE),
      value = as.integer(raw) - 48L # codepont 48 = 0 (zero)
    )
  }
  id.asci <- asci.table()
  
  # label$age <- id.asci[value %in% 0:99, .(id = chr, label = value)]
  # because DLC has issues with asci2i on MAc:
  label$age <- id.asci[value %in% 0:99, .(id = value, label = value, ascii = chr)]
  
  # --- ascii to integer ------------------------------------------------------
  
  test.asci <- function(x) suppressWarnings(any(is.na(as.integer(x)))) 
  cols <- sapply(label, function(x) test.asci(x$id))
  cols <- names(which(cols))
  lapply(label[cols], function(x) x[, `:=` (asci = id, id = asci2i(id))])
  
  # --- special coding --------------------------------------------------------
  
  # ! attention with ascii variables, they may translate to different integer
  # codes than used by Mediapulse / BfS
  
  # EZ is stored by kantar such that values > 9 (eg, :, ;, < etc.) are 
  # coded as continuously growing, so the 16th EZ becomes 16 but is known as
  # ez23french or 23.
  
  setorder(label$ez[, id := as.integer(substr(label,3,4))], 'id')
  label$ez[, abr := substr(label,1,4)]
  
  # KT is differently coded than BfS (apparently in lexical order)
  # -> instead use these integer codes:
  
  kt.bfs <- c('AG'=19,'AR'=15,'AI'=16,'BL'=13,'BS'=12,'BE'=2,'FR'=10,'GE'=25,
              'GL'=8,'GR'=18,'JU'=26,'LU'=3,'NE'=24,'NW'=7,'OW'=6,'SG'=17,
              'SH'=14,'SZ'=5,'SO'=11,'TG'=20,'TI'=21,'UR'=4,'VS'=23,'VD'=22,
              'ZG'=9,'ZH'=1) 
  # order is code in dem!
  id.kt <- data.table(id.zone = kt.bfs, abr = names(kt.bfs))[, id := .I]
  setorder(label$kt[, c('abr','id') := id.kt[,.(abr, id.zone)]], 'id')
  
  # SG extra label
  label$sg[, abr := c('DE','FR','IT')]
  
  # --- delete continuous variables in label ----------------------------------
  
  # label[sapply(label, function(x) all(x$label %in% c(NA,'')))] <- NULL
  
  # --- id needs to be integer ------------------------------------------------
  
  lapply(label, function(x) x[, id := as.integer(id)])
  # only guest is logical
  #label$guest[, id.original := id][, id := ifelse(id.original==2, TRUE, FALSE)]
  suppressWarnings(
    label$guest[, id.original := id][, id := ifelse(id.original==2, TRUE, FALSE)]
  )
  
  # --- set id to variable name for simpler joins -----------------------------
  # X[Y, on = 'var'] instead of X[Y, on = c(id = 'var')]
  for(i in names(label)) setnames(label[[i]], 'id', i) # sapply(label, names)
  
  # --- return
  return(list(file = fwf, label = label))
}
