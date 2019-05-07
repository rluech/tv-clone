
#' Calculating facts 
#' 
#' \code{calc} calculates standard estimates of TV viewing, called \bold{facts}
#' in Instar Analytics. The formulae are documented in Instar Analytics and the 
#' Mediapulse documentation "Gold Standards". \code{calc} aimes to yeld exactly 
#' the same results as Instar Analytics. \code{calc.uni} calculates \code{sample} 
#' and \code{universe}, \code{calc.share} calculates the share.
#' 
#' @param dt A data.table, a left join of \code{dem} and \code{view} on the keys
#'  \code{day} and \code{pin}, e.g. \code{calc(dt = view[dem, on = c('day','pin')])}
#'
#' @param by A character vector, specifying the groups by which the facts will 
#' be calculated. Default is day. Fact are always calculated by day, plus 
#' optionally more grouping variables. Make sure that sample and universe were 
#' calculated by the very same variables (targets).
#' 
#' @param period A character vector, specifying the name of the column which 
#' represents the period. By default this is 'day'. The Period is needed to 
#' calculated the \bold{Rating} (\code{rtgT and rtgP}). Rating is the summed 
#' viewing duration devided by a certain time period. By default ('day') this 
#' is 24 hours or 86400 seconds. When calculating facts by timeband, period has 
#' to be specified as \code{'tmb.dur'}, the duration of each timeband, e.g.:
#' \code{calc(, by = c('day','tmb'), period = 'tmb.dur')}. The same applies for 
#' calculation by programs, e.g.: \code{calc(, by = c('day','prg'), period = 'prg.dur')}.
#' If period is not specified correctly you will see that, compared with Instar,
#' Reach will be correct but Rating is not.
#' 
#' @param unit One of \code{'hour','min','sec'}. Currently not implemented. 
#' Results are in seconds.
#' 
#' @param na.to.0 \code{TRUE} (default) or \code{FALSE}. Replace NAs by Zeros? 
#' Useful to compare results with Instar Analytics
#' 
#' @param x A data.table, usually the object \code{dem}.
#' 
#' @param target A character vector, specifying the variables by which sample
#'  and universe will be calculated. By default \code{'day'}.
#'  
#' @param obs The level observation, either 'ind' or 'hh'. Is the column 
#' \code{pin} representing individuals or households? By default the value in 
#' \code{id$view$obs} is taken which was specified previously in \code{setup(..., obs = )}
#' 
#' @details 
#' \code{calc.uni} calculates \code{sample} and \code{universe} in the \code{dem} 
#' file. To calculate facts, first \code{sample} and \code{universe} have to be 
#' calculated and added as columns to \code{dem}, see details.
#' \code{calc.share} calculates the share, typically between channels. Share 
#' by definition is calculated based on the fact \code{rtgP} \emph{Rating in Percent}.
#' By default the following facts are returned:
#' \code{sample,universe,n,dur,rchT,rchP,rtgT,rtgP,share}. Attention! share 
#' usually is incorrect and has to be calculated separately.
#' 
#' @examples
#' library(tv)
#' setup('2018-01-01', dem.var = 'sg', import = TRUE)
#' dem[] # By default sample and universe are already calculated (added as columns), 
#'       # targets are all the variables specified in setup(..., dem.var = ).
#'       # This is not always inteded, make sure to have the correct targets 
#'       # before calculating facts.
#'
#' # calculate sample and universe
#' setup('2018-01-01', 3, dem.var = c('sg','hhsize'), dem.uni = FALSE, import = TRUE)
#' dem[] # setup(dem.uni = FALSE) will suppress the calculation of sample and universe.
#' calc.uni(dem, target = 'sg') # calculate sample and universe with target 'sg'
#'                              # sample and universe are always calculated by day.
#'                              
#' # with sample and target by 'sg' we can now calculate facts by 'sg'.
#' res <- calc(view[dem, on = c('day','pin')], by = c('day','sg'))                             
#' res[]
#' 
#' # If we want facts by sg and hhsize we first have to calculate the correct sample and universe:                        
#' calc.uni(dem, target = c('sg','hhsize')) # overwriting the columns sample and universe
#' res <- calc(view[dem, on = c('day','pin')], by = c('day','sg','hhsize')) 
#' res[]
#' 
#' @rdname calc
#' @export
#' 
calc <- function(dt, by='day', period=NULL, unit='hour', na.rm=TRUE, na.to.0=TRUE) 
{
  dt[, pin2 := pin] # the copy is only necessary if 'pin' appears in 'by'
  on.exit(dt[, pin2 := NULL])
  
  if(is.null(period) || period=='day') { 
    if(!'period' %in% names(dt)) dt[, period := 86400L] # day
  } else {
    if('period' %in% names(dt)) dt[, period := NULL]
    dt[, period := get(period)]
    }
  
  x <- c('sample','universe','period')
  y <- match(x,names(dt))
  if(any(is.na(y))) stop('missing column: ', paste(x[is.na(y)], collapse=' '))
  
  calc <- expression({
    
    sample   = sample[1]
    universe = universe[1]
    period   = period[1]
    
    nil      = !is.na(dur) # only needed in view[dem]
    dup      = !duplicated(pin2)
    rchT     = sum(weight * nil * dup) # count viewer once and only once
    rchP     = rchT / universe * 100
    dur      = sum(weight * dur, na.rm = TRUE)
    #mdurA    = dur / universe
    #mdurV    = dur / rchT
    rtgT     = dur / period
    rtgP     = rtgT / universe * 100
    
    ans <- list(
      sample   = sample,
      universe = universe,
      n        = sum(nil * dup), 
      dur      = dur,
      rchT     = rchT, 
      rchP     = rchP,
      #mdurA    = mdurA / unit,
      #mdurV    = mdurV / unit,
      rtgT     = rtgT,
      rtgP     = rtgP
    )
    # ans <- lapply(ans, round, digit)
  })
  
  unit <- switch(unit, hour = 3600L, min = 60L, sec = 1L)
  res <- dt[, eval(calc), keyby = by]
  
  set(res, i=which(is.na(res[['rtgP']])), j='rtgP', value=0)
  if(is.element('day',names(res))) res[, share := rtgP / sum(rtgP) * 100, k=day]
  
  if(na.rm) res <- na.omit(res, cols = by)
  if(na.to.0) na.to.0(res)
  # if(any(c("chn","chn.name","prg") %in% by) & "rtgT" %in% by) setorder(res, -rtgT)
  res
}

#' @rdname calc
#' @export
#' 
calc.uni <- function(x, target = NULL, obs = id$view$obs) {
  # add columns sample and universe
  # for obs=='hh', an assignement has to occure dem <- calc.uni(d$dem), other-
  # wise filtering does not take place
  keys <- unique(c('day',target))
  # if(obs == 'hh') {
  #   x <- x[.(1), on='hw'][, pin := as.integer(pin/100)]
  #   on.exit(x[, c('guest','hw') := NULL])
  # }
  stopifnot(all(x[, .N == uniqueN(pin), k=keys]$V1))
  x[, `:=` (sample=.N, universe=sum(weight*!guest)), k=keys][]
}


#'@rdname calc
#' @export
#' 
calc.share <- function(dt, id.chn){
  # we need the complete list of channels in ttv to calculate the share correctly
  # alternatively and more precisely we need sum(rtgP) of all channels in ttv
  dt <- dt[id.chn, on=c(chn='channel')]
  na.to.0(dt)
  dt[, share := rtgP / sum(rtgP) * 100, k=day][]
}


# --- Comment to 'pin2' -------------------------------------------------------
# if by='pin', dup=!duplicated(pin) would no longer work! Because 'pin' is no 
# longer available in 'j' if mentioned in 'by'
# dt <- data.table(id=rep(1:3, c(3,2,3)), value=c(rnorm(8)))
# dt[, duplicated(id), by='id']
# dt[, id2 := id]
# dt[, duplicated(id2), by='id']
# -----------------------------------------------------------------------------

# --- understand duration, pin, guest, nil, sample, weigth, universe ----------
# 
# # duration
# d[is.na(dur),.N] == 0
# d[, max(dur), by = day][, t := s2t(V1, 't')][]
# d[, sum(dur)/3600, by = .(day,pin)][, .(plot(V1, ylab = 'hour'), abline(h=0:24))]
# 
# # sample
# d[, .N, by = .(day,pin)][, .N, by = day]
# d[, .N, by = .(day,pin)][, .N, by = day][, plot(N, type='s', xlab = 'day')]
# 
# # guest
# d[(guest), .N, k=.(day,pin)][, .N, k=day]
# d[(guest), .N, k=.(day,pin)][, .N, k=day][, plot(N, type='s')]
# dcast(d[, .N, k=.(day,guest)], day ~ guest)[, ratio := .(`TRUE`/`FALSE`*100)][]
# 
# d[, .N, k=.(day,pin,guest)][, .(regular=sum(!guest),guest=sum(guest)), k=.(day)]
# dcast(d[, .N, k=.(day,pin,guest)][, .N, k=.(day,guest)], day ~ guest) # keine col names!
# 
# x <- d[, .N, k=.(day,pin,guest)][, .(r=sum(!guest),g=sum(guest)), k=.(day)]
# x[, p := g/(r+g)*100][] # sum(V1,V2) is not the same as V1 + V2 !!!
# 
# # nil viewer
# d[is.na(ttv), .N, k=.(day,pin)][, .N, k=day]
# # d[dur<1, .N, k=.(day,pin)][, .N, k=day] # the same
# d[dur<1, .N, k=.(day,pin)][, .N, k=day][, plot(N, type='s')]
# 
# x <- d[(!guest), .N, k=.(day,pin,nil=dur<1)][, .(.N, v=sum(!nil),nv=sum(nil)), k=.(day)]
# x[, N == v+nv] # sum(V1,V2) is not the same as V1 + V2 !!!
# x[, p := nv/(v+nv)*100][]
# 
# # Why are there 'geust' as nil-viewer?
# x <- d[(guest), .N, k=.(day,pin,nil=dur<1)][, .(.N, v=sum(!nil),nv=sum(nil)), k=.(day)]
# x[, p := nv/(v+nv)*100][]
# 
# # viewer
# d[dur>0, .N, k=.(day,pin)][, .N, k=day]
# d[dur>1, .N, k=.(day,pin)][, .N, k=day][, plot(N, type='s')]
# 
# # viewer no guest
# d[(!guest) & dur>0, .N, k=.(day,pin)][, .N, k=day]
# d[(!guest) & dur>0, .N, k=.(day,pin)][, .N, k=day][, plot(N, type='s')]
# 
# # universe
# x <- d[, .(.N,dur=sum(dur)), k=.(day,pin,weight,guest)]
# x[, .(sample=.N, universe=sum((!guest)*weight)), k=day]

# -----------------------------------------------------------------------------
