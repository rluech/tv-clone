
#' these functions together solve the task of merging a lookup table to viewing 
#' by start end interval. This is relevant for timeband and programs. The 
#' workorse is data.table::foverlaps() in overlap()

overlap <- function(x, y, keys = NULL, nomatch = NA) # x <- view; y <- tmb; y <- prog
  {
  # overlap join of two data.tables followed by calculation of length of overlap
  # In foverlaps(x, y,...), x sould be a long table and y a short one. Do not 
  # reverse x and y, the performance is very slow if y is big.
  # Typically x is viewing statements, y is program logs. 
  # y must be keyed, there needs to be 'start' 'end' column in both x and y.
  # if intervals in y cover the wohle day (no gaps, no overlaps) total duration 
  # in x before and after overlap() is the same.
  # returned is column order [key, y, x] with "i." belonging to x. 
  # nomatch = 0 only returns those rows of y that have a match in x
  # keys = NULL (default) means overlap join only on start/end colums
  # for more details see: ?data.table::foverlaps
  #
  # usually we are interested in the length of overlaps e.g. the start/end 
  # interval in x adjusted to start/end in y, and dur recalculated on these.
  # We want the new columns of interest to call start, end, dur. So columns in x 
  # will be named to x.start, x.end, x.dur (if dur exists), and same for y.
  # We will return [key, y, x, start, end, dur]
  
  if('dur' %in% names(y)) setnames(y, 'dur', 'y.dur')
  if('dur' %in% names(x)) setnames(x, 'dur', 'x.dur')
  
  setkeyv(y, c(setdiff(keys, c('start','end')), c('start','end')))
  setkeyv(x, NULL)  # catch error if x has "old", wrong keys

  x <- foverlaps(x, y, nomatch = nomatch)
  
  setnames(x, c('start','end'), c('y.start','y.end'))
  setnames(x, c('i.start','i.end'), c('x.start','x.end'))
  x[, `:=` (start = x.start, end = x.end)]
  x[start < y.start, start := y.start]
  x[end > y.end, end := y.end]
  x[, dur := end - start + 1]

  # foverlaps returns: 
  #
  #    |---------|     : y
  # |----|             : x.1
  #            |----|  : x.2
  # |---------------|  : x.3
  #       |--|         : x.4
  #
  # usually the overlaping duration is of interest (re-set start/end in x):
  #
  #    |---------|     : y
  #    |-|             : x.1
  #            |-|     : x.2
  #    |---------|     : x.3 
  #       |--|         : x.4
  #
  # intervals in x overlapping with multiple intervals in y get duplicated,
  # increasing the number of rows
  #
  # |----|             : y.1
  #            |----|  : y.2
  #    |---------|     : x.1
  #
  # becomes:
  #
  # |----|             : y.1
  #            |----|  : y.2
  #    |----------|    : x.1_y.1
  #    |----------|    : x.1_y.2
  #
  # and after re-set of start/end in x:
  #
  # |----|             : y.1
  #            |----|  : y.2
  #    |-|             : x.1_y.1
  #            |--|    : x.1_y.2
  #
  # in above examples the sum of interval length is shorter in the end than in 
  # the original x because of re-setting start/end in x to start/end in y. This 
  # is ok if only overlapping length is of interest.
  # To retain the original sum of length in x, y is "filled up" with dummy 
  # intervals, so that every possible second is covered by an interval, so every
  # interval in x will have a match in y. The id of a dummy interval will be NA
  # 
  # |-|                : y.NA
  #   |--|             : y.1
  #      |-----|       : y.NA
  #            |---|   : y.2
  #                |-| : y.NA
  # |-------|          : x.1
  #            |--|    : x.2
  #
  # hence, total sum of length of intervals in x is retained:
  #
  # |-|                : y.NA
  #   |--|             : y.1
  #      |-----|       : y.NA
  #            |---|   : y.2
  #                |-| : y.NA
  # |-|                : x.1_y.NA
  #   |--|             : x.1_y.1
  #       |-|          : x.1_y.NA
  #            |--|    : x.2_y.2
  #
  # however, if y has overlapping intervals:
  #
  #    |------|        : y.1
  #        |------|    : y.2
  #     |-----------|  : x.1
  #
  # ... the total sum of length of intervals in x is no longer retained:
  #
  #    |------|        : y.1
  #        |------|    : y.2
  #     |-----|        : x.1_y.1
  #        |------|    : x.1_y.2
  # 
  # In this situation some viewing gets counted more than once in regards of total x. 
  # However in regard of each y summing up the corresponding viewing is totally 
  # valid. Just keep in mind, all y together is then more than the original viewing.
}

gap.find <- function(x, tmin = 7200L, tmax = 93600L) 
  {
  # create complete lookup table for input y in overlap(x,y)
  # x is a data.table with 'start' and 'end' column and possibly further column
  # if intervals in x are not continuous, this function creates new intervals
  # filling the gaps between intervals, i.e create 'the complement'.
  # intervals are not allowed to overlap within a day
  # parameter defining the day: min = 7200L, max = 93600L
  
  # if(!x[, (tmax - tmin) == sum(end - start + 1)]) {
    x <- rbindlist(list(x, x[NA][, start := tmax]))
    x[, start0 := shift(end, fill = tmin - 1L) + 1L]
    x[start - start0 > 0L, .(start = start0, end = start - 1L)]
  # }
}

gap.fill.tmb <- function(tband, tmin = 7200L, tmax = 93600L) {

  gap <- gap.find(tband) 
  # if no gaps, gap is an empty data.table of 0 rows and 2 cols 'start', 'end'
  
  tband <- rbindlist(list(tband, gap), fill = TRUE)
  tband[is.na(tmb), tmb := paste0(s2t(start),'-',s2t(end))]
  
  # check completeness
  tband[, tmb.dur := end - start + 1] # give also gaps a duration
  stopifnot( tband[, .(dur=sum(tmb.dur))][, all(dur==(tmax-tmin))] )
  tband[]
}


gap.fill.prg <- function(prog, tmin=7200L, tmax=93600L, h24=tmax-tmin)
  {
  # prog is the program log. Sequence summaries must be filtered out before.
  # Overlapping programs are not allowed, within day and channel.
  # Half of channels provide complete program logs. In all other casese there 
  # are gaps, i.e. not ever second of the day is labeled with a program. 
  # to merge programs with viewing using overlap() it is much easier if all 
  # program logs are complete. Hence, this function completes incomplete program
  # logs by adding the missing gaps, i.e. adding the undefined seconds.
  # kantar day-time in seconds: min = 7200, max = 93600, 24 hours = 86400

  setkeyv(prog, c('day','chn','start','end')) # need chronolgical order
  
  # a program log is complete if all programs add up to a full day
  dursum <- prog[, .(dur = sum(end - start + 1)), k = c('day','chn')] # dursum[dur > h24]
  if(dursum[, any(dur > h24)]) warning('program duration exceeds 24 hour')
  
  # check overlaps between programs
  tmp <- prog[, .SD[start - shift(end, fill = tmin-1L) < 1L], k=.(day,chn)]
  if(nrow(tmp) > 0L){
    warning('there are overlapping programs')
    print('the overlapping programs:')
    print(setorder(tmp, day, chn, prg, start))
  } 
  
  # get gaps, it is faster only to treate channels with gaps
  gap <- prog[dursum[dur < h24, -'dur'], on=c('day','chn')][
         , gap.find(.SD), .SDcols=c('start','end'), k=.(day,chn)]
  prog <- rbindlist(list(prog, gap), fill = TRUE)
  
  # check completeness
  prog[, prg.dur := end - start + 1] # give also gaps a duration
  stopifnot( prog[, .(dur=sum(prg.dur)), k=.(day,chn)][, all(dur==h24)] )
  prog[]
}
  

# join.prg <- function(view, prog, keys=c('day','virt')) {
#   
#   # this is a wraper function for calling overlap() to overlap join programs
#   # and viewing.
#   # if there are gaps in prog, the total viewing duration will be less than in
#   # the original viewing data. This makes filling gaps before joining so 
#   # important.
#   # if there are no gaps in prog, i.e. every second of the day is covered by any
#   # interval in prog, than the total viewing duration remains unchanged after 
#   # joining program logs to viewing data
#   # 
#   # only true duration (end-start+1) will be the same but not (end-start) !!
#   #
#   # keys is a parameter because in one project we joined prog from one single day
#   # to viewing across many days, so keys='virt' and dropping 'day' was a stright
#   # foreward solution
#   #
#   # the many column renaming look somewhat cumbersome but it gives clearity in 
#   # the dataset without performance issues. overlaps() returns all columns
#   # y.start, y.end, x.start, x.end, start, end providing full flexibility if 
#   # called outside the import() pipeline
# 
#   col <- names(view)
# 
#   dur.before <- view[, sum(end - start + 1)]
#   view <- overlap(view, fillgaps.prg(prog), keys)
#   dur.after <- view[, sum(end - start + 1)]
#   stopifnot(identical(dur.after, dur.before)) 
#   
#   setnames(view, c('y.start','y.end'), c('prg.start','prg.end'))
#   view[, `:=` (x.start=start, x.end=end)][, c('start','end') := NULL]
#   setnames(view, c('x.start','x.end'), c('start','end'))
#   setcolorder(view,  c(col, setdiff(names(view), col)))
#   ordercol(view, 'dur', 'end')
# }


# join.tmb <- function(view, tmb, keys=NULL){
#   
#   col <- names(view)
#   
#   dur.before <- view[, sum(end - start + 1)]
#   view <- overlap(view, tmb, keys)
#   dur.after <- view[, sum(end - start + 1)]
#   stopifnot(identical(dur.after, dur.before)) 
#   
#   setnames(view, c('y.start','y.end'), c('tmb.start','tmb.end'))
#   view[, `:=` (x.start=start, x.end=end)][, c('start','end') := NULL]
#   setnames(view, c('x.start','x.end'), c('start','end'))
#   setcolorder(view,  c(col, setdiff(names(view), col)))
#   ordercol(view, 'dur', 'end')
# 
# }

#' @export
overlap.join <- function(x, y, type, y.complete = TRUE){
  # x <- view
  # y <- ad
  # type = "prg"
  # y.complete = TRUE
  
  cols <- names(x)
  keys <- switch(type, prg = c('day','chn'), tmb = NULL)
  fillgaps <- switch(type, prg = gap.fill.prg, tmb = gap.fill.tmb)
  
  if(y.complete){
    dur.before <- x[, sum(end - start + 1)]
    x <- overlap(x, fillgaps(y), keys, nomatch=NA)
    dur.after <- x[, sum(end - start + 1)]
    stopifnot(identical(dur.after, dur.before)) 
  } else {
    x <- overlap(x, fillgaps(y), keys, nomatch=NA)
  }

  setnames(x, c('y.start','y.end'), paste0(type,'.',c('start','end')))
  x[, `:=` (x.start=start, x.end=end)][, c('start','end') := NULL]
  setnames(x, c('x.start','x.end'), c('start','end'))
  setcolorder(x,  c(cols, setdiff(names(x), cols)))
  ordercol(x,'dur','end')
  
  # now reduce again, 
  # - drop 'y.start' and 'y.end', they are stored in id$view$tmb or prog
  # - drop 'x.dur' dur of the original segments
  # important is: tmb, tmb.dur or prg, prd.dur, respectively
  x[, c("x.dur", paste0(type,'.',c('start','end'))) := NULL]
  x
}

# # --- understand overlaps -----------------------------------------------------
# 
# data <- data.table(
#   statement = 1:8,
#   virt  = as.integer(c(8001, 8001,  8123,  8123,  8123,  8777,  8777,  8303)),
#   start = as.integer(c(7200, 23456, 35667, 55763, 24098, 77098, 77070, 9983)),
#   end   = as.integer(c(8903, 23478, 36763, 55821, 26745, 77105, 77199, 12012))
#   )
# 
# interval <- data.table(
#   prg = as.integer(c(57643323, 57643324, 57643325, 57643323, 57643326, 57643327)),
#   virt  = as.integer(c(8001, 8001,  8123,  8123,  8777,  8976)),
#   start = as.integer(c(8110, 23000, 36000, 24500, 77098, 50000)),
#   end   = as.integer(c(9000, 24000, 56000, 25500, 77199, 60000))
# )
# 
# data[, dur.data := end - start + 1]
# interval[, dur.intr := end - start + 1]
# overlap(data, interval, c('start','end'))

# --- understand fill gaps ----------------------------------------------------
# 
# test <- data.table(
#   day = '2016-09-01', 
#   virt = 8004L, 
#   prg   = as.integer(c(57643323, 57643324, 57643325, 57643323, 57643326)),
#   start = t2s(c('02:05:00', '02:09:33', '06:10:00', '15:05:01', '18:09:24')),
#   end   = t2s(c('02:09:32', '02:15:32', '08:10:00', '18:09:23', '22:45:00'))
# )
# 
# # or:
# # test <- id.seq[day=='2016-09-01' & virt == 8004L]
# 
# tmp <- rbindlist(list(test, test[NA][, start := 93600L]))
# tmp[, start0 := shift(end) + 1L][1L, start0 := 7200L]
# tmp[, dur := start - start0]
# new <- tmp[dur > 0L, .(start = start0, end = start - 1L)]
# ans <- rbindlist(list(test, new[, c('day','virt') := .('2016-09-01',8004L)]), 
#         fill = TRUE)
# ans <- ans[order(start)]
# ans[, sum(end - start + 1L)]  # sum(93599L - 7200L + 1)
#
# -----------------------------------------------------------------------------
