
#' Reads the .swd-file
#'
#' The .swd file contains the time shifted viewing for a given day.
#' 
#' The time shifted viewing is stored in ".swd" files. For each day there is a 
#' separate file. The files are of fixed-with type, with the following structure
#' 
#'  field	 name                  label	    start	 end  widths	type
#'  1	     household	           hh	        1	     7	  7	      integer
#'  2	     individuum	           ind	      8	     9	  2	      integer
#'  3	     station	             sta	      10	   13	  4	      integer
#'  4	     recording start time	 start	    14	   19	  6	      integer
#'  5	     recording end time	   end	      20	   25	  6	      integer
#'  6	     set	                 set	      26	   26	  1	      character
#'  7	     activity	             act	      27	   27	  1	      character
#'  8	     platform	             plt	      28	   28	  1	      integer
#'  9       recording date        date.live  29     36   8       integer
#'  10      viewing start time    start.tsv  37     42   6       integer
#'  11      viewing end time      ""         43     48   6       integer
#'  12      speed                 ""         49     50   2       integer

#' From 2013-01-01 to 2015-11-23 the ".swd" files have 12 fields (50 characters).
#' After the '2015-11-23' the files only contain fields 1 to 10 (42 characters). 
#' Fields 11 and 12 apparently are redundant information and have actually never
#' been used by Kantar. Simply read only fields 1 to 10 for all cases.
#' 
#' The ".swd" file structure file is very similar to the ".swo" (live) files structure,  except the additional fields 9 and 10.
#' The first two fields can be read as one single field representing "pin" 
#' ("personal identification number"). The hh number is rarely used but can 
#' easaly be restored be e.g. as.integer(pin / 100L). Fields with hexadecimal 
#' coding have to be read as class character (see labels).
#' Each tsv file contains all tsv-viewing at one particular day. To add tsv +7 to
#' the live viewing at a particular day we have to load 8 tsv files, the one with 
#' the same day as live plus the following 7 days. The field 9 gives the date at
#' which the content was first braodcastet. This allows to match tsv viewing to 
#' the corresponding live day. So, from all tsv files loaded we're only 
#' interested in those rows with the date.live matching the date of the live file.
#' Even more surprising, start.tsv, not start, is the start time of 
#' watching. Start represents the start time when the content was broadcasted. 
#' has to be later (start - start.tsv) than the corresponding  start.
#' There are two ways to look at tsv viewing. "overnight 0-7" flags the viewing
#' by the days past since live broadcasting, while "tsv 0-7" flags the viewing by
#' 24-hours shifts since live broadcasting. The "overnight 0-7" label is given by
#' the difference between date.tsv (the date of the file) and date.live. The 
#' "tsv 0-7" labels have to be calculated by the timedifference in seconds between
#' start of watching and start of live broadcasting.
#' @export

read.tsv <- function(id) {
  
  file <- id$lab$day$tsv[(exist), paste0(id$path$data$local, file)]
  if(!length(file)) return()
  
  fwf <- fwf(id$file$tsv)
  tsv <- rbindlist(lapply(file, read, fwf), idcol = 'daytsv')
  cat('\n')
  
  # label
  tsv[, start := id$lab$time[tsv, on=c(id='start'), label]]
  tsv[, end := id$lab$time[tsv, on=c(id='end'), label]]
  tsv[, starttsv := id$lab$time[tsv, on=c(id='starttsv'), label]]
  tsv[, set := id$lab$set[tsv, on=c(asci='set'), id]]
  tsv[, daytsv := id$lab$day$tsv[tsv, on='daytsv', label]]
  tsv[, daylive := id$lab$day$tsvlive[tsv, on='daylive', label]]

  # tsv levels 0-7: 'overnight' (calender day) or 'time shift' (+24h shift)
  if(id$view$tsv.cat == 'none') tsv[, act := 'tsv'] else {
    # tsv[, shift.day := as.numeric(as.Date(daytsv) - as.Date(daylive))]
    # as.Date is very slow. Use lubridate or lookup table: 
    id.day <- tsv[, .N, key=.(daytsv, daylive)] 
    id.day[, shiftday := as.integer(as.Date(daytsv) - as.Date(daylive))]
    switch(id$view$tsv.cat,
      overnight = { 
        id.day[, label :=  paste0('overnight', shiftday)]
        tsv[, act := id.day[tsv, on=c('daytsv','daylive'), label]] 
        },
      tsv = {
        # calculate shift by 24 hours between live broadcast and actual viewing: 
        # tsv0 to tsv7
        tsv[, shiftday := id.day[tsv, on=c('daytsv','daylive'), shiftday]]
        t24 <- as.integer(60*60*24)
        tsv[, shiftsec := shiftday * t24 + (starttsv - start)] # the tricky part
        tsv[, tsv := (!shiftday == 0L) * as.integer(shiftsec / t24 + 1L)]
        # "tsv 8" appears in rare cases:
        if(tsv[, any(tsv > 7L)]) {
          warning('There are "tsv8" statements found. Include to macht Instar "tsv gesamt", exclude to match Instar "tsv categories".')
          # tsv <- tsv[tsv <= 7L]  # Excluding Will match with Instar "tsv categories"
          # tsv[tsv > 7L, tsv := 7L] # Including will match with Instar "tsv gesamt"
          # keeping and labelling "tsv8" is probably the most transarent way to handle this dilemma
        }
        tsv[, act := paste0('tsv', tsv)]
       }
      )
    }
  # prepare tsv to merge with live
  cols <- c('daytsv','starttsv','shiftday','shiftsec','tsv')
  if(id$view$tsv.ref) cols <- cols[-(1:2)]
  cols <- intersect(cols, names(tsv))
  if(length(cols) > 0L) tsv[, (cols) := NULL]
  setnames(tsv, 'daylive', 'day')
  # return only tsv at live day
  tsv[id$lab$day$live$label, on='day'][]
}


# tsv[, c('shiftday','shiftsec','tsv') := NULL]
# x <- tsv[daylive=='2015-01-05' & pin==23701L]
# 
# tsv[act=='tsv0' & (start.tsv - start == 0L)]


# # understand tsv0-7 categories:
#
# # which is start.tsv and start.live? check: shift.s has to be positiv number
# # look at shift.s in one example for a given tsv-date and shift.d == 0
#
# x <- tsv[daytsv == '2015-09-06'][c(6005,3005)]
# x[,.(start=s2t(start,'t'),daylive,starttsv=s2t(starttsv,'t'),shiftsec)]
#
# x <- tsv[datetsv == '2015-09-06' & start < starttsv,][c(4000,1000,10)]
# x[,.(start=s2t(start,'t'),daylive,starttsv=s2t(starttsv,'t'),shiftsec)]
# 
# # plot
# tsv[daylive %in% id$lab$day$label & tsv <= 7,
#     plot(start, starttsv + shiftday * t24,
#          col = tsv+1, ylim = c(0,(t24+tmin)*(max(shiftday)+1)), 
#          frame = FALSE, axes = FALSE)]
# for(i in 1:8) abline(t24*(i-1),1, col=c(2:8,8)[i])
# for(i in 1:9) abline(h=(t24)*(i-1)+tmin)
# tsv[tsv > 7, points(jitter(start,5), starttsv + shiftday * t24, col = tsv+1)]
# abline(v = tmin)
