
#' Reads the .swo-file
#'
#' The .swo file contains the live viewing for a given day. 
#' 
#' The live viewing is stored in ".swo" files. For each day there is a separate 
#' file. The files are of fixed-with type, with the following structure
#' 
#'  field	 name        label	 start	end	 widths	 type
#'  1	     household	 hh	     1	    7	   7	     integer
#'  2	     individuum	 ind	   8	    9	   2	     integer
#'  3	     station	   sta	   10	    13	 4	     integer
#'  4	     start	     start	 14	    19	 6	     integer
#'  5	     end	       end	   20	    25	 6	     integer
#'  6	     set	       set	   26	    26	 1	     character
#'  7	     activity	   act	   27	    27	 1	     character
#'  8	     platform	   plt	   28	    28	 1	     integer
#'
#' The first two fields can be read as one single field, the hh number is rarely 
#' used but can easely be recreated as.integer(pin / 100L). The fields with 
#' hexadecimal coding have to be read as character.
#' 
#' # filter activity
#' The currency includes: 'live','tsv' excludes: 'teletext','recordedview'
#' 'recordedview' is the same viewing as 'tsv', so the viewing is duplicated
#' 'tsv' is referenced to the program broadcasting time, 'recordedview' to the 
#' time of consumption by the viewer.
#' @export

read.live <- function(id) 
{ 
  file <- id$lab$day$live[(exist), paste0(id$path$data$local, file)]
  if(!length(file)) return()
  
  fwf <- fwf(id$file$live)
  view <- rbindlist(lapply(file, read, fwf), idcol = 'day')
  cat('\n')
  
  view[, act := id$lab$act[view, on='act', label]]
  x <- intersect(id$view$act, c('live','recordedview','teletext')) 
  view <- view[x, on='act'] # 'tsv' in filt would produce NA
  view[, start := id$lab$time[view, on=c(id='start'), label]]
  view[, end := id$lab$time[view, on=c(id='end'), label]]
  view[, day := id$lab$day$live[view, on='day', label]]
  view[, set := id$lab$set[view, on=c(asci='set'), id]][]
}
