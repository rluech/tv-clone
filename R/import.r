
# import kantar tv pin data: demographics, viewing(live, tsv), program logs
#' @export

import <- function(id)
{
  # demographics
  dem <- if(id$read["dem"]) read.dem(id) else NULL
  if(!is.null(dem) && !is.null(id$view) && id$view$obs == 'hh') 
    dem <- dem[.(1), on='hw'][, pin := as.integer(pin/100)]
  
  # calcuate universe
  if(!is.null(dem) && id$dem$uni)
    calc.uni(dem, target = setdiff(id$dem$var, c("pin","weight","guest")))
  
  # viewing 
  view <- if(sum(id$read[c("live","tsv")]) == 0) NULL else
    rbindlist(list(
      if(id$read["live"]) read.live(id), 
      if(id$read["tsv"]) read.tsv(id)
    ), fill = TRUE)
  # rbindlist returns not NULL but a Null data.table
  if(!is.null(view) && ncol(view) == 0) view <- NULL 
  
  if(!is.null(view) & !is.null(id$view)) {
    # total tv
    cols <- c('chn','chn.name','ttv') #[-2]
    view[, (cols) := id$lab$sta[view, on=c(id='base'), mget(cols)]]
    ordercol(view, cols, 'base')
    if(id$view$ttv) view <- view[.(1), on='ttv'][, ttv := NULL][]
    
    # filter plt (if not, household aggregation will be incorrect)
    if(!all(id$lab$plt$label %in% id$view$plt)) {
      view[, plt := id$lab$plt[view, on = "plt", label]]
      view <- view[id$view$plt, on = 'plt']
    }
    
    # houshold level
    if(id$view$obs == 'hh') view <- household(view)
    
    # duration
    view[, dur := end - start + 1] # +1 because time is zero based, see time.r
    ordercol(view,'dur','end')
    
    # timeband
    if(!'wholeday' %in% id$view$tmb$tmb)
      view <- overlap.join(view, id$view$tmb, type = 'tmb')
  }
  
  # programs
  prog <- if(id$read["prg"]) read.prg(id) else NULL
  
  # join prog and view
  if(!is.null(prog) && !is.null(view) && id$prg$join) 
    view <- overlap.join(view, prog, type='prg')

  # join dem and view
  if(!is.null(dem) && !is.null(view) && id$dem$join) {
    view <- view[dem, on=c('day','pin')]
    ordercol(view, names(dem))
    }
    
  # return
  for(i in c('dem','view','prog')) 
    if(!is.null(get(i))) assign(i, get(i), envir = .GlobalEnv)
  return(invisible(list(dem = dem, view = view, prog = prog)))
  
}

