
#' utility functions for R data.table
#' 

#' @rdname util_dt
#' @export
#' @import data.table
#' 
na.to.0 <- function(dt, to = 0L) {
  for(j in seq_along(dt)) set(dt, which(is.na(dt[[j]])), j, to); dt
}


#' @rdname util_dt
#' @export
#' 
ordercol <- function(dt, cols, nextto = 1L) {
  # reordering columns of data.table dt. Column names in cols will be placed 
  # next to the column given by nextto. If nextto is 1 (default) cols are
  # placed at the beginning, if 0 placed at the end
  nm <- names(dt)
  x <- match(cols, nm)
  y <- if(nextto==1) {
    NULL 
  } else if(nextto==0) {
    setdiff(seq_along(nm), x)
  } else {
    setdiff(seq(match(nextto, nm)), x)
  }
  z <- setdiff(seq_along(nm), c(y,x))
  setcolorder(dt, c(y, x, z))
}


#' @rdname util_dt
#' @export
#' 
id.dt <- function(label, id.name='id', id=NULL, label.name='label', roll=NULL) {
  # creates a keyed data.table, often used as lookup table for labels
  # parameter label and id.name come first because they are most often the input:
  # standard use: id.dt(date, 'day')
  if(is.null(label)) { 
    id <- integer()
    label <- character()
  } else if(is.null(id)) {
    id <- seq_along(label)
  }
  dt <- setnames(data.table(id, label), c(id.name, label.name))
  if(!is.null(roll)) dt[, roll := roll] 
  setkeyv(dt, id.name)[]
}


#' @rdname util_dt
#' @export
#' 
id.dt.cross <- function(x) { # x <- id$label[c('hw','sg')]
  # create new variable labels as the result of crossing two or more variable 
  # labels. x is a list of the labels of the variables to be corossed. 
  # Each label is a data.table as returned by id.dt() containing value and
  # labels of a variable in column 1 and 2, respectively. The order
  # in x matters. The labels get pasted in reverse.
  val <- expand.grid(lapply(x, `[[`, 1L)) # multiple value columns
  lab <- expand.grid(lapply(x, `[[`, 2L)) # multiple label columns pasting into one
  dt <- data.table(val, label = do.call(paste, c(rev(lab), sep='_')))[, id:=.I]
  setkey(setcolorder(dt, ncol(dt):1L), 'id')[]
}

