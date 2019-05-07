
#' Reads fixed width files
#' 
#' A simple wrapper for the read::read_fwf function
#' vignette("locales")
#' 
#' reading only the necessary columns is much faster than reading the wihole 
#' file. This is especially true for dem-file with over 200 columns but also for
#' STD file wich 50 colums
#' 
#' Providing column types gives a significant performance boost.
#' 
#' There are different ways to select columns. read_fwf is not very well 
#' documented. If start and end is better than withds and the two hav no 
#' performance difference anyway, internally the columns are read with begin 
#' and end indices anyway: fwf_widths(c(3,5,7))
#' 
#' Providing column names makes it more flexible to selct columns and has no
#' performance impact
#' 
#' performance tests revealed nor calling any function in read is faster
#' especially locale() is slow
#' 
#' Weardly that was nut true for calling cat(), printing each filename to the 
#' console bade the reading a bit faster
#' 
#' @rdname read
#' @export
#' 
read <- function(file, fwf, quiet = FALSE) 
{
  if(!quiet) on.exit(cat('\r', file2date(file)))
  read_fwf(file, fwf$pos, fwf$typ, fwf$loc, na = "", progress = FALSE)
}


#' @rdname read
#' @export
#' 
fwf <- function(d, cols = NULL, enc = "UTF-8", pin = TRUE)
{
  if(pin) d <- d[2L, c('name','start') := .('pin', 1L)][-1L]
  if(length(cols)) {
    miss <- setdiff(cols, d$name)
    if(length(miss)) stop('in tv::read.fields() variables ',miss,' not found')
    d[cols, on = 'name', select := 1L]
  }
  d <- d[(as.logical(select))]
  list(
    pos = fwf_positions(d$start, d$end, d$name),
    typ = paste(substr(d$type, 1L, 1L), collapse = ""),
    loc = if(enc == "UTF-8") default_locale() else locale(encoding = "latin1")
  )
}
