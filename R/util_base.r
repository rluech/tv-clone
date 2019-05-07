
#' Utility functions in base-R
#'
#' Utility functions in base-R
#'
#' @details
#' \code{na.locf} repeats the last non NA value. Keeps leading NA. This algorith 
#' is generally called \emph{last observation carried forward (LOCF)}.
#' make sure NAs are really NAs, e.g. use to.na = c(""," ") to convert to NAs
#' to.na = NULL is ~100 times faster
#' 
#' \code{quiet} captures cat() and print(), e.g.: run import() silently.
#' 
#' \code{to.pdf} takes an expression like to.pdf(myplotfun()) and creates a PDF.
#'  This is convenient because otherwise you have to \code{pdf(); ...code...; dev.off();}
#' 
#' @examples 
#' df <- data.frame(V1 = c(NA, 'hello', NA, NA, NA, 'world', NA, NA), stringsAsFactors = FALSE)
#' df$V2 <- na.locf(df$V1)
#' df
#' 
#' df <- data.frame(V1 = c(0, 3, 0, 0, 0, 6, 0, 0), stringsAsFactors = FALSE)
#' df$V2 <- na.locf(df$V1, to.na = 0)
#' df
#' 
#' cat('hello world')
#' quiet(cat('hello world')) 
#' 
#' print('hello world')
#' quiet(print('hello world')) 
#' 
#' quiet(message('hello world')) # but
#' 
#' plot01 <- function() {
#'  par(mfrow = c(1,2))
#'  plot(1:100, sin(1:100), type = 'b', main = 'sine')
#'  plot(1:100, tan(1:100), type = 'b', main = 'tangent')
#' }
#' file = tempfile(fileext = ".pdf")
#' to.pdf(plot01(), file = file, width = 10, height = 7.5)
#' system(paste0('open "', file, '"'))
#' 
#' @rdname util_base
#' @export
#' 
na.locf <- function(x, to.na = NULL) 
{ 
  if(!is.null(to.na)) x[x %in% to.na] <- NA 
  ind <- which(!is.na(x))
  if(is.na(x[1L])) ind <- c(1L,ind)
  rep(x[ind], times = diff(c(ind, length(x) + 1L)))
}

#' @rdname util_base
#' @export
#' 
quiet <- function(x) 
{ 
  sink(tempfile())
  on.exit(sink())
  invisible(force(x))
}

#' @rdname util_base
#' @export
#' 
to.pdf <- function(expr, filename, ..., verbose = TRUE) 
{ 
  # wide format: width = 10, height = 7.5
  if(verbose) cat(sprintf("Creating %s\n", filename)) 
  pdf(filename, ...) 
  on.exit(dev.off()) 
  eval.parent(substitute(expr)) 
}

