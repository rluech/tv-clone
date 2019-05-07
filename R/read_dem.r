
#' Reads the .dem-file
#'
#' The dem file contains the socio-demografic information of the panelists at a 
#' given day. 
#' 
#' There are about 4500 rows in one file for the roughly 4500 panelists per day.
#' The panel member id is column 'pin'. 'pin' in fact is the combination of the
#' household id and the individual id. Most panelmembers will occure the 
#' following days but not all, a household can drop out of the panel for 
#' different reasons temporarily or for ever. Not all Panelmember are watching TV
#' on a given day, hence not all panelmember will appera in the viewing files.
#' 
#' There are about 200 columns but the number of columns is in fact growing 
#' constantly, hence the shape of the dem file is changing over time. See below.
#' 
#' The most importannt columns are the first ones: day, pin, weight, guest
#' These four columns are read by default. Any additional column has to be 
#' specified by its name to be read. The exact name of each column can be 
#' retrieved from the 'name' column in the 'file_dem' table.
#' 
#' The 'dem' file, like all Kantar Media TV rawdata is stored as flat text fixed
#' with files. The table 'file_dem' gives the details on how the file has to be read.
#' The .dem file is read by using 'read_fwf' from the 'readr' package. read_fwf() 
#' apparently is very robust. It still reads the file correctly even if the last
#' fields are missing. It returns the column as NA with a warning.
#' So it is ok to request a variable (say, "upc-abonnement") that does not exist
#' in the dem file on that particular day. When combining all requested daily 
#' files into one data.table dem using rbindlist(..., fill = TRUE), that column 
#' with the fully or patially missing data for some days is represented by NAs.
#' Therefore it is not necessary to know the number of fields for a particular 
#' day.
#' 
#' Nevertheless, a function returning the number of fields would look like this:
#' nvar <- function(x) { # use like: n <- nvar(date)
#'   id.n <- c(
#'     '2013-01-01'=107, '2014-02-27'=108, '2015-02-05'=157, '2015-07-01'=158, 
#'     '2016-01-01'=159, '2016-02-02'=181, '2016-07-01'=182, '2016-12-05'=186
#'     )
#'   id.n[findInterval(as.Date(x), as.Date(names(id.n)))] 
#' }
#' 
#' Other than that, there should never be NAs in dem and it would possibly mean
#' a fatal error in the data production by Kantar. Such incindets have occured
#' in the past. An NAs is a empty character field "" in the rawdata. It is not 
#' trivial to verify in Excel for example that a NA is truly an emply field 
#' as the can be non-printable ascii character at least for 'age'.
#' However ESXCEL::=ISTLEER() proofs there are indeed missing values, e.g. there
#' is NA in age found during April 2013! 
#' 
#' age        day N
#' 1:  NA 2013-04-08 1
#' 2:  NA 2013-04-10 1
#' 3:  NA 2013-04-13 1
#' 4:  NA 2013-04-20 1
#' 5:  NA 2013-04-23 1
#' 6:  NA 2013-04-24 1
#' 7:  NA 2013-04-27 1
#' 
#' All columns but the first three are stored by a field width of 1. Using a the
#' extended ascii character set a length one filed can represent a variable with
#' more than 100 levels, such as 'kanton' or 'age'. All columns get converted 
#' to integer. Exceptions are day (character), weigth (numeric), guest (logical).
#' 
#' Every column has labels for its corresponding values, like factor levels. The
#' labels are stored in id$lab and each column in dem has its corresponding 
#' element of the same name. 
#' 
#' @return A data.table. All column are of type integer but day (chr), weight (num) 
#' and guest (log)
#' @export

read.dem <- function(id) 
{
  file <- id$lab$day$dem[(exist), paste0(id$path$data$local, file)]
  if(!length(file)) return()
  
  fwf <- fwf(id$file$dem, id$dem$var)
  dem <- rbindlist(lapply(file, read, fwf), idcol = 'day', fill = TRUE)
  cat('\n')
  
  dem[, day := id$lab$day$dem[dem, on='day', label]]
  dem[, guest := id$lab$guest[dem, on=c(id.original='guest'), 'guest']]
  # guest are NA in hw, make them non-hw
  if('hw' %in% names(dem)) dem[.(TRUE), on='guest', hw := 2L]
  # all ascii character to integer
  if('age' %in% names(dem)) {
    id.age <- dem[, .N, k = age][, value := asci2i(age)]
    dem[, age := id.age[dem, on='age', value]]
  }
  asci <- intersect(id$file$dem['character', on='type', name], names(dem))
  asci <- setdiff(asci, 'age')
  if(length(asci)) 
    for(j in asci) dem[, (j) := id$lab[[j]][dem, on=c('asci'=j), j, with=FALSE]]
  dem
}

