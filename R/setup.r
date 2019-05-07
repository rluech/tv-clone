
#' Read Kantar TV-Raw-Data
#'
#' This function is the interface and starting point to read \strong{Kantar TV-Raw-Data} 
#' (sometimes called \emph{PIN-Data}). The function collects parameters on how 
#' rawdata shall be imported and later analyzed, much like the interface of 
#' \strong{Instar Analytics}. All parameters, together with more information about
#' the tv rawdata, is returned as a list \code{id}. This list is input to most 
#' functions of the \code{tv} package.
#' 
#' @param day A character vector. Can also be of class 'Date' (POSIX). Specifies
#'  the days to import. The only excepted format is the ISO standard format, 
#'  e.g.: \code{'2013-12-31'}. There are 3 ways to specify the days to import, 
#'  see details. Also see subfunction \code{id.day}.
#' 
#' @param to Either a (single) numeric or character value. Default 
#' is \code{1}. If \code{day} is of length one, and \code{to} is numeric, than 
#' \code{to} is taken as the desired length of the sequence. If the value is 
#' negative, the sequence runs into the past of the reference in \code{day}. If
#' \code{day} is of length one, and \code{to} is a single date value, \code{to}
#' is the desired end date of the sequence. If \code{day} is not of length one 
#' \code{to} is ignored.
#' 
#' @param obs Either 'ind' (default) or 'hh'. The level of observation, either 
#' the data is on individuals level or aggregated to household level. If 'hh', 
#' \code{dem} only the "housewifes" of each household is returned. In \code{view} 
#' the viewing statements are aggregated within each household by means of the 
#' subfunction \code{household}. 'hh' is still experimental and does not match 
#' in all cases with Instar Analytics.
#' 
#' @param hh.calc Either \code{'normal'} (default) or \code{'by channel'}. The type of 
#' household aggregation. If calculation TV Total Instar Analytics uses 'normal'.
#' If calculating facts by channel the choice is 'by channel'. The former is
#' known to return restults that match exactly with Instar, the latter not. 
#' For more dertails see \code{?household}.
#' 
#' @param guest \code{TRUE} (default) or \code{FALSE}. Only used if 
#' \code{obs = 'hh'}. For the household aggregation algorithm it is nescessary to
#' know if guest should be excluded or not. Ignored otherwise. To calculate facts
#' without guests overwrite \code{dem <- dem[!(guest)]} after importing. 
#' Re-calculate sample and universe \code{calc.uni(dem)}.
#' 
#' @param dem \code{TRUE} (default) or \code{FALSE}. Read demografics? In cases 
#' \code{dem} is not needed, setting \code{dem = FALSE} of course will speed up 
#' the whole reading process.
#' 
#' @param dem.var Character vector specifiying the additional variables of the 
#' demorgaphics file to be imported. By default \code{dem} contains columns 
#' \code{day,pin,weight,guest,hw}. But the Kantar rawfiles .dem contains about 
#' 240 variables. Reading a small subset is much faster and gives a better 
#' overview. If other variables are of interest they have to specifies here by 
#' their correct name. The names of all variables in the rawfile is found 
#' here: \code{id$file$dem$name} and more information here \code{id$file$dem}. 
#' The names are not the sam as in Instar Analytics but shorter better suited 
#' for interactive programming.
#' 
#' @param dem.day For the \code{dem}-file a date different to those specified 
#' in in \code{day} can be supplied. For example, if the panel is fixed to a 
#' specific sample day and their viewing in a period before and after that day 
#' is evaluated. This procedure is known from BARB. The advantage is that each 
#' person has one single weight and all together the weigths are congruent to 
#' the population. The same is achieved by filtering \code{dem} after 
#' importing of course but unnescessary \code{dem}-files will be read.
#' 
#' @param dem.uni \code{TRUE} (default) or \code{FALSE}. Should \code{sample} and 
#' \code{universe} be calculated? See subfunction \code{calc.uni}. All demografic 
#' variables specified in dem.var above will be used as tagret group. This is 
#' not always intended. It is recommended to calculate \code{sample} and 
#' \code{universe} after importing and specifying target groups explicitly,
#'  e.g.: \code{calc.uni(dem, target = c('sg','ageclass'))}. See examples.
#'  
#' @param dem.join \code{TRUE} or \code{FALSE} (default). Should \code{dem} and 
#' \code{view} be joined? This means: \code{view <- view[dem, on = c('day','pin')]}.
#' Only use this if you know the import is exactly as you intend it to be and 
#' you're sure to apply \code{calc(view)} directly after import. Usually it makes more
#' sense to join \code{dem} and \code{view} after importing, and first make sure
#' \code{sample} and \code{universe} are correct or filter \code{guests}, etc.
#' 
#' @param view \code{TRUE} (default) or \code{FALSE}. Read viewing? In cases 
#' \code{view} is not needed, setting \code{view = FALSE} of course will speed up 
#' the whole reading process.
#' 
#' @param act A character vector. Like in Instar Analytics possible values are 
#' \code{live, tsv, recordedview, teletext}. Default is \code{c('live','tsv')} 
#' and stands for \emph{Live} and \emph{time-shifted} viewing, together they 
#' yeld default currency \emph{Total-TV} as returned by Instar Analytics. 
#' \code{recordedview} is not part of \emph{Total-TV}, but a different 
#' representation of \code{tsv}. \code{teletext} is ot part of the currency.
#' 
#' @param plt A character vector. Possible values are found here: id$lab$plt$name
#' Default is to use all. \code{plt} stands for \emph{platform} and is also a 
#' column in \code{view}. Dropping any intems here from the list will filter 
#' view and only return the viewing statements (rows) in view recorded on the 
#' corresponding \emph{platform}. Of cource, plt in view can be filtered after 
#' importing, but to yeld household aggregation like Instar Analytics, the 
#' filtering has to be appied before the household aggregation algorithm.
#' 
#' @param tsv.cat A character vector. Possible values are \code{tsv, overnight, none} 
#' (last one is default). \code{tsv.cat} stand for time-shifted categories which
#' are also found in Instar Analytics. \code{tsv} labels each viewing statement \code{tsv0, 
#' tsv1, ..., tsv7} according to the time past relative to the live broadcasting, 
#' in 24-h-steps. \code{overnight} returns labels \code{overnight0, overnight1, 
#' ..., overnight7}, according to the past number of calendar days relative to 
#' live broadcasting. \code{none} does nothing, time-shifted viewing 
#' (the .swd rawdata file) is simply read and together with live viewing 
#' (the .swo rawdata file) returned as a single data.table \code{view}. The two 
#' types of viewing can be identified by the column \code{act}.
#' 
#' @param tsv.ref \code{TRUE} or \code{FALSE} (default). Should the two columns 
#' of the time-shifted rawdata file \code{daytsv, starttsv} be returned in view?
#' These two columns reflect the day and time on which the tsv-viewing statement 
#' was watched. \code{tvs categories} are calculated based on this information 
#' in \code{read.tsv} but afterwards the colmns are deleted by default. Ignored 
#' if \code{tsv.cat = none}.
#' 
#' @param ttv \code{TRUE}  (default) or \code{FALSE}. \code{ttv} stands for 
#' Total-TV. Should viewing be filtered by channels that belong to the Total-TV?
#' This is also the default in Instar Analytics. If \code{FALSE} the viewing is 
#' not filtered, hence contains also viewing statements on channels that are 
#' excluded from the standard currency.
#' 
#' @param tmb A list of time bands of the form: \code{list('wholeday' = c(start='02:00:00', end='25:59:59'))}. 
#' Multiple time bands can be specified. Each list element represents a time 
#' band. Its start and end times are given as a length two character vector. 
#' The expected time format is "hh:mm:ss". Note that end is -1 second. If the vector is named, this name 
#' will be use as label in the column tmb, otherwise a name will be automatically 
#' created based on its start and end times. Timebands are not allowed to 
#' temporally overlap and a error will be thrown. If the time bands do not cover 
#' the whole 24 hours, time band in between will be produced automaticlly, 
#' resulting in a list of timebands that always cover each second of the 24 hours.
#' This guarantees that the sum of viewing is always the same if timeband were 
#' specified or not. The timebands of interest can by subsetted.
#' Specifying time band(s) means the viewing statements in \code{view} will be 
#' matched against all timebands (called an \emph{overlap join}, see ?foverlaps) 
#' and the overlapping viewing statements will be cropped to the overlapping 
#' time interval. specifying time bands will always result in more viewing 
#' statements (rows) in view but the sum of viewing remains unchanged.
#'
#' @param prg \code{TRUE} (default) or \code{FALSE}. Read program logs? In cases 
#' \code{prog} is not needed, setting \code{prg = FALSE} of course will speed up 
#' the whole reading process. Note that programs are abbreviated \code{prg} in parameter
#' and column names but the data.table is named \code{prog}. This was nescessary due to
#' scopeing interferance in data.table if the data.table and one of its columns
#' share the same name.
#' 
#' @param prg.seq Either \code{'gross'} or \code{'net'} (default). Programs that 
#' were aired with advertisement breaks in between have multiple records 
#' (retaining the same program-ID) in the program logs. Next to the separate \code{'net'}
#' sequences there is an additional entry spanning the total \code{'gross'} timerange 
#' including ad breaks. To calculate facts by programs, the standard is to use 
#' net program duration.
#' 
#' @param prg.join \code{TRUE} or \code{FALSE} (default). Should \code{prog} and 
#' \code{view} be joined? This means: \code{view <- overlap.join(view, prog, type='prg')},
#' see \code{import}.
#' Only use this if you know the import of view and prog is as intended and you
#' are sure to apply \code{calc(view, by = c('day','prg'))} directly after 
#' import.
#' 
#' @param path To specify ad hoc paths. \code{path} is a named list of paths, 
#' see \code{path()}, use the very same structure as found there.
#' 
#' @param import \code{TRUE} or \code{FALSE} (default). For convenience, start 
#' reading the files immediatly? If \code{TRUE} you'll find the four objects 
#' \code{id,dem,view,prog} in your global environment after import is finished.
#' If \code{FALSE} you only get \code{id}, and follow up by importing 
#' \code{dem,view,prog} by means of \code{import(id)}.
#' 
#' @details 
#' The default values provide the minimum on information for performance reason 
#' but enough to calculate standard estimates (facts). The default values are 
#' usually the same as the default in Instar Analytics. All parameters are 
#' optional. There are many parameters to specify the data import.
#' 
#' Dates in \code{day} do not need to be continuous or in chronological order, 
#' e.g loading weekends only is fine. Lowest possible value is \code{'2013-01-01'}. 
#' Highest possible value is yesterday's date, e.g.: \code{Sys.Date()-1)} but depends 
#' on what is found in the \code{/data} directory in \code{path}. Like in Instar
#' Analytics, Overnight +7 is only available after 8 days.
#' 
#' If some files specified to import are not found under /data, instead of 
#' breaking, the program runs with a warning listing all missing files.
#' 
#' @return A list named \code{id} containing all nescessary information to 
#' read the rawdata. \code{id} is assigned to the global environment for 
#' convenience while interactive programming. At the same time the function 
#' returns the same list in the classical way, allowing assignment 
#' (e.g.: \code{id <- setup()}). The latter is nescessary to import the data 
#' within a function call. See examples. If \code{setup(import = TRUE)} the subfunction
#' \code{import(id)} is called and consequently in addition the data.tables
#' \code{dem,view,prog} are returned. Again, to the global environment. See 
#' examples.
#' 
#' @examples
#' 
#' # calculate standard facts for 10 days:
#' library(tv)
#' setup('2018-01-01', 10, dem.join = TRUE, import = TRUE)
#' calc(view)
#' 
#' # the same, for non-interactive programming and more educative:
#' id <- setup('2018-01-01', 10)
#' data <- import(id)
#' 
#' dem  <- data$dem
#' view <- data$view
#' prog <- data$prog
#' 
#' join <- view[dem, on = c('day','pin')]
#' calc(join, by = 'day')
#' 
#' # what is meant by interactive programming?
#' ?interactive
#' 
#' # speed up import, if not all of the 3 datsets (dem, view, prg) are needed:
#' setup('2018-01-01', dem = TRUE, view = FALSE, prg = FALSE) # only dem
#' 
#' # import more demografic variables:
#' id$file$dem$name # all available variables of the dem-files
#' setup('2018-01-01', dem = TRUE, dem.var = c('sg','age','sex'), import = TRUE)
#' 
#' # create variable "ageclass" based on variable "age":
#' dem.add(dem, 'ageclass')
#' 
#' # calculate facts by target group:
#' calc.uni(dem, target = c('day','sg','ageclass','sex'))
#' join <- view[dem, on = c('day','pin')]
#' res <- calc(join, by = c('day','sg','ageclass','sex'))
#' res
#' 
#' # switch between values and labels:
#' res[, 'sg' := id$lab$sg[res, on = 'sg', label]]
#' res[, 'ageclass' := id$lab$ageclass[res, on = 'ageclass', label]]
#' res[, 'sex' := id$lab$sex[, x := c('male','female')][res, on = 'sex', x]] # customize
#' # switch back to values:
#' res[, 'sg' := id$lab$sg[res, on = c(label='sg'), sg]]
#' res[, 'ageclass' := id$lab$ageclass[res, on = c(label='ageclass'), ageclass]]
#' res[, 'sex' := id$lab$sex[res, on = c(x = 'sex'), sex]]
#' 
#' # specify date, date range
#' setup('2018.12.31') # Error with message what date format is expected
#' setup('31-12-2018') # Error
#' setup('2018-12-31') # correct
#' 
#' A. date as a vector of dates
#' setup(day = '2018-01-01', import = TRUE)
#' setup(day = c('2017-01-02','2018-01-02','2019-01-01'), import = TRUE)
#' dem[, .N, k = day] # number of rows per day
#' 
#' B. date as a continuous sequence to a reference date
#' setup(day = '2018-01-01', to = 10, import = TRUE) # the 10 following days
#' setup(day = '2018-01-01', -3, import = TRUE)      # the 3 previous days
#' 
#' C. date as a continuous sequence with start and end date
#' setup('2018-01-01', '2018-01-31', import = TRUE)
#' 
#' # add calendar variables "year" "month", "weekday", "weekend" etc.
#' dem.add(dem, 'calendar')
#' dem[, 'wend' := id$lab$wend[dem, on = 'wend', label]]
#' dem[, .N, k = .(day,wend)]
#' 
#' @export
#' @import data.table readr

setup <- function(
  
  day   = '2013-01-01',
  to    = 1,
  
  obs     = c('ind','hh')[1],
  hh.calc = c('normal','by channel')[1],
  guest   = TRUE,
  
  dem      = TRUE,
  dem.var  = NULL,
  dem.day  = NULL,
  dem.uni  = TRUE,
  dem.join = FALSE,

  view    = TRUE,
  act     = c('live','tsv','recordedview','teletext')[1:2],
  plt     = c('terA','terD','satA','satD','cabA','cabD','iptv','web','unknown'),
  tsv.cat = c('tsv','overnight','none')[3],
  tsv.ref = FALSE,
  ttv     = TRUE,
  tmb     = list('wholeday' = c(start='02:00:00', end='25:59:59')),
  
  prg      = TRUE,
  prg.seq  = c('gross','net')[2],
  prg.join = FALSE,
  
  path     = NULL,
  import   = FALSE
  
  ) {
  
  path <- if(is.null(path)) path() else path
  
  read <- c(
    dem  = isTRUE(dem), 
    live = isTRUE(view) && any(c('live','recordedview','teletext') %in% act), 
    tsv  = isTRUE(view) && 'tsv' %in% act, 
    prg  = isTRUE(prg)
    )
  
  dem <- if(!read["dem"]) NULL else 
    list(
      var   = unique(c('pin','weight','guest', if(obs=='hh') 'hw', dem.var)),
      guest = guest,
      uni   = dem.uni,
      join  = dem.join
    )

  view <- if(!any(read[c('live','tsv')])) NULL else 
    list(
      obs     = obs,
      hh.calc = hh.calc,
      guest   = guest, 
      ttv     = ttv, 
      act     = act,
      plt     = plt,
      tsv.cat = tsv.cat, 
      tsv.ref = tsv.ref, 
      tmb     = id.tmb(tmb)
    )
  
  prg <- if(!read['prg']) NULL else 
    list(
      seq  = prg.seq, 
      join = prg.join
    )

  file.dem <- file.dem(path$id)

  file <- list(
    dem  = fread(paste0(path$id,'file_dem.csv')),
    live = fread(paste0(path$id,'file_swo.csv')),
    tsv  = fread(paste0(path$id,'file_swd.csv')),
    prg  = fread(paste0(path$id,'file_std.csv'), select = 1:7)
  )
  
  lab <- c(
    list(
      day    = file.exist(id.day(day, to, dem.day), path$data$local),
      time   = id.time(),
      act    = fread(paste0(path$id,'label_act.csv')),
      plt    = fread(paste0(path$id,'label_plt.csv')),
      set    = fread(paste0(path$id,'label_set.csv')),
      sta    = fread(paste0(path$id,'label_channel.csv')),
      prgtyp = fread(paste0(path$id,'label_prgtyp.csv')),
      genre  = fread(paste0(path$id,'label_genre.csv'))
      ),
    file.dem$label
  )
  
  rim <- fread(paste0(path$id,'rim_tv.csv'))
  rim[, rim := paste0('rim_',na.locf(rim,''))]
  
  id <- list(
    path  = path,
    file  = file,
    read  = read,
    dem   = dem,
    view  = view,
    prg   = prg,
    lab   = lab,
    rim   = rim
    )
  
  # checks before start reading data
  setup.check(id)
  
  # return
  assign('id', id, envir = .GlobalEnv)
  if(import) import(id) # dem, view, prog (if not NULL) to .GlobalEnv 
  return(invisible(id)) # allows explicit assignment: id <- setup()

}

