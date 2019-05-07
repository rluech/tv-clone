
#' read instar analytics export
#' 
#' Define the Report and its Export in Instar Analytics according to these rules
#' to allow a predictable file format ready for automatic processing
#' 
#' Always display the report with only one header row. This is the 'long' format,
#' any further variable is added as rows not as columns, e.g.:
#' 
#'  Timebands  |     Dates    | Sample | Universe | AvRch(000) | ...
#' 'Whole Day' | '01.01.2017' |  1995  | 3351.017 | 2784.1940  | ...
#' 'Whole Day' | '02.01.2017' |  1995  | 3351.017 | 2784.1940  | ...
#' 'Whole Day' | '03.01.2017' |  1995  | 3351.017 | 2784.1940  | ...
#' 
#' This has to be defined 
#' in the 'period' sheet in Instar. If the job ist saved, this display formating
#' will be retained the next time the job is executed. The export table will be 
#' written the same way as the period table is formatted.
#' 
#' Make sure to not display any Totals in the report. You can disable Totals in
#' the 'Template' sheet.
#' 
#' If there are unwanted attributes in the period table select from the menue
#' report > attributes > channel to deselect columns. Only export columns that
#' you need
#' 
#' Which seperator to use should not matter when using data.table::fread(),
#' however ; or , is recommended
#' 
#' 'Print labels' is needed otherwise header as well as grouping variable  will 
#' be missing completely. Use 'repeate every row/column'. Otherwise you have to 
#' fill empty cells in R using local observation carried forward na.locf().
#' 
#' numbers should be exported with the highest possible precision which is 9 
#' decimals. This can be set in options.
#' 
#' Exporting to .txt is probably most predictable. Only Export the report, do 
#' not export Technical Card, which would append in the same file.
#' 
#' For reproducebility save the job and batch, then Technical Card is not needed
#' 
#' @return A data.table. column header should match the variable names used in 
#' the TV package. Variable values should also match thode used in the 
#' TV package, e.g in id$lab[variable].
#' @export

read.ia <- function(file) 
# file <- '~/tv/projects/2018-09-21_replicate_Instar/hh/replicate_instar_days_households.txt'
# file <- 'I:/InfoSysplus/1_IS_KMA_Live/KMA Seconds VM/Export/rlue/replicate_instar_channels_01_individuals.txt'
# path <- '~/tv/projects/2018-09-21_replicate_Instar/channels'
# file <- file.path(path,'replicate_instar_channels_2018_households.txt')
{
  ia <- fread(file)

  # --- column names ----------------------------------------------------------
  
  hd <- data.table(id = names(ia))
  
  dictionary <- c(
    'Dates'                = 'day',
    'Activities'           = 'act',
    'Timebands'            = 'tmb',
    'Channels'             = 'chn.name',
    'Channelcode'          = 'chn',
    'Channel Group'        = 'chn.grp',
    'Sample'               = 'sample',
    'Universe'             = 'universe',
    'AvRch(000)'           = 'rchT',
    'AvRch%'               = 'rchP',
    'Rtg(000)'             = 'rtgT',
    'Rtg%'                 = 'rtgP',
    'Share'                = 'share',
    'TAud(View) [seconds]' = 'dur.view'
  )
  id.dict <- data.table(id = names(dictionary), label = dictionary)
  
  id.head <- id.dict[hd, on='id']
  missing <- na.omit(id.head, invert = TRUE)
  if (nrow(missing) > 0){
    warning("Instar Export has unknown header fields:\n ", 
            paste(dQuote(missing$id), collapse = ', '), 
            '\n\n these columns will be dropped during the import')
    ia[, missing$id := NULL]
  }
  id.head <- na.omit(id.head)
  setnames(ia, id.head$id, id.head$label)
  
  # --- convert columns -------------------------------------------------------
  
  id.date <- ia[, .N, k=day][, label := as.character(as.Date(day,'%d.%m.%Y'))]
  ia[, day := id.date[ia, on='day', label]]
  
  if(any(c('chn','chn.name') %in% id.head$label)){
    
    # drop the Totals if present
    if(is.element('chn', id.head$label)){
      ia <- ia[!is.na(chn)]
    } else if(is.element('chn.name', id.head$label)) {
      ia <- ia[!chn.name=='TotalTV']
    }
    #
    # 7 chn have no channel id displayed in Instar: MULTIMATCH ans all UNMATCHED
    # They are allowed to have NAs
    # cols <- ia[, names(which(sapply(.SD, function(x) any(is.na(x)))))]
    #
    # cells of channels not valid at a specific day are filled with 'x' by Instar
    # Export. This forcees number columns to be read as characters. Reading 
    # those 'x' as NA directly by fread(file, na.strings = 'x) is somewhat tricky
    # so do it more controlled but less efficient:
    #
    cols <- names(which(sapply(ia, function(x) any(x == 'x'))))
    for (j in cols) set(ia, which(ia[[j]] == 'x'), j, '0')
    for (j in cols) set(ia, j = j, value = as.numeric(ia[[j]]))
  }
  
  if(all(c('rchT','dur.view') %in% id.head$label)){
    ordercol(ia[, dur := dur.view * rchT][, dur.view := NULL],'dur','universe')
  }
  
  

  
  cols <- intersect(c('day','x'), id.head$label)
  setorderv(ia, cols)
  ia[]
}
