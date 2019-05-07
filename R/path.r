
# we expect .First() returning .id at R startup.
# write in C:/Program Files/R/R-3.4.3/etc/Rprofile.site
# .First <- function() { 
#   assign(".id", "rl", envir = .GlobalEnv) 
#   assign(".path_git", "C:/Users/rlue/Documents/GitHub/", envir = .GlobalEnv) 
# }

path <- function()
{
  path <- list(
    # meta data to interpret raw pin data
    id   = '~/tv/pkg/tv/data/meta/',
    data = list(
      # raw pin data, not in package and not on GitHub: too big
      local = '~/tv/pkg/data/raw/',
      # the raw pin data repository from mediapulse
      repo = 'P:/InfoSys+/x-Daten_Archiv-Kantar/Archiv_Rohdaten IS+/raw_data_', # no slash ending 
      # the server where kantar puts the latest raw pin data to share with mediapulse
      # on a daily bases. Path leads to a file with URL and credentials (confidential)
      ftp = '~/tv/pkg/tv/data/meta/',
      # the kantar sysingredients database to extract the meta data
      sysingredients = "I:/InfoSysplus/1_IS_KMA_Live/KMA Seconds VM/"
    ),
    task = list(
      download       = '~/tv/pkg/task/download/',
      integrity      = '~/tv/pkg/task/integrity/',
      sysingredients = '~/tv/pkg/task/sysingredients/',
      channels       = '~/tv/pkg/task/channels/'
    )
  )
  
  if(.id=='dlc'){
    path$id <- "sc/git/tv/data/meta/"
    path$data$ftp <- "sc/git/tv/data/meta/"
    path$data$local <- "../data/TC-Kantar/"
    path$task <- list(
      download       = 'sc/git/tv-task/download/',
      integrity      = 'sc/git/tv-task/integrity/',
      sysingredients = 'sc/git/tv-task/sysingredients/',
      channels       = 'sc/git/tv-task/channels/'
    )
    Sys.setenv(TZ='GMT')
  }

  if(.id=='VD'){
    root <- 'Q:/09_Forschung/09-10_DataScience'
    path <- relist(gsub('~', root, unlist(path)), path)
    path$data$local <- path$data$repo
    setwd(root)
  }
  
  return(path)
  
}
