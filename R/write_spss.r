
# Der import haven::read_sav() des Masterfiles liefert 2 bis 4 attribute:
# Mindestens 'label' und 'format.spss' (Bsp. 'ID'), falls eine Variable 
# Wertelabels aufweist auch 'class' und 'labels' (Bsp. 'Wave').
# Um nur labels anzufuegen existiert die function haven::labelled()
#
# at <- list(label       = 'Households with a netflix subscription',
#            format.spss = 'F1.0',
#            class       = 'labelled',
#            labels      = c('Filter No Internet' = -3,
#                            'Dont know' = -1,
#                            'No' = 0,
#                            'Yes' = 1))
# 
# identical(at, attributes(es$InternetTVnetflix)) # TRUE

# Leider produziert haven::write_sav() viel groessere Dateien als noetig ist: 
# wird das rausgeschriebene file in SPSS geoeffnet und wieder gespeichert betraegt
# die Filegroesse noch einen Bruchteil.
# Deshalb wird ein zweiter Weg angeboten ueber fwrite() und separatem SPSS 
# syntaxt file. Das package foreign bietet dies bereits an jedoch nicht sauber

# SPSS runs on virtual desktop only. However writing big files to local drive 
# is much faster than to other drives across the network like Q:/

write_spss <- function(dt, id, file, type=c('haven','csv')[1]) { # dt <- copy(out)
  
  # dt is usually dem but possibly with additional colums like viewing data etc. 
  
  # --- corresponding variables between dt and id$label ----------------------
  classmatch <- t(rbindlist(list(
    dtx   = lapply(dt,typeof), 
    labelx = lapply(id$label, function(x) typeof(x[[1]]))
    ), fill = TRUE))
  colnames(classmatch) <- c('dt','label')
  
  # all variables in dt to integer
  dt[, day := id$label$day[dt, on=c(label='day'), day]]
  
  # --- SPSS value labels -----------------------------------------------------
  
  # haven::labelled requires labels to be named vectors, the names give the 
  # labels for each value
  
  empty <- which(sapply(id$label, function(x) is.null(x) || NROW(x) < 1))
  noint <- which(sapply(id$label, function(x) !is.integer(x[[1]])))
  label <- names(id$label[-c(empty, noint)])
  label <- label[label %in% names(dt)]
  label <- lapply(id$label[label], function(x) setNames(x[[1]], x[[2]]))
  
  # --- SPSS variable labels --------------------------------------------------
  
  varlabel <- names(dt)[names(dt) %in% id$file$dem$name]
  varlabel <- setNames(id$file$dem[varlabel, on='name', name.long], varlabel)
  
  # --- SPSS format -----------------------------------------------------------
  
  fmt <- structure(names(dt), names = names(dt))
  fmt <- ifelse(fmt=='weight', 'F1.5', 'F1.0')
  
  # --- out with haven::write_sav() -------------------------------------------
  
  if(type=='haven') {
    
    library(haven)
    # value labels
    for(j in names(label)) 
      set(dt, j=j, value = labelled(dt[[j]], label[[j]]))
    # variable label
    for(j in names(varlabel)) 
      set(dt, j=j, value = structure(dt[[j]], label=varlabel[j]))
    # add spss variable format
    for(j in names(fmt))
      set(dt, j=j, value = structure(dt[[j]], format.spss=fmt[j]))
    # out
    cat('write_sav() has started ...')
    write_sav(dt, file)
    cat('done !')
    
  }
  
  # --- out csv with spss syntax file -----------------------------------------
  
  # if(type=='csv') {
  #   
  #   file.csv <- 'Q:/09_Forschung/09-09_ES/dem_2017-10-05.csv'
  #   file.sps <- 'Q:/09_Forschung/09-09_ES/dem_2017-10-05.sps' # spss syntax file
  # 
  #   spss.variable.labels <- function(x){ # x <- varlabel
  #     # varlabel is a named character vector of variable labels named by 
  #     # variable name
  #     txt <- do.call(paste0, list("  ",names(x)," '",x,"'"))
  #     x <- txt[length(txt)] 
  #     txt[length(txt)] <- paste0(substr(x, 1, nchar(x)-1), "'.") 
  #     c('VARIABLE LABELS', txt, 'EXECUTE.')
  #   }
  # 
  #   spss.value.labels <- function(x){ # x <- label
  #     # label is a named list of named character vectors: name of list elements 
  #     # give the variable names, character vector gives the values and an its 
  #     # name the labels
  #     txt <- do.call(paste0, list("  ",names(x)," '",x,"'"))
  #     x <- txt[length(txt)] 
  #     txt[length(txt)] <- paste0(substr(x, 1, nchar(x)-1), "'.") 
  #     c('VARIABLE LABELS', txt, 'EXECUTE.')
  #   }
  #   
  # file = 'C:/rlue/tv/R/out/2017-06-20_sozdemo_luca/spss.syntax.txt'
  # text <- spss.variable.labels(varlabel)
  # writeLines(text, file.sps)
  
}