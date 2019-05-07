
#' Reads the .std-file
#'
#' The .std file contains the program logs ("Sendeprotokoll") for a given day. 
#' 
#'  The program logs are stored in ".std" files. For each day there is a separate 
#'  file. The files are of fixed-with type, with up to 55 fields, some fields as
#'  long as 100 characters. Following are the first fields. Possably not all of 
#'  these fields are needed, e.g 3, 6, 7, 8 and 9 might be not needed: 
#' 
#'  field	 name            label	 length class
#'  1	     channel code    sta	   5	    integer (the code of virtual channels)
#'  2	     BrdCstId	       prg	   8	    integer (broadcast id)
#'  3	     date	           date	   8	     integer (is equal to file date)
#'  4	     start	         start	 6	    integer
#'  5	     duration	       dur	   5	    integer
#'  6	     BrdCstTimePlan  ref	  15      integer (date/time reference for some ads)
#'  7	     Title	         -	   100	    character
#'  8	     Description	   -	   100	    character
#'  9       Comments        -     100      character
#' 10       BrdCstTyp       typ     2      integer (Senderuebergreifende definition)
#' ..       ...             ...     .      ...
#' 24       BrdCstSeq       seq     3      integer
#' 25       SumPieces       seqN    3      integer
#' ..       ...             ...     .      ...
#' 
#' Inkonsistenzen
#' - DE180224.STD genre code (2 Zeichen lang) auf Zeile 1608 ist "1.", read.prg
#'   gibt eine warnung aus, leist das File aber korrekt, mit coercion to NA
#' 
#' @export

read.prg <- function(id) 
{
  
  file <- id$lab$day$prg[(exist), paste0(id$path$data$local, file)]
  if(!length(file)) return()
  
  fwf <- fwf(id$file$prg, enc = 'latin1', pin = FALSE)
  prog <- rbindlist(lapply(file, read, fwf), idcol = 'day')
  cat('\n')

  # filter out 9029
  prog <- prog[chn < 9000L]
  # filter out summary statements
  # prog[seq == 0 & seqn == 0, seqtyp := 'programs not split up']
  # prog[seq > 0             , seqtyp := 'program splits']
  # prog[seq == 0 & seqn > 0 , seqtyp := 'summary of program splits'][]
  prog <- switch(id$prg$seq, 
    gross = {
      prog[seq == 0L, -c('seqn')] }, 
    net = { 
      prog <- prog[!(seq == 0L & seqn > 0L), -c('seqn')]
      prog[, c('prg','seq') := .(prg * 100 + seq, NULL)] } # too big for integer
    )
  prog[, start := id$lab$time[prog, on=c(id='start'), label]]
  prog[, end := start + dur - 1L] # max(end) = 93599L, same as in viewing data
  ordercol(prog, 'end', 'start')
  setnames(prog, 'dur', 'prg.dur')
  prog[, day := id$lab$day$prg[prog, on='day', label]]
  nm <- names(prog)
  if("prgtyp" %in% nm){
    prog[, prgtyp := id$lab$prgtyp[prog, on='prgtyp', label]]
  }
  if("genre" %in% nm){
    prog[, genre := id$lab$genre[prog, on='genre', label]] 
    prog[is.na(genre), genre := "missing"] # genre has NAs
  }
  prog[]
}


# # --- plot program sequences --------------------------------------------------
# 
# # color
# prog[seq == 0 & seqn == 0, seqtyp := 3]
# prog[seq > 0             , seqtyp := 2]
# prog[seq == 0 & seqn > 0 , seqtyp := 4]
# 
# nday <- '2016-09-01'
# idvirt <- id$lab$sta[virt>0, unique(virtnm), k=.(virt)]
# dat <- prog[day == nday & virt %in% idvirt$virt]
# plot.fun <- function()
# {
#   for(i in dat[, .N, virt][order(+virt), virt]) # i <- 8001
#   { 
#     d <- dat[.(i), on='virt']
#     plot(NA, xlim = c(7200, 93599), ylim = c(1,d[,.N]), type = 'n', 
#          xlab = 'day time', ylab = 'program statement', xaxt = 'n',
#          main = c(paste0(idvirt[.(i),on='virt',V1],' [',i,']'), nday))
#     axis(1, 3600*c(2,8,14,20,26), s2t(3600*c(2,8,14,20,26),'t'))
#     segments(d$start, d[,.I], d$end, col = d$seqtyp)
#     legend('topleft', c('program','program-sequence','sequence-summary'), 
#            text.col = c(3,2,4), bty = 'n')
#   }
# }
# 
# source('C:/rlue/doc/R/util/to_pdf.r')
# to.pdf(plot.fun(), 'C:/rlue/tv/doc/out/2017-06-12_programlog/programlog.pdf')

# --- Das Sendeprotokolle muss aufgeteilt werden ------------------------------

# Die Summe der duration im programfile kann pro Tag und Sender groesser als 24h
# sein! 
# prog[, sum(dur), k = .(day,virt)][V1 > 3600*24]
#
# Das program file besteht aus zwei Datenbanken. 
# id.prg <- prog[  seq == 0 & seqn > 0]
# id.seq <- prog[!(seq == 0 & seqn > 0)]

# 1. Einerseits die einzelnen Programteile, das sind alle programm sequenzen, 
#    wobei ein nicht unterteiltes Program aus einer einzelnen sequenz besteht. 
#    Die Summer der dauer dieser Elemente kann nicht groesser als 24 h sein, 
#    id.seq[, sum(dur), k = .(day,virt)][V1 > 3600*24]

# 2. Andernseits die Programm summaries von sequenzierten Programmen. Diese 
#    geben die Butto dauer eines Programs wieder, also von Anfang bis Ende 
#    inklusive der Werbebloecke und sonstgen Unterbrechnugen, die nicht dieselbe 
#    Program-id tragen. Die Bruttodauer muesste gemaess Simon die Dauer sein von 
#    Start erster Sequenz bis Ende letzter Sequenz. Somit koennen die summries 
#    komplett ignoriert werden.

# datenbank   program-log-type         seq       seqn
# 1           1 "nicht sequenziert"    = 0       = 0
# 1           2 "sequenziert"          > 0       > 0
# 2           3 "summary sequenzen"    = 0       > 0
#
# wobei seq die Sequenz Nummer, seqn die Anzahl Sequenzen wiedergibt. Der Filter
# fuer summary sequenzen ist demnach: { seq == 0 & seqn > 0 }

# Beispiel:
# prog[day == '2016-09-01' & virt == 8005][1:5, .(day,virt,prg,start,end,dur,seq,seqn,title)]
#
#    day        virt      prg start   end  dur seq seqn                                 title
# 1: 2016-09-01 8005 57631107  7200 12605 5406   0    2 ZDF SPORTextra: Fussball: Laenderspiel
# 2: 2016-09-01 8005 57631107  7200  9705 2506   1    2 ZDF SPORTextra: Fussball: Laenderspiel
# 3: 2016-09-01 8005 57631453  9706  9712    7   0    0            ZDF SPORTextra: Moderation
# 4: 2016-09-01 8005 57631454  9713  9721    9   0    0            ZDF SPORTextra: Moderation
# 5: 2016-09-01 8005 57631107  9722 12605 2884   2    2 ZDF SPORTextra: Fussball: Laenderspiel

# Zeile 1, 2 und 5 gehoeren demselben Program (prg 57631107) an. Zeile 2 und 5 
# sind die beiden sequenzen in denen das Program ausgestrahlt wurde. Zeile 1 
# gibt die gesamtdauer des programs (inklusive zwischenschaltungen) an. Zeilen
# 3 und 4 sind nicht-sequenzierte eigenstaendige Sendungen.

# --- Luecken ------------------------------------------------------------------

# Im Beispiel knuepft eine Sendung direkt an die Naechste an (vergl. start und end)
# Dies muss nicht immer so sein. Zwischen zwei Sendungen koennen Luecken bestehen. 
# SRF 1 hat zum Beispiel keine Luecken, das heisst jeder Sekunde von 7200 bis 
# 93599 ist eine Sendung zugeordnet. Die meisten anderen Sender habe jedoch 
# Luecken. 

# --- ueberlappungen -----------------------------------------------------------

# Die Sendeprotokolle sind so definiert, dass zeitliche ueberlappungen 
# zwischen Sendungen eines Senders nicht vorkommen duerfen. Das heisst, eine 
# Sekunde hat genau eine Sendungs-Zuordnung.
# 
# Beides ist releavant um im merging process mit den viewing Zeitstempeln nicht 
# viewing zu verlieren (nomatch) oder doppelt (multimatch) zu zaehlen.

# --- program-id (prg) --------------------------------------------------------

# Die program-id (prg) ist ein-eindeutig, auch ueber mehrere Tage hinweg und 
# zwischen channels (dieselbe Simpson Folge auf verschiedenen Sendern oder zu 
# verschiedenen Zeiten hat jedesmal eine andere id, 10vor10 wird morgen eine 
# andere id haben als heute). Ausnahme sind Programme die ueber das Tagesende 
# hinausgehen: der Rest der Sendung am morgen des naechsten Tages hat noch 
# dieselbe id wie am Abend:
#
# id.seq[seq<2, .N, k=prg][, .N, k=N]
# prgdup <- id.seq[seq<2][duplicated(prg), prg]
# id.seq[seq<2][prg %in% prgdup, .(day,virt,prg,s2t(start,'t'),s2t(end,'t')), k=prg]
#
# id.prg[seq<2, .N, k=prg][, .N, k=N]
# prgdup <- id.prg[seq<2][duplicated(prg), prg]
# id.prg[seq<2][prg %in% prgdup, .(day,virt,prg,s2t(start,'t'),s2t(end,'t')), k=prg]

# --- program-type (typ) ------------------------------------------------------

# Program type erscheint auch in Instar und sei recht zuverlaessig um Werbung zu 
# identifizieren. Die program summary enhalten keine Werbung, da Werbung nie 
# sequenziert ist.
# id.seq[, .N, typ]
# id.prg[, .N, typ]

# --- virtual channels (virt) -------------------------------------------------

# Zur Zeit gibt es nur von virtual channels Sendeprotokolle, virtual channel ist 
# also key variable um mit viewing zu mergen.
# Alle virtual channels sind in ttv, jedoch nicht zwingend jeder stream eines
# virtual channels, e.g bei virtual channel 8549-"S1" ist nur S1_n nicht aber
# S1_a in ttv
#
# idvirt <- id$lab$sta[virt > 0, .N, k = .(virt, ttv)][, N := NULL][]
# duplicated(idvirt$virt) # .. es gibt aber virt mit ttv 1 und 0
# idvirt[.(0), on='ttv']  # "S1" und "CartoonNetwork", 
# 
# Total TV muss also per stream (sta) in den viewing daten gefiltert werden, 
# fuer Sendeprotokolle muss ttv nicht beachtet werden
#
# Summary virtual channels
# view[, .N, k=virt]                    # 142 virtual channel
# prog[, .N, k=virt]                    # 66  virtual channel
# id$lab$sta[, .N, k=.(virt, virtnm)] # 155 virtual channel
#
# Es gibt (ca) 4 virt channels (8745,8954,8955,9029) im program file welche in
# der lookuptable der channels gar nicht definiert sind
# id$lab$sta[, .N, k=.(virt, virtnm)][prog[, .N, k=virt], on='virt']
# view[, .N, k=virt][prog[, .N, k=virt], on='virt']
# prog[.(8745), on='virt'] # title: 'MEDIASHOP', 'SHIVA TV'
# prog[.(8954), on='virt'] # title: 'Couleurs locales (r)', 'Indicatif bonne nuit RTS 2015'
# prog[.(8955), on='virt'] # title: 'Couleurs locales (r)', 'Indicatif bonne nuit RTS 2015'
# prog[.(9029), on='virt'] # title: 'Werbung News', 'Werbung Kochen / Genuss'


# --- join --------------------------------------------------------------------

# key um id.seq und view zu joinen sind day, virt, start, end
# prog[seq == 0 & seqn > 0, .N , k=c('day','virt','start')][, .N, k=N]
# oder
# id.prg[, .N , k=c('day','virt','start')][, .N, k=N] 

# Beispiel join aus Sicht einer Person
# x <- view[day == '2016-09-01' & pin == 434201 & virt == 8003]
# setkeyv(id.seq, c('day','virt','start','end'))
# X <- foverlaps(x, id.seq, nomatch=0)

# was passiert aus sicht eines viewing statements bei partial overlap
# v <- view[.('2016-09-01',434201, 8014), on=c('day','pin','virt')]
# y <- foverlaps(v, id.prg)
# y[, c('j.start','j.end') := .(i.start, i.end)]
# y[, c('typ','genre','set','act','plt') := NULL]
# 
# y[i.start < start, j.start := start]
# y[i.end > end, j.end := end]

# --- Stack Overflow example --------------------------------------------------
# 
# vs <- c(from = 5, to = 20) # viewing statement
# kn <- lapply(list(kn1=c(5,7), kn2=c(11,18)), setNames, nm = c('from','to'))
#
# fun <- function(x) do.call(seq, as.list(x))
# vs.v <- fun(vs)
# kn.v <- lapply(kn, fun)
# un <- lapply(kn.v, setdiff, vs)
# c(kn.v, diff)
# 
# # problem description
# X <- data.table(start = 12345, end = 23987)
# Y <- data.table(start = c(13094,14856,22721), end = c(13409,20867,23511),
#                 label = c('a','b','c'), key = c('start','end'))
# 
# Z <- foverlaps(X, Y)
# 
# Z[, c('j.start','j.end') := .(i.start, i.end)]
# Z[i.start < start, j.start := start]
# Z[i.end > end, j.end := end]
# 
# Z[, sum(j.end - j.start)] == X[, end - start]
# 
# fill <- data.table(
#   start = NA, end = NA, label = NA, i.start = 12345, i.end = 23987, 
#   j.start = c(12345, 13409, 20867, 23511),
#   j.end = c(13094, 14856, 22721, 23987))
# 
# z <- rbind(Z, fill)[order(j.start)]
# 
# z[, sum(j.end - j.start)] == X[, end - start]

# see also data.table::shift()