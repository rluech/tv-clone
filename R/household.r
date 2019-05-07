
#' Aggregate viewing statements within household
#' 
#' This function takes the \code{view} object and returns it after aggregating 
#' viewing statements within each household.
#' 
#' Instead of calculating TV viewing on individual level, TV viewing can be 
#' aggregated within each household. The Kantar aggregation method is not simply
#' summing up all indidividuals viewing within the household. Rather, the 
#' viewing satetments are rebuilt to reflect a household perspective: If more 
#' than one person is watching a given content at the same time, the viewing 
#' only counts once on household level not multiple times as it would be the
#' case on individuals level. Use \code{setup(obs = c('ind','hh'))} to 
#' switch level of observation between individual and household (\code{'ind'} 
#' is default).
#' 
#' By default guest viewing is included when aggregating by houlsehold. To 
#' exclude guest viewing use \code{setup(guest = FALSE)}.library(data.table)
#' 
#' There are two types of Kantar household aggregation methods, namely 'normal' 
#' and 'by channel'. For Switzerland the parameter is set to 'normal'. Use 
#' \code{setup(hh.calc = c('normal','by channel'))} to switch types of 
#' household calculation (\code{'normal'} is default). This only takes effect if 
#' \code{obs} is set to \code{'hh'}.
#' 
#' 'by channel' means, if two or more person are watching at the same time but 
#' different channels, the viewing is aggregated by channel. 'normal' means in 
#' such a case the viewing is interpreted as a single viewing statement ignoring
#' that there was different content being watched. If later facts are calculated 
#' by channels hh.calc has to be aggregated by channel to macht instar results.
#' 
#' @param view The view object, a data.table containing the viewing statements 
#' by day and individuals (\code{pin}).
#' @param guest logical, including (TRUE) or excluding (FALSE) guest viewing? If
#' not otherwise set, the parameter is taken from \code{id$view$guest}, the guest
#' parameter set in \code{setup(guest = )}.
#' @param hh.calc 'normal' (default) or 'by channel'. If not otherwise set, 
#' the parameter is taken from \code{id$view$hh.calc}, the type of household 
#' caclulation set in \code{setup(hh.calc = )}.
#' 
#' 
#' @return The function returns the view object with viewing statements beeing 
#' aggregated within each household. The sum of viewing duration is less than 
#' on individual level and the number of viwing statements is less too. The 
#' column \code{pin} now represents houshold ID.
#' 
#' @examples
#' 
#' setup('01-01-2018', 3, prg = FALSE, import = TRUE) # on individual level
#' dem.ind <- dem
#' view.ind <- view
#' # pin in data.table 'dem' and 'view' reflect indiviual IDs 3 to 6 digit number e.g. 601
#' 
#' setup('01-01-2018', 3, prg = FALSE, obs = 'hh', import = TRUE)
#' dem.hh <- dem
#' view.hh <- view
#' # pin in data.table 'dem' and 'view' reflect household IDs 1 to 4 digit number e.g. 6
#' # viewing statements are less and different form dem.ind
#' nrow(view.ind); nrow(view.hh)
#' sum(view.ind$dur); sum(view.hh$dur)
#' 
#' @export
#' 
household <- function(view, guest = id$view$guest, hh.calc = id$view$hh.calc) 
{
  cat('running household algorithm...')
  on.exit(cat(' done.'))
  
  view[, hh := pin %/% 100L]
  if(!guest) view <- view[pin %% 100L < 50L]
  hh.calc <- switch(hh.calc, 'normal' = NULL, 'by channel' = 'chn')
  key <- c('day','hh', hh.calc)
  setkeyv(view, c(key, 'start','end'))
  view[, grp := cumsum(cummax(shift(end, fill = 0)) < start), by = key]
  hh <- view[, .(start[1L], max(end)), by = c(key, 'grp')]
  hh <- view[hh, on = c(key, 'grp'), mult = 'first']
  hh[, c('pin','start','end') := .(hh, V1, V2)]
  hh[, c('hh','V1','V2','grp') := NULL][]
}


household.iterativ <- function(view, guest = id$view$guest, silent = FALSE) {
  
  view[, hh := pin %/% 100L]
  if(!guest) view <- view[pin %% 100L < 50L]

  key <- c('day','hh','base')
  setkeyv(view, c(key,'start','end'))
  # view[, gap := start > shift(end, fill = FALSE), by = key] # slow
  view[, gap := start > shift(end, fill = FALSE)]             # same, much faster
  view[view[, .N, by=key], on = key, mult = "first", gap := TRUE] 
  # idea: for each new group (key) gap is always TRUE, so no need to calculate 
  # it for each group separately, just replace first item in each group with TRUE
  
  # repeate until all statement have a gap to the statement before
  if(!silent) cat('\n')
  iter <- 0
  while(view[, !all(gap)]) { # view[(!gap)]
    
    view[(gap), id := .I][, id := na.locf(id, NULL)]
    id.gap <- view[, .(start = min(start), end = max(end)), by = 'id']
    view <- view[id.gap, on = 'id', mult = 'first']
    view[, c('start','end','pin') := .(i.start, i.end, hh)] # keeps colorder
    view[, c('i.start','i.end','gap','id') := NULL]
    setkeyv(view, c(key,'start','end'))
    view[, gap := start > shift(end, fill = FALSE)]
    view[view[, .N, by=key], on = key, mult = "first", gap := TRUE] 
    iter <- iter + 1
    if(!silent) cat("household iteration: ", iter, "\n")

  }
  view[, c('hh','gap') := NULL][]
}


# # benchmark
# 
# setup('2017-10-26', 30, dem = FALSE, prg = FALSE, import = TRUE)
# view
# system.time(hh <- household(copy(view), TRUE))
# system.time(hh <- household(copy(view), TRUE))
# system.time(hhi <- household.iterativ(copy(view), TRUE))
# system.time(hhi <- household.iterativ(copy(view), TRUE))
# 
# all.equal(hh, hhi)

# # a good example
# out <- view[day=='2017-01-01' & hh==1264 & sta==50]
# fwrite(out, sep=';', 'C:/rlue/tv/R/out/2017-07-11_by_houshold/statements_hh1264_sta50.csv')
# 
# plot(0, xlim = c(min(out$start), max(out$end)), ylim = rev(c(1, nrow(out))))
# for(i in seq(nrow(out)))
#   segments(out[i,start], i, out[i,end], i, lwd = 5, lty = ifelse(out[i,gap]==FALSE,3,1),
#            col = out[i,as.integer(pin-hh*100)])
#
# # this is the same data:
# dat <- data.table(day = '01.01.2017', hh = 1264,	sta	= 50,
#   start = c(47671,47876,48911,51129,52439,54379,55684,67377),
#   end = c(53475,48649,50688,52109,55564,57401,56684,68597),
#   key = 'day,hh,sta,start,end')

# --- an extreme case ---------------------------------------------------------
# 
# source(paste0(.path_git,'git_tvpackage/path/',.id,'.r'))
# id <- setup("2017-10-28")
# go manually through import() to household() and there stop at while()
# (dd <- view[day == "2017-10-28" & hh == 2821])
# dd[, ind := as.integer(pin-hh*100)]
# setorder(dd, "start", "end")
# # 
# 
# tbl <- dd[chn.name == "SRF zwei"][1:50, .(day,hh,ind,chn, chn.name,start,end,set,act,plt)]
# tbl[, plt := id$lab$plt[tbl, on="plt", label]]
# 
# 
# file.out <- "~/tv/projects/2018-06-06_pin_data_issues_for_3plus/plot01.png"
# png(file.out, width = 1000, height = 700)
# d <- dd[chn.name == "SRF zwei"]
# plot(0, xlim = c(min(d$start), max(d$end)), ylim = rev(c(1, nrow(d))),
#      ylab = "viewing statements", xlab = "time")
# #xt <- do.call(seq, as.list(setNames(par("xaxp"), c("from","to","length.out"))))
# #axis(1, xt, s2t(xt))
# for(i in seq(nrow(d)))
#   segments(d[i,start], i, d[i,end], i, lwd=1.5, col=d[i,ind])
# 
# dev.off()
