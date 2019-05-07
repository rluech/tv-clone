
#' import dem and add rim variables
#' this is simply a wrapper for setup() but handles problems in old .dem files
#' 
#' @export

dem.rim <- function(from, to) # from <- '2013-01-01'; to <- '2013-12-31'
{
  require(tv)
  dem.var <- eval(formals(tv:::dem.add.rim)$need)
  setup(from, to, dem.var = dem.var, dem.uni = FALSE, view = FALSE, prg = FALSE,
        import = TRUE)
  # exception
  if('2013' %in% dem[, .N, k=substr(day,1,4)]$substr){
    # na.omit(dem, invert = TRUE)
    # dcast(dem, day~pin, identity, subset=.(pin %in% 24551:24556), value.var='age', fill=0)
    dem[day == '2013-03-03' & pin %in% 24551:24556, age := 99L] # hw hat 46
    # dem[day == '2013-03-03' & pin %in% 24500:24560]
    # Guest are not relevant for weighting and universe
    # print(dem[pin == 205402, age, k=day], nrows = 500) # pin 205402 is 60
    dem[pin == 205402 & is.na(age), age := 60L]
  }
  dem.add(dem, 'rim')
}

#' caclulate universe approximately by summing up the weights for each rim and 
#' each day. Guest are excluded, they are not part of the universe. The weight 
#' of a guests is always a copy of the weight of the housewive.
#' Value as well as labels are returned because values need to match the values 
#' in margins for ipf weighting. Labels allow to check that the same value have
#' also the same meaning
#' 
#' @export
#'
sum.weight <- function(dem, cols=grep('rim',names(dem),value=TRUE), w='weight')
{
  fun <- function(x) dem[!(guest), lapply(.SD,sum), .SDcols=w, k=c(x,'day')]
  sumw <- Map(fun, cols)
  for(j in cols) sumw[[j]][, label := id$lab[[j]][sumw[[j]], on=j, label]]
  sumw <- rbindlist(sumw, idcol = 'rim')
  ordercol(setnames(sumw, 2, 'id'), 'label', 'id')
}

#' Applies ipf weighting for each day. Input is dem and rims. Output is one 
#' additional column "reweight" in dem (Appearing just next to "weight").
#' 
#' dem is expected to contain columns 'day', 'pin', 'guest', 'hw', and all rims. 
#' Pin has to represent individuals (weigth are individuals). Ipf will run 
#' without guests, guests get the weigth of the hw in the correspondinh 
#' household. 
#' 
#' rims is a named list of n data.frames (or data.tables) each representing a 
#' weighting rim. A rim contains the population margins as the last column. 
#' Other columns may give the labels for each weigthin cell. Each rim has to 
#' sum up to the same total. The order of rims as well as the order of each 
#' rim's cells matters.
#' 
#' rim names as well as each rim's levels (numeric or string labels) have to 
#' match exactly between rims and dem. 
#' 
#' @export
#' 
dem.weight <- function(dem, rims, crit = 0.5) # crit von KMS ev. 0.5
{
  stopifnot(all(c('day','pin','guest','hw') %in% names(dem)))
  # check rims labels are identical between dem and rims
  cols <- names(rims)
  stopifnot(all.equal(
    sum.weight(dem, cols)[, .N, by = .(rim,id,label)][,-'N'],
    rbindlist(rims, idcol = 'rim')[,.(rim,id,label)]
  ))
  days <- unique(dem$day)
  log <- setNames(vector('list', length(days)), days)
  cat('\nweighting, crit:', crit, '\n')
  for(i in days) {
    ans <- ipf(
      data = dem[.(i,FALSE), on=c('day','guest'), c('pin',cols), with = FALSE], 
      rims = rims,
      crit = crit
    )
    log[[as.character(i)]] <- ans$log
    dem[.(i,FALSE), on=c('day','guest'), reweight := ans$weight][]
    cat(i,' iter: ', nrow(ans$log), '\n')
  }
  ordercol(dem, 'reweight', 'weight')
  attr(log,'crit') <- crit
  assign('log', log, envir = .GlobalEnv)
  
  # --- add guest weights -------------------------------------------------------
  
  cat('add guest weights')
  ordercol(dem[, hh := pin %/% 1e2L], 'hh', 'pin')
  id.hw.w <- dem[.(1L), on='hw', .N, k=.(day, hh, reweight)]
  stopifnot(id.hw.w[, .N == sum(N)])       
  dem[(guest), reweight := id.hw.w[dem[(guest)], on=c('day','hh'), reweight]][]
  # stopifnot(identical(
  #   dem[(guest), .N, k=.(day, hw, reweight)]$reweight,
  #   dem[(guest), .N, k=.(day, guest, reweight)]$reweight
  #   ))
}



#' iterative proportional fitting
#'
#' @return a list containing a numeric vector of the weights and a vector of 
#' the convergence criterion for each iteration
#' @export
#'
ipf <- function(data, rims, crit = 0.0001, iter = 250) {
  
  # data:
  # A data.table or data.frame with observation by rim variables. The first 
  # column has to be the observation id, all the remaining columns will be used
  # in weighting and they have to be in numeric values, character will be sorted
  # when dcast forms column names and wrong results would be produced without a
  # warning
  #
  #    pin     rim_hw      rim_agesex
  # 1: 901   fr_nonhw  fr_m_age50to59
  # 2: 902      fr_hw  fr_f_age40to49
  # 3: 903   fr_nonhw  fr_f_age20to29
  #
  # rims:
  # A named list with as many elements as rim variables and element being a 
  # data.table with rim levels column and a column named 'pop' with the margins:
  #
  # $`rim_hw`
  #       label  pop
  # 1:    de_hw 2367
  # 2: de_nonhw 2808
  # 3:    fr_hw  796
  #
  # rim levels can be numeric or string, but make sure rim levels match between
  # margins and sample including their order, otherwise you get wrong results 
  # without a warning!
  #
  # use the faster matrix algebra so first turn data.frame into matices
  # 
  # for data:
  #
  # turn data.frame: 
  #
  #           pin    rim_agesex
  # 1: 1211012411            18
  # 2: 1211013581            16
  # 3: 1211013961            16
  # 4: 1211013962             7
  # 5: 1211013963            12
  # 6: 1211013971             9
  # 
  # into matrix:
  # 
  #            7 9 12 16 18
  # 1211012411 0 0  0  0  1
  # 1211013581 0 0  0  1  0
  # 1211013961 0 0  0  1  0
  # 1211013962 1 0  0  0  0
  # 1211013963 0 0  1  0  0
  # 1211013971 0 1  0  0  0
  
  nm <- names(rims)
  stopifnot(identical(names(data)[-1], nm))
  dt2mx <- function(x, id = names(data)[1L], data) {
    dt <- dcast(data, paste(id,'~',x), length, fill = 0L, value.var = id)
    as.matrix(dt[, -(id), with = FALSE])
  }
  D  <- lapply(nm, dt2mx, data = data) 
  # Muss aus 0/1 bestehen. Falls > 1, hat data wohl mehr als 1 Tag
  Dt <- lapply(D, t)
  # and turn also the rims from a list of data.tables into a list of matrices
  M  <- lapply(rims, function(x) as.matrix(x[[length(x)]])) # expect last col
  w  <- matrix(1L, nrow = nrow(data))
  
  # --- iterative proportional fitting ----------------------------------------
  
  fit <- function(d, dt, m, w){
    x <- m / (dt %*% w)
    w <- d %*% x * w
  }
  dif <- function(w, m) max(abs(w - m)) # "maxdiff" to quantify convergence
  log <- rep(NA, iter) # trace the convergence 
  
  for(i in seq(iter)) {
    for(j in seq_along(nm)) w <- fit(D[[j]], Dt[[j]], M[[j]], w)
    wsum <- lapply(Dt, `%*%`, w)
    log[i] <- dif(unlist(wsum), unlist(M))
    if(log[i] < crit) break
  }
  
  # --- return ----------------------------------------------------------------
  
  list(weight = w, log = cbind(na.omit(log)))
  
}
