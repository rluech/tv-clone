
#' Construct new Variables
#' 
#' \code{dem.add} is a generic function. It is intended to by applied to the 
#' \code{dem} object and constructs new variables based on existing ones. The 
#' new variables are added as columns to dem and the labels to the variable 
#' values are added to \code{id$lab}.
#' 
#' @param x A data.tabel, typically \code{dem}.
#' 
#' @param what A length one character vector. What set of variabels? Corrently 
#' available are: \code{'rim','calendar','ageclass','gm','zone','lifestage'},
#' see details.
#' 
#' @param id. The \code{id} object. See \code{id <- setup()}.
#' 
#' @param breaks An optional numeric vector, sepcifying the age breaks to split 
#' age into ageclasses. Default are the 7-folded ageclasses used to weight the 
#' TV-panel: \code{c(3,15,20,30,40,50,60)}.
#' 
#' @details
#' To allow the construction of new variabels based on other variabels, these 
#' other variabels have to be present in the \code{x} (usually \code{dem}). This 
#' means they have to be imported in the first place, e.g.: 
#' \code{setup(..., dem.var = c('v1','v2'))}. How do we know which variables are 
#' needed? There is a parameter \code{need} in each subfunction where the needed 
#' variabels are listed. So, to add rims first ask what variabels are needed 
#' and the import the data accordingly, see examples.
#' 
#' Careful with \code{'gm'} and \code{'zone'}. These read the corresponding CSV 
#' file in ../data/meta/. 
#' 
#' @return \code{x}, the input data.table (usually \code{dem}) is changed in 
#' place. The new variabels are added as new columns to \code{x} and the 
#' corresponding variable values and labels are added to \code{id$lab}.
#' 
#' @examples 
#' library(tv)
#' dem.var <- eval(formals(tv:::dem.add.rim)$need)
#' setup('2018-01-01', 3, dem.var = dem.var, import = TRUE, view = FALSE, prg = FALSE)
#' dem[]               # dem contains additional variables specified in dem.var
#' dem.add(dem, 'rim') # construct new variables 'rim' (weighting rims).
#' dem[]               # The new variables are added to dem
#' id$lab$rim_agesex   # And theis labels are added to id$lab
#' 
#' # dem.add.calendar only needs 'day' which is always present:
#' setup('2018-01-01', 3, import = TRUE, view = FALSE, prg = FALSE, dem.uni = FALSE)
#' dem.add(dem, 'calendar')
#' id$lab$month
#' 
#' @rdname dem.add
#' @export
#' 
dem.add <- function(x, what, id.=id, breaks=c(3,15,20,30,40,50,60)) # https://goo.gl/G65Nbd StackOverflow for .id solution
  switch(what, 
         rim       = dem.add.rim(x, id=id.),
         calendar  = dem.add.calendar(x, id=id.),
         ageclass  = dem.add.ageclass(x, id=id., breaks=breaks),
         gm        = dem.add.gm(x, id=id.),
         zone      = dem.add.zone(x, id=id.),
         lifestage = dem.add.lifestage(x, id=id.)
         )

dem.add.calendar <- function(x, id, need='day', cols = c('year','month','wday','wend')) {
  
  stopifnot(all(need %in% names(x)))
  lab <- id$lab$day$live
  # add label
  lab[, date := as.Date(label)] # for more flexibility see base::strptime()
  lab[, (cols) := .(year(date), month(date), wday(date)-1L, 2L)]
  lab[wday == 0L, wday := 7L]
  lab[wday < 6, wend := 1L]
  lab <- list(
    year  = id.dt(NULL),
    month = id.dt(month.name),
    wday  = id.dt(c('Mon','Tue','Wed','Thu','Fri','Sat','Sun')),
    wend  = id.dt(c('Workday','Weekend'))
  )
  for(j in names(lab)) setnames(lab[[j]], old=1L, new=j)
  id$lab <- c(id$lab, lab)
  assign('id', id, envir = .GlobalEnv)
  # add columns
  x[, (cols) := id$lab$day$live[x, on=c(label='day'), cols, with = FALSE]][]
  ordercol(x, cols, 'day')
}


dem.add.ageclass <- function(x, breaks=c(3,15,20,30,40,50,60), need='age', id=id) 
  {
  stopifnot(all(need %in% names(id$lab)))
  stopifnot(all(need %in% names(x)))
  if(x[, any(is.na(age)) || min(age) < 3]) 
    warning('panelmember age missing or below 3 years!')
  # label
  age <- as.integer(breaks)
  names(age) <- sprintf('age%02ito%02i', age, c(age[-1],100)-1)
  lab <- list(ageclass = id.dt(names(age), id.name='ageclass', roll=age))
  id$lab <- c(id$lab[setdiff(names(id$lab),'ageclass')], lab)
  assign('id', id, envir=.GlobalEnv)
  # column
  x[, ageclass := id$lab$ageclass[x, on=c(roll='age'), roll = TRUE, 1L]]
}


dem.add.rim <- function(x, id, need=c('hw','sg','age','sex','hhsize','hhkids02',
                                    'hhkids14','receptiontype','ez','tvset')) {
  # x <- dem
  stopifnot(all(need %in% names(id$lab)))
  stopifnot(all(need %in% names(x)))
  if(x[, min(age) < 3]) warning('panelmember age below 3 years!')
  
  rim <- list(
    'rim_hw'         = c('sg','hw'),
    'rim_agesex'     = c('sg','sex','ageclass'),
    'rim_adult'      = c('sg','adult'),
    'rim_hhsize'     = c('sg','hhsize','hw'),
    'rim_kids02'     = c('sg','hhkids02'),
    'rim_kids14'     = c('sg','hhkids14'),
    'rim_reception5' = c('sg','receptiontype','hw'),
    'rim_reception3' = c('sg','reception3','hw'),
    'rim_ez'         = c('ez'),
    'rim_tvset'      = c('sg','tvset')
    )
  
  lab <- id$lab[need]
  # first add helper label
  age <- as.integer(c(3,15,20,30,40,50,60))
  names(age) <- sprintf('age%02ito%02i', age, c(age[-1],100)-1)
  lab <- c(lab, list(
    ageclass   = id.dt(names(age), roll=age),
    adult      = id.dt(c('adult_m','adult_f','child')),
    reception3 = id.dt(c('iptv','cab','sat'))
    ))
  # create and add rim label
  lab <- c(lab, sapply(names(rim), function(x) id.dt.cross(lab[rev(rim[[x]])])))
  # id.dt.cross returns always coninuous which is what we want to match Kantar.
  # When droping rows in the folowing rims, restore continuous id.
  # id and label should match the tv rims as displayed by Kantar
  lab$rim_hhsize <- lab$rim_hhsize[!.(1L,2L), on=c('hhsize','hw')][, id := .I]
  # rim_reception5: no PC-only
  lab$rim_reception5 <- lab$rim_reception5[!receptiontype==6L][, id := .I] 
  # rim_ez: stay with continuous id for its rim! rewrite labels
  lab$rim_ez[, label := paste0(substr(label,5,6), '_', substr(label,1,4))]
  #
  for(j in names(lab)) setnames(lab[[j]], old = 1L, new = j)
  id$lab <- c(id$lab, lab[setdiff(names(lab), names(id$lab))])
  assign('id', id, envir=.GlobalEnv)
  
  # add columns
  x[, ageclass := lab$ageclass[x, on=c(roll='age'), roll = TRUE, 1L]]
  x[, adult := sex][.(1), on='ageclass', adult := 3L]
  x[, reception3 := receptiontype][reception3 >= 4L, reception3 := 2L][]
  for(j in names(rim)) set(x, j=j, value=lab[[j]][x, on=rim[[j]], 1L])
  
  # check
  #
  # return new labels
  # lab[setdiff(names(lab), need)]
  #
  # interactive:
  # dcast(dem, sg + hw ~ rim_hw)
  #
  # automated:
  # sapply(names(rim), function(x) dcast(dem, value.var = 'day', fun = length,
  #    as.formula(paste(do.call(paste, c(collapse='+',list(rim[[x]]))),'~',x))
  # ))
  #
  # class:
  # cbind(sapply(dem, class))
}

dem.add.gm <- function(x, id=id, need=NULL) # x <- dem
{
  d <- fread(paste0(id$path$id,'label_gm_2017.csv'))
  # 'label_gm_2017.csv' is incomplete, the following information needs to be 
  # added. See email correspondance with Adriana DeSanots
  # there are 5 duplicated but identical entries # d[d[,.N,k=hh][N!=1], on='hh']
  d <- d[!duplicated(hh)]
  # there are 5 missing homes (nil viewers throughout the whole year)
  k <- data.table(
    hh=c(4193,5194,5440,5558), gm=c(4033,2762,5568,69), plz=c(5507,4123,1450,8304), 
    pmr=c(8,9,2,8))
  for(j in seq_along(k)) set(k, j=j, value=as.integer(k[[j]])) 
  d <- rbindlist(list(d,k))[order(hh)]
  cols <- intersect(c('gm','plz','pmr'), names(d))
  x[, hh := if(pin.type(x)=='ind') as.integer(pin/100) else pin]
  x[, (cols) := d[x, on='hh', cols, with=FALSE]][]
  # there are 3 homes that moved during the year
  m <- data.table(hh=c(569,4681,4692), gm=c(1059,2016,2771), plz=c(6010,1532,4104), 
                  pmr=c(5,2,9), day=c('2017-03-07','2017-12-09','2017-11-28'))
  for(j in which(sapply(m, is.numeric))) set(m, j=j, value=as.integer(m[[j]])) 
  for (i in seq(nrow(m))){ # i = 2
    q <- id$lab$day$dem[label < m[i,day], label] # e.g. no match in 2 semester
    if(length(q))
      x[list(m[i,hh], q), on=c('hh','day'), (cols) := m[i, cols, with=FALSE]]
    # check:
    # x[hh==m$hh[i] & day < m$day[i]][, unique(gm)]
    # x[hh==m$hh[i] & day >= m$day[i]][, unique(gm)]
    # x[, unique(gm), k=hh][,.N,k=hh][N>1]
  }
  x[, hh := NULL]
  # label
  lab <- list(pmr = id.dt(sprintf('pmr%02i',1:14), id.name='pmr'))
  id$lab <- c(id$lab[setdiff(names(id$lab),'pmr')], lab)
  assign('id', id, envir=.GlobalEnv)
  x
}

dem.add.zone <- function(x, id=id, need='gm') 
{
  d <- fread(paste0(id$path$id,'label_zone_2017_tv_kms.csv'))
  setnames(d, 'name', 'gm.name')
  cols <- c('gm.name','sg','tg','wf','ez','kt')
  # label
  # column
  x[, (cols) := d[x, on=c(id='gm'), cols, with=FALSE]][]
}


dem.add.lifestage <- function(x, id=id, need=c('hw','age','hhkids14','hhkids02','occupation')) 
{
  # Lifestage is a control determined by the composition of individuals within 
  # the household, this control takes into account the age of the housewife, 
  # the presence and age of children present as well as the working status of 
  # all householders. This control has proven to be a strong discriminator of 
  # viewing behaviour on the BARB panel [...]. 
  # Source: RSMB, London, 2015, Swiss TAM Panel Control Review.
  #
  # Pre Family:   Housewife aged <45, No Children 0-15 in Home
  # Young Family: Household contains Children 0-3
  # Older Family: Household contains Children 4-15 but none 0-3
  # Post Family:  Housewife aged 45+, No Children 0-15 in Home, at least one 
  #               Household Member working full/part time
  # Inactive:     Housewife aged 45+, No Children 0-15 in Home, no Household
  #               Member working full/part time
  #
  # library(tv)
  # need <- c('hw','age','hhkids14','hhkids02','occupation')
  # setup(to = 100, dem.var = need, import = TRUE, prg = FALSE, view = FALSE)
  # x <- copy(dem)
  
  stopifnot(all(need %in% names(id$lab)))
  stopifnot(all(need %in% names(x)))
  # add label
  
  id.kids <- id.dt.cross(id$lab[c('hhkids14', 'hhkids02')])
  y <- c('kids02','kids14only','nokids')
  id.kids[, `:=` (id = c(1,1,2,3), label = y[c(1,1,2,3)])]
  x[, kids := id.kids[x, on = c('hhkids14', 'hhkids02'), id]] # x[, .N, k=kids]
 
  ordercol(x[, hh := pin %/% 1e2L], 'hh', 'day')
  x[, hwage := age[hw == 1L], by = .(day, hh)][, hwage := as.integer(hwage >= 45L)]
  # There are hh with no hw ! 
  # This happens if the hw drops out and no new hw is assigned immediately
  # x[day == '2013-04-09' & hh==108]
  # x[day == '2013-01-01' & hh==108]
  
  id.active <- id$lab$occupation[, active := 0L][occupation %in% 1:4, active := 1L]
  x[, active := id.active[x, on = 'occupation', active]]
  
  # careful with NAs!
  id.lifestage <- x[, .N, k=.(kids, hwage, active)][, -'N']
  id.lifestage[kids == 3 & hwage == 0,               id := 1L]
  id.lifestage[kids == 1,                            id := 2L]
  id.lifestage[kids == 2,                            id := 3L]
  id.lifestage[kids == 3 & hwage == 1 & active == 1, id := 4L]
  id.lifestage[kids == 3 & hwage == 1 & active == 0, id := 5L]
  y <- id.dt(c(paste(c('pre','young','older','post'),'family'),'inactive'))
  id.lifestage[, label := y[id.lifestage, on='id', label]]
  x[, lifestage := id.lifestage[x, on = c('kids','hwage','active'), id]]
  
  setnames(ordercol(setkey(id.lifestage, id), c('id','label')),'id', 'lifestage')
  id$lab$lifestage <- id.lifestage
  assign('id', id, envir = .GlobalEnv)
  x
}

dem.add.hhcomposition <- function(x, id=id, need=c('age','sex','hhkids14','hhkids02')) 
{
# 1. Family (F): a household that consists of two adults, irrespective of their
#    gender.
# 2. Family with children (FC): a household that consists of two adults, irrespec-
#    tive of their gender, with at least onechild2 (a child is defined by age < 18).
# 3. Household (H): a household that consists of more than two adults.
# 4. Household with children (HC): a household that consists of more than two
#    adults with at least one child.
# 5. Single female (SF): a household with only one adult female.
# 6. Single male (SM): a household with only one adult male.
# 7. Single female parent (SPF3): a household with only one adult female with
#    at least one child.
# 8. Single male parent (SPM4): a household with only one adult male with at 
#    least one child.

  # library(tv)
  # need=c('age','sex,'hhkids14','hhkids02')
  # setup(to = 100, dem.var = need, view = FALSE, prg = FALSE, import = TRUE)
  x <- copy(dem)
  
  stopifnot(all(need %in% names(id$lab)))
  stopifnot(all(need %in% names(x)))
  # add label
  
  id$lab$age[, adult := 0L, ][age >= 18L, adult := 1L]
  x[, adult := id$lab$age[x, on = 'age' , adult]]
  # There is missing age for guests: zB: x[day == '2013-03-03' & hh == 245]
  # But also for panelists: zB: x[day == '2013-04-05' & hh == 2045]
  
  ordercol(x[, hh := pin %/% 1e2L], 'hh', 'day')
  x[, adult.N := sum(adult[!guest] == 1, na.rm = TRUE), by = .(day,hh)]
  x[, kids := as.integer(any(hhkids14 == 1L | hhkids02 == 1L | adult.N == 0)), by = .(day,hh)]
  
  x[adult.N == 2,                        hhcomp := 1L]
  x[adult.N == 2 & kids == 1,            hhcomp := 2L]
  x[adult.N > 2  & kids == 1,            hhcomp := 3L]
  x[adult.N > 2  & kids == 0,            hhcomp := 4L]
  x[adult.N == 1 & sex == 1,             hhcomp := 5L]
  x[adult.N == 1 & sex == 2,             hhcomp := 6L]
  x[adult.N == 1 & sex == 1 & kids == 1, hhcomp := 7L]
  x[adult.N == 1 & sex == 2 & kids == 0, hhcomp := 8L]
  
  # x[, .N, k=hhcomp]
  x[is.na(hhcomp)]
}


  # #___abonement_________________________________________________________________
  # 
  # if(rsmb) {
  #   dem[, c('abolight','abomedium','abopremium','aboteleclub','abo') := 'no']
  #   dem[aboSwissLight=='yes'|aboSwissStart=='yes'|aboSunStart=='yes'|aboUpcCompact=='yes',
  #       abolight := 'yes']
  #   dem[aboSwissBasic=='yes'|aboSunBasic=='yes'|aboUpcClassic=='yes',
  #       abomedium := 'yes']
  #   dem[aboSwissPlus=='yes'|aboSunComfort=='yes'|aboUpcComfort=='yes',
  #       abopremium := 'yes']
  #   #dem[,.N,keyby=.(abopremium,abomedium,abolight)]
  #   dem[aboClubBasic=='yes'|aboClubSport=='yes'|aboClubMovie=='yes'|aboClubFamily=='yes',
  #       aboteleclub := 'yes']
  #   dem[aboteleclub=='yes'|abopremium=='yes'|abomedium=='yes'|abolight=='yes'|
  #         aboSRG=='yes'|aboCineplus=='yes'|aboCanal=='yes'|aboOther=='yes',
  #       abo := 'yes']
  # }
  # 
  # #___nationality______________________________________________________________
  # 
  # if(rsmb) {
  #   # nms = names(dem)[grep('nat',names(dem))]
  #   dem[natItaly=='yes'|natPortugal=='yes'|natSpain=='yes'|natCroatia=='yes'|
  #         natSerbia=='yes'|natMacedonia=='yes'|natKosovo=='yes'|natBosnia=='yes'|
  #         natTurkey=='yes'|natSrilanka=='yes'|natBrasil=='yes'|natOther=='yes',
  #       natsouth := 'yes']
  #   #dem[,.N,natsouth]
  #   dem[natGermany=='yes'|natFrance=='yes'|natUK=='yes'|natAustria=='yes'|
  #         natNetherlands=='yes'|natUSA=='yes',
  #       natnorth := 'yes']
  #   #, which is dem[,.N,natnorth]
  #   dem[natSwiss=='yes', X := 'swiss']
  #   dem[natsouth=='yes', X := 'south']
  #   dem[natnorth=='yes', X := 'north']
  #   dem[natSwiss=='yes' & natsouth=='yes', X := 'double_south']
  #   dem[natSwiss=='yes' & natnorth=='yes', X := 'double_north']
  #   dem[,.N,X]
  #   # nms = names(dem)[grep('nat',names(dem))]
  #   # dem[,(nms):=NULL]
  #   setnames(dem,'X','nationality')
  # }
  # 
  # 
