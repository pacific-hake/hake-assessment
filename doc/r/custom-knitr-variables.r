## Allotments by country
can.allotment.percent <- 26.12
us.allotment.percent <- 73.88

## Attainment, used in the management performance section
usa.last.5.years.attainment <- fmt0(mean(landings.vs.tac[landings.vs.tac$Year %in% (end.yr-5):(end.yr-1),8]), 1)
can.last.5.years.attainment <- fmt0(mean(landings.vs.tac[landings.vs.tac$Year %in% (end.yr-5):(end.yr-1),9]), 1)
tot.last.5.years.attainment <- fmt0(mean(landings.vs.tac[landings.vs.tac$Year %in% (end.yr-5):(end.yr-1),10]), 1)
tot.last.10.years.attainment <- fmt0(mean(landings.vs.tac[landings.vs.tac$Year %in% (end.yr-10):(end.yr-1),10]), 1)
tot.last.year.attainment <- fmt0(mean(landings.vs.tac[landings.vs.tac$Year == (end.yr-1),"ATTAIN"]), 1)

if(verbose){
  cat("DEBUG: Catches\n\n")
}
## Recent catches
last.5.years.of.catch.data <- (max(catches$Year)-4):max(catches$Year)
last.5.years.total.catch <- catches[catches$Year %in% last.5.years.of.catch.data, "TOTAL"]
long.term.avge.catch <- mean(catches$TOTAL)
last.5.years.above.avge <- last.5.years.of.catch.data[last.5.years.total.catch > long.term.avge.catch]
last.5.years.below.avge <- last.5.years.of.catch.data[last.5.years.total.catch < long.term.avge.catch]

if(verbose){
  cat("DEBUG: Last year's values\n\n")
}
## last year's values (mostly for the one-page-summary and introduction)
last.year.landings <- fmt0(as.numeric(landings.vs.tac[landings.vs.tac$Year %in% (end.yr-1),]$TOTAL), 0)
last.year.tac <- fmt0(landings.vs.tac[landings.vs.tac$Year %in% (end.yr-1),]$TAC)
last.year.attained <- fmt0(as.numeric(landings.vs.tac[landings.vs.tac$Year %in% (end.yr-1),]$ATTAIN), 1)

last.year.us.attained <- fmt0(as.numeric(landings.vs.tac[landings.vs.tac$Year %in% (end.yr-1),]$USATTAIN), 1)
last.year.us.not.attained <- fmt0(as.numeric(100 - landings.vs.tac[landings.vs.tac$Year %in% (end.yr-1),]$USATTAIN), 1)
last.year.us.not.attained.tonnes <- filter(landings.vs.tac, Year == last.data.yr)$TACUSA - filter(landings.vs.tac, Year == last.data.yr)$Ustotal
last.year.us.tac <- landings.vs.tac[landings.vs.tac$Year %in% (end.yr-1),]$TACUS
## Not doing fmt0 here since want to do further calculations
last.year.us.tribal <- filter(further.tac, Year == last.data.yr)$us.tribal.quota
last.year.us.research <- filter(further.tac, Year == last.data.yr)$us.research.quota
last.year.us.non.tribal <- filter(further.tac, Year == last.data.yr)$us.nontribal.quota
last.year.us.tribal.quota.reallocated <- filter(further.tac, Year == last.data.yr)$us.tribal.quota.reallocated
last.year.us.tribal.reallocate.dates <- filter(further.tac, Year == last.data.yr)$us.tribal.reallocate.dates
last.year.us.tribal.max.landed <- filter(further.tac, Year == last.data.yr)$us.tribal.max.landed
last.year.us.shore.quota.reallocated <- filter(further.tac, Year == last.data.yr)$us.shore.reallocated
last.year.us.cp.quota.reallocated <- filter(further.tac, Year == last.data.yr)$us.cp.reallocated
last.year.us.ms.quota.reallocated <- filter(further.tac, Year == last.data.yr)$us.ms.reallocated

last.year.can.carryover <- fmt0(filter(further.tac, Year == last.data.yr)$can.carried.over)
last.year.can.attained <- fmt0(as.numeric(landings.vs.tac[landings.vs.tac$Year %in% (end.yr-1),]$CANATTAIN), 1)   # the percentage
last.year.can.landings <- fmt0(as.numeric(landings.vs.tac[landings.vs.tac$Year %in% (end.yr-1),]$CANtotal))
last.year.can.tac <- fmt0(landings.vs.tac[landings.vs.tac$Year %in% (end.yr-1),]$TACCAN)
latest.year.can.jv <- max(filter(catches, CAN_JV > 0)$Year)  # latest year of JV in Canada
last.year.can.shore <- fmt0(filter(catches, Year == last.data.yr)$CAN_Shoreside)
last.year.can.freezer <- fmt0(filter(catches, Year == last.data.yr)$CAN_FreezeTrawl)
last.year.can.shore.percent <- fmt0(filter(catches, Year == last.data.yr)$CAN_Shoreside /
                                    as.numeric(landings.vs.tac[landings.vs.tac$Year %in% (end.yr-1),]$CANtotal) * 100.0, 1)
last.year.can.freezer.percent <- fmt0(filter(catches, Year == last.data.yr)$CAN_FreezeTrawl /
                                      as.numeric(landings.vs.tac[landings.vs.tac$Year %in% (end.yr-1),]$CANtotal) * 100.0, 1)
years.Can.JV.catch.eq.0.recent = years.Can.JV.catch.eq.0(catches)

## Survey values
survey.biomass <- survey.history$biomass
names(survey.biomass) <- as.character(survey.history$year)
survey.comps <- base.model$dat$agecomp[base.model$dat$agecomp$FltSvy==2,]
rownames(survey.comps) <- survey.comps$Yr
## Survey extrapolation percentages and years
survey.extrap.percent <- 100 * (survey.comparison$with.extrap - survey.comparison$no.extrap) / survey.comparison$with.extrap
names(survey.extrap.percent) <- as.character(survey.comparison$year)
survey.extrap.percent <- survey.extrap.percent[!is.na(survey.extrap.percent)]

survey.largest.extrap.percent <- fmt0(max(survey.extrap.percent), 2)
survey.year.of.largest.extrap <- names(survey.extrap.percent[survey.extrap.percent == max(survey.extrap.percent)])

survey.smallest.extrap.percent <- fmt0(min(survey.extrap.percent), 2)
survey.year.of.smallest.extrap <- names(survey.extrap.percent[survey.extrap.percent == min(survey.extrap.percent)])

survey.average.extrap.percent <- fmt0(mean(survey.extrap.percent), 2)



## New depletion and spawning biomass estimates
if(verbose){
  cat("DEBUG: New depletion and spawning biomass estimates\n\n")
}
curr.depl.lower <- fmt0(base.model$mcmccalcs$dlower[names(base.model$mcmccalcs$dlower) %in% end.yr] * 100, 1)
curr.depl.median <- fmt0(base.model$mcmccalcs$dmed[names(base.model$mcmccalcs$dmed) %in% end.yr] * 100, 1)
curr.depl.upper <- fmt0(base.model$mcmccalcs$dupper[names(base.model$mcmccalcs$dupper) %in% end.yr] * 100, 1)

curr.bio.lower <- fmt0(base.model$mcmccalcs$slower[names(base.model$mcmccalcs$slower) %in% end.yr], 3)
curr.bio.median <- fmt0(base.model$mcmccalcs$smed[names(base.model$mcmccalcs$smed) %in% end.yr], 3)
curr.bio.upper <- fmt0(base.model$mcmccalcs$supper[names(base.model$mcmccalcs$supper) %in% end.yr], 3)

## Estimates of spawning biomass for previous year (calculated in this assessment):
prev.bio.lower <- fmt0(base.model$mcmccalcs$slower[names(base.model$mcmccalcs$slower) %in% last.data.yr], 3)      # last.data.yr = end.yr-1
prev.bio.median <- fmt0(base.model$mcmccalcs$smed[names(base.model$mcmccalcs$smed) %in% last.data.yr], 3)
prev.bio.upper <- fmt0(base.model$mcmccalcs$supper[names(base.model$mcmccalcs$supper) %in% last.data.yr], 3)


## First forecast year depletion and spawning biomass estimates
if(verbose){
  cat("DEBUG: First forecast year depletion and spawning biomass estimates\n\n")
}
fore.tac.mcmc <- base.model$forecasts$mcmccalcs[[catch.tac.ind]]
next.depl.lower.tac.based <- fmt0(fore.tac.mcmc$dlower[names(fore.tac.mcmc$dlower) %in% (end.yr + 1)] * 100, 1)
next.depl.median.tac.based <- fmt0(fore.tac.mcmc$dmed[names(fore.tac.mcmc$dmed) %in% (end.yr + 1)] * 100, 1)
next.depl.upper.tac.based <- fmt0(fore.tac.mcmc$dupper[names(fore.tac.mcmc$dupper) %in% (end.yr + 1)] * 100, 1)

next.bio.lower.tac.based <- fmt0(fore.tac.mcmc$slower[names(fore.tac.mcmc$slower) %in% (end.yr + 1)] * 100, 1)
next.bio.median.tac.based <- fmt0(fore.tac.mcmc$smed[names(fore.tac.mcmc$smed) %in% (end.yr + 1)] * 100, 1)
next.bio.upper.tac.based <- fmt0(fore.tac.mcmc$supper[names(fore.tac.mcmc$supper) %in% (end.yr + 1)] * 100, 1)

## Calculations for exec summary and assessment-section.rnw:
##  number of mcmc samples, minimum median biomass,
##  years when fishing intensity > 1
if(verbose){
  cat("DEBUG: Calculations for exec summary and assessment-section.rnw:\n\n")
}
num.mcmc.samples <- dim(base.model$mcmc)[1]
median.bio.min  <- fmt0(min(base.model$mcmccalcs$smed), 3)  # min median biomass
median.bio.min.year <- names(which.min(base.model$mcmccalcs$smed)) # year of min
median.intensity <- base.model$mcmccalcs$pmed
median.intensity.2007.to.2011 <- median.intensity[c("2007", "2008", "2009", "2010", "2011")]
median.intensity.2007.to.2011.min <- fmt0(min(median.intensity.2007.to.2011)*100, 0)
median.intensity.2007.to.2011.max <- fmt0(max(median.intensity.2007.to.2011)*100, 0)
# Could replace some of the following ..pmed with median.intensity:
median.intensity.above.one.all.years <- names(which(base.model$mcmccalcs$pmed > 1))    # includes > end.yr
median.intensity.above.one.years <- median.intensity.above.one.all.years[
         median.intensity.above.one.all.years < end.yr]  # ones to mention
median.intensity.2010 <- fmt0(base.model$mcmccalcs$pmed["2010"] * 100, 1)
median.intensity.penult.yr <- fmt0(base.model$mcmccalcs$pmed[as.character(end.yr-1)] * 100, 1)

median.relative.bio <- base.model$mcmccalcs$dmed
median.relative.bio.2007.to.2011 <- median.relative.bio[c("2007", "2008", "2009", "2010", "2011")]
median.relative.bio.2007.to.2011.min <- fmt0(min(median.relative.bio.2007.to.2011), 2)
median.relative.bio.2007.to.2011.max <- fmt0(max(median.relative.bio.2007.to.2011), 2)
median.relative.bio.below.target <- median.relative.bio[median.relative.bio < 0.4]     # when below target
median.relative.bio.above.target.since <- as.numeric(max(names(median.relative.bio.below.target)))+1   # has been above target since
# Prob biomass declines next year to year after with zero catch:
zero.catch.prob.bio.down.1 <- fmt0(base.model$risks[[1]][1,2])
# Prob biomass declines year after next to year after that with zero catch:
zero.catch.prob.bio.down.2 <- fmt0(base.model$risks[[2]][1,2])



## Second forecast year depletion and spawning biomass estimates
if(verbose){
  cat("DEBUG: Second forecast year depletion and spawning biomass estimates\n\n")
}
next2.depl.lower.tac.based <- fmt0(fore.tac.mcmc$dlower[names(fore.tac.mcmc$dlower) %in% (end.yr + 2)] * 100, 1)
next2.depl.median.tac.based <- fmt0(fore.tac.mcmc$dmed[names(fore.tac.mcmc$dmed) %in% (end.yr + 2)] * 100, 1)
next2.depl.upper.tac.based <- fmt0(fore.tac.mcmc$dupper[names(fore.tac.mcmc$dupper) %in% (end.yr + 2)] * 100, 1)

next2.bio.lower.tac.based <- fmt0(fore.tac.mcmc$slower[names(fore.tac.mcmc$slower) %in% (end.yr + 2)] * 100, 1)
next2.bio.median.tac.based <- fmt0(fore.tac.mcmc$smed[names(fore.tac.mcmc$smed) %in% (end.yr + 2)] * 100, 1)
next2.bio.upper.tac.based <- fmt0(fore.tac.mcmc$supper[names(fore.tac.mcmc$supper) %in% (end.yr + 2)] * 100, 1)

## number.to.word function located in utilities.r
catches.below.200000.since.1986 <- number.to.word(length(filter(catches, TOTAL <= 200000, Year > 1986)$Year))

## Age composition data for data section
survey.age.years <- base.model$dat$agecomp[base.model$dat$agecomp$FltSvy == 2,]$Yr
max.survey.age.prop <- make.age.comp.bubble.plot(base.model,
                                                 subplot = 2,
                                                 do.plot = FALSE)
max.fishery.age.prop <- make.age.comp.bubble.plot(base.model,
                                                  subplot = 1,
                                                  do.plot = FALSE)

catch.limit.quantiles <- fmt0(make.forecast.catch.posterior.plot(base.model,
                                   fore.yr = end.yr, do.plot = FALSE) * 1000)
                # 2.5%, median and 97.5% quantiles of catch limit for assess.yr
                #  using the default harvest policy; tonnes

## Estimated numbers at age for fishery for Recruitment section in Exec Summary and main text
##  From make.age.comp.fit.plot() which in turn calls age.fits()
if(verbose){
  cat("DEBUG: Estimated numbers at age for fishery for Recruitment section\n\n")
}
fishery.estimated.age.comp <- base.model$agedbase[base.model$agedbase$Fleet==1,]  #I think that this has ageing error incorporated
year.class.2010.in.2013 <- fmt0(filter(fishery.estimated.age.comp, Yr==2013, Bin==3)$Exp * 100)
year.class.2010.in.2014 <- fmt0(filter(fishery.estimated.age.comp, Yr==2014, Bin==4)$Exp * 100)
year.class.2010.in.2015 <- fmt0(filter(fishery.estimated.age.comp, Yr==2015, Bin==5)$Exp * 100)

tmp <- base.model$catage[base.model$catage$Fleet==1,-(1:10)]  #This does not have ageing error
fishery.estimated.age.comp <- cbind(base.model$catage[base.model$catage$Fleet==1,(1:10)],t(apply(tmp,1,function(x){x/sum(x)})))
year.class.2010.in.2013 <- fmt0(filter(fishery.estimated.age.comp, Yr==2013)$"3" * 100)
year.class.2010.in.2014 <- fmt0(filter(fishery.estimated.age.comp, Yr==2014)$"4" * 100)
year.class.2010.in.2015 <- fmt0(filter(fishery.estimated.age.comp, Yr==2015)$"5" * 100)

catcher.processor.catch <- fmt0(100 * filter(catches, Year == last.data.yr)$atSea_US_CP / (last.year.us.cp.quota.reallocated), 1)
mothership.catch <- fmt0(100 * filter(catches, Year == last.data.yr)$atSea_US_MS / (last.year.us.ms.quota.reallocated), 1)
shore.based.catch <- fmt0(100 * filter(catches, Year == last.data.yr)$US_shore / (last.year.us.shore.quota.reallocated), 1)

## Canadian age data variables
if(verbose){
  cat("DEBUG: Canadian age data variables\n\n")
}
get.age.prop <- function(vec, place = 1){
  ## returns the age prop and the age itself for the place,
  ## where place is 1=max, 2-second highest, etc.
  prop <- rev(sort(vec))
  prop <- prop[place]
  age <- as.numeric(names(vec[vec == prop]))
  return(c(age, prop))
}

## Canadian Freezer trawlers
if(verbose){
  cat("DEBUG: Canadian Freezer trawlers\n\n")
}
last.year.can.ages.ft <- can.ages[[2]][rownames(can.ages[[2]]) == last.data.yr,]
get.age.prop(last.year.can.ages.ft, 1)
ft.age.prop.holder <- get.age.prop(last.year.can.ages.ft, 1)
max.freezer.trawler.age.prop.age <- ft.age.prop.holder[1]
max.freezer.trawler.age.prop <- fmt0(ft.age.prop.holder[2] * 100, 1)
ft.age.prop.holder <- get.age.prop(last.year.can.ages.ft, 2)
second.freezer.trawler.age.prop.age <- ft.age.prop.holder[1]
second.freezer.trawler.age.prop <- fmt0(ft.age.prop.holder[2] * 100, 1)
ft.age.prop.holder <- get.age.prop(last.year.can.ages.ft, 3)
third.freezer.trawler.age.prop.age <- ft.age.prop.holder[1]
third.freezer.trawler.age.prop <- fmt0(ft.age.prop.holder[2] * 100, 1)
ft.age.prop.holder <- get.age.prop(last.year.can.ages.ft, 4)
fourth.freezer.trawler.age.prop.age <- ft.age.prop.holder[1]
fourth.freezer.trawler.age.prop <- fmt0(ft.age.prop.holder[2] * 100, 1)
## Canadian Shoreside
if(verbose){
  cat("DEBUG: Canadian Shoreside\n\n")
}
last.year.can.ages.ss <- can.ages[[1]][rownames(can.ages[[1]]) == last.data.yr,]
get.age.prop(last.year.can.ages.ss, 1)
ss.age.prop.holder <- get.age.prop(last.year.can.ages.ss, 1)
max.shoreside.age.prop.age <- ss.age.prop.holder[1]
max.shoreside.age.prop <- fmt0(ss.age.prop.holder[2] * 100, 1)
ss.age.prop.holder <- get.age.prop(last.year.can.ages.ss, 2)
second.shoreside.age.prop.age <- ss.age.prop.holder[1]
second.shoreside.age.prop <- fmt0(ss.age.prop.holder[2] * 100, 1)
ss.age.prop.holder <- get.age.prop(last.year.can.ages.ss, 3)
third.shoreside.age.prop.age <- ss.age.prop.holder[1]
third.shoreside.age.prop <- fmt0(ss.age.prop.holder[2] * 100, 1)
ss.age.prop.holder <- get.age.prop(last.year.can.ages.ss, 4)
fourth.shoreside.age.prop.age <- ss.age.prop.holder[1]
fourth.shoreside.age.prop <- fmt0(ss.age.prop.holder[2] * 100, 1)

## Years for which median recruitment is below the mean of the median
##  recruitments for years >2010 and <(end.yr-1) ; end.yr-1 won't be
##  well estimated
recruitment.med.since.2010 <- base.model$mcmccalcs$rmed[ which(as.numeric(names(base.model$mcmccalcs$rmed)) > 2010 & as.numeric(names(base.model$mcmccalcs$rmed)) < (end.yr-1))]
years.since.2010.recruitment.med.below.mean <- names(recruitment.med.since.2010[recruitment.med.since.2010  < mean(base.model$mcmccalcs$rmed)])

## Exploitation values
if(verbose){
  cat("DEBUG: Exploitation values\n\n")
}
exploitation.med.2010 <- fmt0(base.model$mcmccalcs$fmed["2010"],2)
exploitation.med.penult.yr <- fmt0(base.model$mcmccalcs$fmed[as.character(end.yr-1)],2)

## Survey comparisons of biomass from year to year. Use the table, not the value of survey.end.year
## Next year, we should set survey.end.yr to be what is in the table. Not going to attempt it
##  with only hours left to submission.
last.survey.year <- survey.history[nrow(survey.history),]$year
last.survey.year.biomass <- fmt0(survey.history[nrow(survey.history),]$biomass * 10, 2) ## millions of tonnes
penult.survey.year <- survey.history[nrow(survey.history) - 1,]$year
penult.survey.year.biomass <- fmt0(survey.history[nrow(survey.history) - 1,]$biomass * 10, 2)
antepenult.survey.year <- survey.history[nrow(survey.history) - 2,]$year
antepenult.survey.year.biomass <- fmt0(survey.history[nrow(survey.history) - 2,]$biomass * 10, 2)
## How many times higher is the last survey than the one before it?
last.factor.penult <- fmt0(survey.history[nrow(survey.history),]$biomass / survey.history[nrow(survey.history) - 1,]$biomass, 1)
## How many times higher is the last survey than the one that was two before it?
last.factor.antepenult <- fmt0(survey.history[nrow(survey.history),]$biomass / survey.history[nrow(survey.history) - 2,]$biomass, 1)

## Get priors informtaion
split.prior.info <- function(prior.str, dec.points = 1, first.to.lower = FALSE){
  ## Parses a string like Lognormal(2.0,1.01) and returns a vector of length 3:
  ## "Lognormal", 2.0, 1.01
  ## if first.to.lower = TRUE, makes the first letter of the name of the prior lower case.
  p <- strsplit(prior.str, "\\(")[[1]]
  if(first.to.lower){
    ## Make the name of the prior lower case
    p[1] <- paste0(tolower(substr(p[1], 1, 1)), substr(p[1], 2, nchar(p[1])))
  }
  p.type <- p[1]
  p <- strsplit(p[2], ",")[[1]]
  p.mean <- fmt0(as.numeric(p[1]), dec.points)
  p.sd <- fmt0(as.numeric(gsub(")", "", p[2])), dec.points)
  return(c(p.type, p.mean, p.sd))
}
param.details <- make.parameters.estimated.summary.table(base.model,
                                                         start.rec.dev.yr = recruit.dev.start.yr,
                                                         end.rec.dev.yr = end.yr,
                                                         return.xtable = FALSE)
m.prior <- split.prior.info(param.details[rownames(param.details) == "m.vals",][4], dec.points = 2, first.to.lower = TRUE)
## Now use m.prior[1] for name of prior, m.prior[1] for mean, and m.prior[3] for SD.

cohortCatch <- function(cohort,catage,ages=0:20) {
  cohortYrs <- cohort+ages
  tmp <- as.matrix(catage[catage$Yr %in% cohortYrs,as.character(ages)])
  wtatage <- as.matrix(base.model$wtatage[base.model$wtatage$fleet==1 & base.model$wtatage$yr %in% (-1*cohortYrs),paste("X",ages,sep="")])
  catchWtAtAge <- tmp * wtatage

  ind <- 1:(nrow(tmp)+1)
  if(length(ind) > length(ages)) {ind <- 1:nrow(tmp)}
  catchCoh <- diag(catchWtAtAge[,ind])
  names(catchCoh) <- cohortYrs[1:(nrow(tmp))]
  return(catchCoh)
}
cohort.catch.1999 <- sum(cohortCatch(1999,base.model$catage))
cohort.catch.2010 <- sum(cohortCatch(2010,base.model$catage))
