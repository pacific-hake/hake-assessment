## Put any variables you intend to use in the text here.
## The function f() is for formatting and is defined in
##  r-functions/utilities.r

################################################################################
can.allotment.percent <- 26.12
us.allotment.percent <- 73.88

################################################################################
## Attainment, used in the management performance section
usa.last.5.years.attainment <- f(mean(landings.vs.tac[landings.vs.tac$Year %in% (end.yr-5):(end.yr-1),8]), 1)
can.last.5.years.attainment <- f(mean(landings.vs.tac[landings.vs.tac$Year %in% (end.yr-5):(end.yr-1),9]), 1)
tot.last.5.years.attainment <- f(mean(landings.vs.tac[landings.vs.tac$Year %in% (end.yr-5):(end.yr-1),10]), 1)
tot.last.10.years.attainment <- f(mean(landings.vs.tac[landings.vs.tac$Year %in% (end.yr-10):(end.yr-1),10]), 1)
tot.last.year.attainment <- f(mean(landings.vs.tac[landings.vs.tac$Year == (end.yr-1),"ATTAIN"]), 1)
tot.2015.attainment <- f(landings.vs.tac[landings.vs.tac$Year == 2015, "ATTAIN"], 1)

################################################################################
## Recent catches
last.5.years.of.catch.data <- (max(catches$Year)-4):max(catches$Year)
last.5.years.total.catch <- catches[catches$Year %in% last.5.years.of.catch.data, "TOTAL"]
long.term.avge.catch <- mean(catches$TOTAL)
last.5.years.above.avge <- last.5.years.of.catch.data[last.5.years.total.catch > long.term.avge.catch]
last.5.years.below.avge <- last.5.years.of.catch.data[last.5.years.total.catch < long.term.avge.catch]

################################################################################
## last year's values (mostly for the one-page-summary and introduction)
last.year.landings <- f(as.numeric(landings.vs.tac[landings.vs.tac$Year %in% (end.yr-1),]$TOTAL), 0)
last.year.tac <- f(landings.vs.tac[landings.vs.tac$Year %in% (end.yr-1),]$TAC)
last.year.attained <- f(as.numeric(landings.vs.tac[landings.vs.tac$Year %in% (end.yr-1),]$ATTAIN), 1)

################################################################################
## US landings, TAC, and attainments
last.year.us.landings <- f(as.numeric(landings.vs.tac[landings.vs.tac$Year %in% (end.yr-1),]$Ustotal))
last.year.us.attained <- f(as.numeric(landings.vs.tac[landings.vs.tac$Year %in% (end.yr-1),]$USATTAIN), 1)
last.2year.us.attained.diff <- f(as.numeric(landings.vs.tac[landings.vs.tac$Year %in% (end.yr-1),]$USATTAIN - landings.vs.tac[landings.vs.tac$Year %in% (end.yr-2),]$USATTAIN), 0)
last.year.us.not.attained <- f(as.numeric(100 - landings.vs.tac[landings.vs.tac$Year %in% (end.yr-1),]$USATTAIN), 1)
last.year.us.not.attained.tonnes <- filter(landings.vs.tac, Year == last.data.yr)$TACUSA - filter(landings.vs.tac, Year == last.data.yr)$Ustotal
last.year.us.tac <- f(as.numeric(landings.vs.tac[landings.vs.tac$Year %in% (end.yr-1),]$TACUSA))
## Not doing f here since want to do further calculations
last.year.us.tribal <- filter(further.tac, Year == last.data.yr)$us.tribal.quota
last.year.us.research <- filter(further.tac, Year == last.data.yr)$us.research.quota
last.year.us.non.tribal <- filter(further.tac, Year == last.data.yr)$us.nontribal.quota
last.year.us.tribal.quota.reallocated <- filter(further.tac, Year == last.data.yr)$us.tribal.quota.reallocated
last.year.us.tribal.reallocate.dates <- filter(further.tac, Year == last.data.yr)$us.tribal.reallocate.dates
last.year.us.tribal.max.landed <- filter(further.tac, Year == last.data.yr)$us.tribal.max.landed
last.year.us.shore.quota.reallocated <- filter(further.tac, Year == last.data.yr)$us.shore.reallocated
last.year.us.cp.quota.reallocated <- filter(further.tac, Year == last.data.yr)$us.cp.reallocated
last.year.us.ms.quota.reallocated <- filter(further.tac, Year == last.data.yr)$us.ms.reallocated

################################################################################
## Last year US catches by fleet
last.year.us.research.catch <- filter(catches, Year == last.data.yr)$USresearch
last.year.us.cp.catch <- filter(catches, Year == last.data.yr)$atSea_US_CP
last.year.us.ms.catch <- filter(catches, Year == last.data.yr)$atSea_US_MS
last.year.us.shore.catch <- filter(catches, Year == last.data.yr)$US_shore
## Last year US percent of TAC caught by fleet
last.year.us.research.catch.percent <- f(last.year.us.research.catch / last.year.us.research * 100, 1)
last.year.us.cp.catch.percent <- f(last.year.us.cp.catch / last.year.us.cp.quota.reallocated * 100, 1)
last.year.us.ms.catch.percent <- f(last.year.us.ms.catch / last.year.us.ms.quota.reallocated * 100, 1)
last.year.us.shore.catch.percent <- f(last.year.us.shore.catch / last.year.us.shore.quota.reallocated * 100, 1)
last.year.us.tribal.catch.percent <- f(last.year.us.tribal.max.landed / last.year.us.tribal.quota.reallocated * 100, 1)

################################################################################
## Last year Canadian catch and TAC
last.year.can.carryover <- f(filter(further.tac, Year == last.data.yr)$can.carried.over)
last.year.can.attained <- f(as.numeric(landings.vs.tac[landings.vs.tac$Year %in% (end.yr-1),]$CANATTAIN), 1)   # the percentage
last.2year.can.attained.diff <- f(as.numeric(landings.vs.tac[landings.vs.tac$Year %in% (end.yr-1),]$CANATTAIN - landings.vs.tac[landings.vs.tac$Year %in% (end.yr-2),]$CANATTAIN), 0)
last.year.can.landings <- f(as.numeric(landings.vs.tac[landings.vs.tac$Year %in% (end.yr-1),]$CANtotal))
last.year.can.tac <- f(landings.vs.tac[landings.vs.tac$Year %in% (end.yr-1),]$TACCAN)
last.year.can.tac.jv <- f(filter(further.tac, Year == last.data.yr)$can.jv.tac)
last.year.can.shoreside.tac <- f(landings.vs.tac[landings.vs.tac$Year %in% (end.yr-1),]$TACCAN - filter(further.tac, Year == last.data.yr)$can.jv.tac)

latest.year.can.jv <- max(filter(catches, CAN_JV > 0)$Year)  # latest year of JV in Canada
last.year.can.shore <- f(filter(catches, Year == last.data.yr)$CAN_Shoreside)
last.year.can.freezer <- f(filter(catches, Year == last.data.yr)$CAN_FreezeTrawl)
last.year.can.jv <- f(filter(catches, Year == last.data.yr)$CAN_JV)
last.year.can.shore.percent <- f(filter(catches, Year == last.data.yr)$CAN_Shoreside /
                                    as.numeric(landings.vs.tac[landings.vs.tac$Year %in% (end.yr-1),]$CANtotal) * 100.0, 1)
last.year.can.freezer.percent <- f(filter(catches, Year == last.data.yr)$CAN_FreezeTrawl /
                                      as.numeric(landings.vs.tac[landings.vs.tac$Year %in% (end.yr-1),]$CANtotal) * 100.0, 1)
last.year.can.jv.percent <- f(filter(catches, Year == last.data.yr)$CAN_JV /
                                    as.numeric(landings.vs.tac[landings.vs.tac$Year %in% (end.yr-1),]$CANtotal) * 100.0, 1)
years.Can.JV.catch.eq.0.recent <- years.Can.JV.catch.eq.0(catches)

################################################################################
## Survey values
survey.biomass <- survey.history$biomass
names(survey.biomass) <- as.character(survey.history$year)
survey.comps <- base.model$dat$agecomp[base.model$dat$agecomp$FltSvy==2,]
rownames(survey.comps) <- survey.comps$Yr
## Survey extrapolation percentages and years
survey.extrap.percent <- 100 * (survey.comparison$with.extrap - survey.comparison$no.extrap) / survey.comparison$with.extrap
names(survey.extrap.percent) <- as.character(survey.comparison$year)
survey.extrap.percent <- survey.extrap.percent[!is.na(survey.extrap.percent)]
survey.largest.extrap.percent <- f(max(survey.extrap.percent), 2)
survey.year.of.largest.extrap <- names(survey.extrap.percent[survey.extrap.percent == max(survey.extrap.percent)])
survey.smallest.extrap.percent <- f(min(survey.extrap.percent), 2)
survey.year.of.smallest.extrap <- names(survey.extrap.percent[survey.extrap.percent == min(survey.extrap.percent)])
survey.average.extrap.percent <- f(mean(survey.extrap.percent), 2)

################################################################################
## New depletion and spawning biomass estimates
curr.depl.lower <- f(base.model$mcmccalcs$dlower[names(base.model$mcmccalcs$dlower) %in% end.yr] * 100, 1)
curr.depl.median <- f(base.model$mcmccalcs$dmed[names(base.model$mcmccalcs$dmed) %in% end.yr] * 100, 1)
curr.depl.upper <- f(base.model$mcmccalcs$dupper[names(base.model$mcmccalcs$dupper) %in% end.yr] * 100, 1)
curr.bio.lower <- f(base.model$mcmccalcs$slower[names(base.model$mcmccalcs$slower) %in% end.yr], 3)
curr.bio.median <- f(base.model$mcmccalcs$smed[names(base.model$mcmccalcs$smed) %in% end.yr], 3)
curr.bio.upper <- f(base.model$mcmccalcs$supper[names(base.model$mcmccalcs$supper) %in% end.yr], 3)

################################################################################
## Estimates of spawning biomass for previous year (calculated in this assessment):
prev.bio.lower <- f(base.model$mcmccalcs$slower[names(base.model$mcmccalcs$slower) %in% last.data.yr], 3)      # last.data.yr = end.yr-1
prev.bio.median <- f(base.model$mcmccalcs$smed[names(base.model$mcmccalcs$smed) %in% last.data.yr], 3)
prev.bio.upper <- f(base.model$mcmccalcs$supper[names(base.model$mcmccalcs$supper) %in% last.data.yr], 3)

################################################################################
## First forecast year depletion and spawning biomass estimates
fore.tac.mcmc <- base.model$forecasts$mcmccalcs[[catch.tac.ind]]
next.depl.lower.tac.based <- f(fore.tac.mcmc$dlower[names(fore.tac.mcmc$dlower) %in% (end.yr + 1)] * 100, 1)
next.depl.median.tac.based <- f(fore.tac.mcmc$dmed[names(fore.tac.mcmc$dmed) %in% (end.yr + 1)] * 100, 1)
next.depl.upper.tac.based <- f(fore.tac.mcmc$dupper[names(fore.tac.mcmc$dupper) %in% (end.yr + 1)] * 100, 1)
next.bio.lower.tac.based <- f(fore.tac.mcmc$slower[names(fore.tac.mcmc$slower) %in% (end.yr + 1)] * 100, 1)
next.bio.median.tac.based <- f(fore.tac.mcmc$smed[names(fore.tac.mcmc$smed) %in% (end.yr + 1)] * 100, 1)
next.bio.upper.tac.based <- f(fore.tac.mcmc$supper[names(fore.tac.mcmc$supper) %in% (end.yr + 1)] * 100, 1)

################################################################################
## Calculations for executive-summary.rnw and assessment-section.rnw:
##  number of mcmc samples, minimum median biomass,
##  years when fishing intensity > 1
num.mcmc.samples <- dim(base.model$mcmc)[1]
median.bio.min  <- f(min(base.model$mcmccalcs$smed[names(base.model$mcmccalcs$smed) %in% start.yr:end.yr]), 3)  # min median biomass
median.bio.min.year <- names(which.min(base.model$mcmccalcs$smed[names(base.model$mcmccalcs$smed) %in% start.yr:end.yr])) # year of min
median.intensity <- base.model$mcmccalcs$pmed
median.intensity.2007.to.2011 <- median.intensity[c("2007", "2008", "2009", "2010", "2011")]
median.intensity.2007.to.2011.min <- f(min(median.intensity.2007.to.2011)*100, 0)
median.intensity.2007.to.2011.max <- f(max(median.intensity.2007.to.2011)*100, 0)
# Could replace some of the following ..pmed with median.intensity:
median.intensity.above.one.all.years <- names(which(base.model$mcmccalcs$pmed > 1))    # includes > end.yr
median.intensity.above.one.years <- median.intensity.above.one.all.years[
         median.intensity.above.one.all.years < end.yr]  # ones to mention
median.intensity.2010 <- f(base.model$mcmccalcs$pmed["2010"] * 100, 1)
median.intensity.2015 <- f(base.model$mcmccalcs$pmed["2015"] * 100, 1)
median.intensity.2017 <- f(base.model$mcmccalcs$pmed["2017"] * 100, 1)
median.intensity.2018 <- f(base.model$mcmccalcs$pmed["2018"] * 100, 1)
median.intensity.penult.yr <- f(base.model$mcmccalcs$pmed[as.character(end.yr-1)] * 100, 1)
median.relative.bio <- base.model$mcmccalcs$dmed
median.relative.bio.2007.to.2011 <- median.relative.bio[c("2007", "2008", "2009", "2010", "2011")]
median.relative.bio.2007.to.2011.min <- f(min(median.relative.bio.2007.to.2011), 2)
median.relative.bio.2007.to.2011.max <- f(max(median.relative.bio.2007.to.2011), 2)
median.relative.bio.below.target <- median.relative.bio[names(median.relative.bio) %in% start.yr:end.yr & median.relative.bio < 0.4]     # when below target
median.relative.bio.above.target.since <- max(as.numeric(names(median.relative.bio.below.target)),na.rm=T)+1   # has been above target since

################################################################################
## Prob biomass declines next year to year after with zero catch:
zero.catch.prob.bio.down.1 <- f(base.model$risks[[1]][1,2])
## Prob biomass declines year after next to year after that with zero catch:
zero.catch.prob.bio.down.2 <- f(base.model$risks[[2]][1,2])

## Canadian provisional reference points
dfo.probs.curr <- base.model$risks[[1]][,(ncol(base.model$risks[[1]])-2):ncol(base.model$risks[[1]])]
dfo.probs.fore <- base.model$risks[[2]][,(ncol(base.model$risks[[2]])-2):ncol(base.model$risks[[2]])]

################################################################################
## Second forecast year depletion and spawning biomass estimates
next2.depl.lower.tac.based <- f(fore.tac.mcmc$dlower[names(fore.tac.mcmc$dlower) %in% (end.yr + 2)] * 100, 1)
next2.depl.median.tac.based <- f(fore.tac.mcmc$dmed[names(fore.tac.mcmc$dmed) %in% (end.yr + 2)] * 100, 1)
next2.depl.upper.tac.based <- f(fore.tac.mcmc$dupper[names(fore.tac.mcmc$dupper) %in% (end.yr + 2)] * 100, 1)
next2.bio.lower.tac.based <- f(fore.tac.mcmc$slower[names(fore.tac.mcmc$slower) %in% (end.yr + 2)] * 100, 1)
next2.bio.median.tac.based <- f(fore.tac.mcmc$smed[names(fore.tac.mcmc$smed) %in% (end.yr + 2)] * 100, 1)
next2.bio.upper.tac.based <- f(fore.tac.mcmc$supper[names(fore.tac.mcmc$supper) %in% (end.yr + 2)] * 100, 1)

################################################################################
## number.to.word function located in utilities.r
catches.below.200000.since.1986 <- number.to.word(length(filter(catches, TOTAL <= 200000, Year > 1986)$Year))

################################################################################
## Age composition data for data section
survey.age.years <- base.model$dat$agecomp[base.model$dat$agecomp$FltSvy == 2,]$Yr
max.survey.age.prop <- make.age.comp.bubble.plot(base.model,
                                                 subplot = 2,
                                                 do.plot = FALSE)
max.fishery.age.prop <- make.age.comp.bubble.plot(base.model,
                                                  subplot = 1,
                                                  do.plot = FALSE)
catch.limit.quantiles <- f(as.numeric(quantile(base.model$mcmc[[paste0("ForeCatch_", end.yr)]],
                                               probs=c(0.025, 0.5, 0.975))))
names(catch.limit.quantiles) <- c("lower", "median", "upper")
                # 2.5%, median and 97.5% quantiles of catch limit for assess.yr
                #  using the default harvest policy; tonnes

top.coh <- function(yr = last.data.yr,
                    num.cohorts = 3,
                    decimals = 0,
                    cap = TRUE,
                    spec.yr = NA){
  ## Returns text describing the top N cohorts by year and percentage as a sentence.
  ## egs. top.coh(2018, 2) produces:
  ##  "The 2014 cohort was the largest (29\\%), followed by the 2010 cohort (27\\%)"
  ## top.coh(2018, 2, cap = FALSE) produces:
  ##  "the 2014 cohort was the largest (29\\%), followed by the 2010 cohort (27\\%)"
  ## top.coh(2018, spec.yr = 2010) produces:
  ##  "27"
  ## If spec.yr is a year, then the value only will be returned
  ##  as a percentage of that cohort caught in yr
  if(num.cohorts < 1){
    num.cohorts = 1
  }
  tmp <- base.model$catage[, -c(1, 3, 4, 5, 6)] %>%
    dplyr::filter(Fleet == 1) %>%
    select(-c(Fleet, Seas, XX, Era, 0))
  tmp <- tmp[-1,]
  row.sums <- rowSums(select(tmp, -Yr))
  x <- tmp %>%
    select(-Yr) %>%
    mutate_all(~ ./row.sums)
  x <- cbind(Yr = tmp$Yr, x) %>%
    dplyr::filter(Yr == yr) %>%
    select(-Yr) %>%
    sort() %>%
    rev()
  txt <- paste0(ifelse(cap, "The ", "the "),
                yr - as.numeric(names(x)[1]),
                " cohort was the largest (",
                f(x[1] * 100, decimals),
                "\\%)")
  if(num.cohorts > 1){
    for(i in 2:num.cohorts){
      txt <- paste0(txt,
                    ", followed by the ",
                    yr - as.numeric(names(x)[i]),
                    " cohort (",
                    f(x[i] * 100, decimals),
                    "\\%)")
    }
  }
  if(!is.na(spec.yr)){
    return(f(as.numeric(x[names(x) == yr - spec.yr]) * 100, decimals))
  }
  txt
}

catcher.processor.catch <- f(100 * filter(catches, Year == last.data.yr)$atSea_US_CP / (last.year.us.cp.quota.reallocated), 1)
mothership.catch <- f(100 * filter(catches, Year == last.data.yr)$atSea_US_MS / (last.year.us.ms.quota.reallocated), 1)
shore.based.catch <- f(100 * filter(catches, Year == last.data.yr)$US_shore / (last.year.us.shore.quota.reallocated), 1)

################################################################################
## Canadian age data variables
## Canadian Freezer trawlers
last.year.can.ages.ft <- can.ages[[2]][rownames(can.ages[[2]]) == last.data.yr,]
get.age.prop(last.year.can.ages.ft, 1)
ft.age.prop.holder <- get.age.prop(last.year.can.ages.ft, 1)
max.freezer.trawler.age.prop.age <- ft.age.prop.holder[1]
max.freezer.trawler.age.prop <- f(ft.age.prop.holder[2] * 100, 1)
ft.age.prop.holder <- get.age.prop(last.year.can.ages.ft, 2)
second.freezer.trawler.age.prop.age <- ft.age.prop.holder[1]
second.freezer.trawler.age.prop <- f(ft.age.prop.holder[2] * 100, 1)
ft.age.prop.holder <- get.age.prop(last.year.can.ages.ft, 3)
third.freezer.trawler.age.prop.age <- ft.age.prop.holder[1]
third.freezer.trawler.age.prop <- f(ft.age.prop.holder[2] * 100, 1)
ft.age.prop.holder <- get.age.prop(last.year.can.ages.ft, 4)
fourth.freezer.trawler.age.prop.age <- ft.age.prop.holder[1]
fourth.freezer.trawler.age.prop <- f(ft.age.prop.holder[2] * 100, 1)
## Canadian Shoreside
last.year.can.ages.ss <- can.ages[[1]][rownames(can.ages[[1]]) == last.data.yr,]
ss.age.prop.holder <- get.age.prop(last.year.can.ages.ss, 1)
max.shoreside.age.prop.age <- ss.age.prop.holder[1]
max.shoreside.age.prop <- f(ss.age.prop.holder[2] * 100, 1)
ss.age.prop.holder <- get.age.prop(last.year.can.ages.ss, 2)
second.shoreside.age.prop.age <- ss.age.prop.holder[1]
second.shoreside.age.prop <- f(ss.age.prop.holder[2] * 100, 1)
ss.age.prop.holder <- get.age.prop(last.year.can.ages.ss, 3)
third.shoreside.age.prop.age <- ss.age.prop.holder[1]
third.shoreside.age.prop <- f(ss.age.prop.holder[2] * 100, 1)
ss.age.prop.holder <- get.age.prop(last.year.can.ages.ss, 4)
fourth.shoreside.age.prop.age <- ss.age.prop.holder[1]
fourth.shoreside.age.prop <- f(ss.age.prop.holder[2] * 100, 1)

################################################################################
## Years for which median recruitment is below the mean of the median
##  recruitments for years >2010 and <(end.yr-1) ; end.yr-1 won't be
##  well estimated
recruitment.med.since.2010 <- base.model$mcmccalcs$rmed[ which(names(base.model$mcmccalcs$rmed) %in% 2010:end.yr & names(base.model$mcmccalcs$rmed) %in% start.yr:(end.yr-1))]
years.since.2010.recruitment.med.below.mean <- names(recruitment.med.since.2010[recruitment.med.since.2010  < mean(base.model$mcmccalcs$rmed)])


################################################################################
## Estimated recruitment in 2014 (and 2016) to discuss in Recruitment subsubsection, in billions
recruitment.med.in.2014 <- f(base.model$mcmccalcs$rmed["2014"], 3)
prob.percent.2014.rec.gt.2010.rec <- f(mean(base.model$mcmc$Recr_2014 > base.model$mcmc$Recr_2010) * 100, 1)
recruitment.med.in.2016 <- f(base.model$mcmccalcs$rmed["2016"], 3)

################################################################################
## Exploitation values
exploitation.med.2010 <- f(base.model$mcmccalcs$fmed["2010"],2)
exploitation.med.2012 <- f(base.model$mcmccalcs$fmed["2012"],2)
exploitation.med.2011 <- f(base.model$mcmccalcs$fmed["2011"],2)
exploitation.med.2015 <- f(base.model$mcmccalcs$fmed["2015"],2)
exploitation.med.2017 <- f(base.model$mcmccalcs$fmed["2017"],2)
exploitation.med.2018 <- f(base.model$mcmccalcs$fmed["2018"],2)
exploitation.med.penult.yr <- f(base.model$mcmccalcs$fmed[as.character(end.yr-1)],2)

################################################################################
last.survey.year <- survey.history[nrow(survey.history),]$year
last.survey.year.biomass <- f(survey.history[nrow(survey.history),]$biomass * 10, 2) ## millions of tonnes
penult.survey.year <- survey.history[nrow(survey.history) - 1,]$year
penult.survey.year.biomass <- f(survey.history[nrow(survey.history) - 1,]$biomass * 10, 2)
antepenult.survey.year <- survey.history[nrow(survey.history) - 2,]$year
antepenult.survey.year.biomass <- f(survey.history[nrow(survey.history) - 2,]$biomass * 10, 2)
## How many times higher is the last survey than the one before it?
last.factor.penult <- f(survey.history[nrow(survey.history),]$biomass / survey.history[nrow(survey.history) - 1,]$biomass, 1)
## How many times higher is the last survey than the one that was two before it?
last.factor.antepenult <- f(survey.history[nrow(survey.history),]$biomass / survey.history[nrow(survey.history) - 2,]$biomass, 1)

################################################################################
## Get priors settings from the control file
param.details <- make.parameters.estimated.summary.table(base.model,
                                                         start.rec.dev.yr = recruit.dev.start.yr,
                                                         end.rec.dev.yr = end.yr - 1,
                                                         return.xtable = FALSE)
m.prior <- split.prior.info(param.details[rownames(param.details) == "m.vals",][4],
                            dec.points = 2,
                            first.to.lower = TRUE)
## Now, in document, use m.prior[1] for name of prior, m.prior[1] for mean, and m.prior[3] for SD.

################################################################################
cohort.catch.1999 <- sum(cohortCatch(1999, base.model$catage))
cohort.catch.2010 <- sum(cohortCatch(2010, base.model$catage))

## cumulative sums for use in JMC presentation
cohortCumSum1999 <- cumsum(cohortCatch(1999,base.model$catage))
cohortCumSum2010 <- cumsum(cohortCatch(2010,base.model$catage))
cohortCumSum2014 <- cumsum(cohortCatch(2014,base.model$catage))
ages1999 <- as.numeric(names(cohortCumSum1999)) - 1999
ages2010 <- as.numeric(names(cohortCumSum2010)) - 2010
ages2014 <- as.numeric(names(cohortCumSum2014)) - 2014

## Estimated (median MCMC) proportions by age (using numbers) of the catch in first forecast year
fore.catch.prop <- as.data.frame( t(as.numeric(f(apply(base.model$extra.mcmc$natsel.prop, 2, median)* 100))))
names(fore.catch.prop) <- paste0("Age", 0:20)
# Confidence intervals for age5 (pick the biggest cohort; note natsel.prop columns start with age-0).
fore.catch.prop.age5.lower <- quantile(base.model$extra.mcmc$natsel.prop[,6], 0.025) * 100
fore.catch.prop.age5.upper <- quantile(base.model$extra.mcmc$natsel.prop[,6], 0.975) * 100
# Estimated proportion by age (using catch) of catch in first forecast year
fore.catch.prop.wt.age5.median <- median(base.model$extra.mcmc$natselwt.prop[,6]) * 100
fore.catch.prop.wt.age9.median <- median(base.model$extra.mcmc$natselwt.prop[,10]) * 100
fore.catch.prop.wt.age3.median <- median(base.model$extra.mcmc$natselwt.prop[,3]) * 100

################################################################################
## Sigma_r, standard deviation of recruitment variability.
sigma.r <- f(base.model$sigma_R_in, 2)

# alternative sigma.r based on all years of recdevs
sigma.r.alt.allyr <- f(base.model$sigma_R_info$alternative_sigma_R[3],2)
sigma.r.alt.main <- f(base.model$sigma_R_info$alternative_sigma_R[1],2)

# range of "main" recdevs
main.recdev.start <- min(base.model$recruit$Yr[base.model$recruit$era=="Main"])
main.recdev.end <- max(base.model$recruit$Yr[base.model$recruit$era=="Main"])

# range of "main" bias adjustement period for recdevs
main.recdevbias.start <- min(base.model$recruit$Yr[base.model$recruit$biasadj==max(base.model$recruit$biasadj)])
main.recdevbias.end <- max(base.model$recruit$Yr[base.model$recruit$biasadj==max(base.model$recruit$biasadj)])

################################################################################
## Load weight-at-age file now that models are loaded
wt.at.age <- load.wt.at.age(base.model, weight.at.age.file.name)

################################################################################
## Retrospective setup for the document. This must be done after the base.model
##  object has been fully setup.
retro.model.names <- c(base.model.name,
                       sapply(plot.retro.yrs,
                              function(x) paste0("-", x, if(x == 1) " year" else " years")))
## Need to assemble the list with the base as the first element
retro.list <- list(base.model)
for(i in plot.retro.yrs){
  retro.list[[i + 1]] <- base.model$retros[[i]]
}

################################################################################
## Define number of 'recent' years for several tables at the start of
##  main-tables.rnw.
num.recent.yrs <- 10

################################################################################
## values of Dirichlet-Multinomial data weighting parameters

# MLE estimates of parameters
log.theta.fishery <- round(base.model$parameters["ln(EffN_mult)_1","Value"],3)
log.theta.survey <- round(base.model$parameters["ln(EffN_mult)_2","Value"],3)
# non-log MLE estimates
theta.fishery <- exp(base.model$parameters["ln(EffN_mult)_1","Value"])
theta.survey <- exp(base.model$parameters["ln(EffN_mult)_2","Value"])
# approximate MLE weights
DM.weight.fishery <- round(theta.fishery/(1+theta.fishery),3)
DM.weight.survey <- round(theta.survey/(1+theta.survey),3)
# MCMC medians for the fishery (survey value fixed at MLE)
log.theta.fishery.median <- round(median(base.model$mcmc$ln.EffN_mult._1),3)
DM.weight.fishery.median <- round(median(exp(base.model$mcmc$ln.EffN_mult._1)/(1+exp(base.model$mcmc$ln.EffN_mult._1))),3)
