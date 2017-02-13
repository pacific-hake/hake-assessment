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
last.year.us.not.attained <- f(as.numeric(100 - landings.vs.tac[landings.vs.tac$Year %in% (end.yr-1),]$USATTAIN), 1)
last.year.us.not.attained.tonnes <- filter(landings.vs.tac, Year == last.data.yr)$TACUSA - filter(landings.vs.tac, Year == last.data.yr)$Ustotal
last.year.us.tac <- f(as.numeric(landings.vs.tac[landings.vs.tac$Year %in% (end.yr-1),]$TACUS))
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
last.year.can.landings <- f(as.numeric(landings.vs.tac[landings.vs.tac$Year %in% (end.yr-1),]$CANtotal))
last.year.can.tac <- f(landings.vs.tac[landings.vs.tac$Year %in% (end.yr-1),]$TACCAN)
last.year.can.tac.jv <- f(filter(further.tac, Year == last.data.yr)$can.jv.tac)
last.year.can.shoreside.tac <- f(landings.vs.tac[landings.vs.tac$Year %in% (end.yr-1),]$TACCAN - filter(further.tac, Year == last.data.yr)$can.jv.tac)

latest.year.can.jv <- max(filter(catches, CAN_JV > 0)$Year)  # latest year of JV in Canada
last.year.can.shore <- f(filter(catches, Year == last.data.yr)$CAN_Shoreside)
last.year.can.freezer <- f(filter(catches, Year == last.data.yr)$CAN_FreezeTrawl)
last.year.can.shore.percent <- f(filter(catches, Year == last.data.yr)$CAN_Shoreside /
                                    as.numeric(landings.vs.tac[landings.vs.tac$Year %in% (end.yr-1),]$CANtotal) * 100.0, 1)
last.year.can.freezer.percent <- f(filter(catches, Year == last.data.yr)$CAN_FreezeTrawl /
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
median.bio.min  <- f(min(base.model$mcmccalcs$smed), 3)  # min median biomass
median.bio.min.year <- names(which.min(base.model$mcmccalcs$smed)) # year of min
median.intensity <- base.model$mcmccalcs$pmed
median.intensity.2007.to.2011 <- median.intensity[c("2007", "2008", "2009", "2010", "2011")]
median.intensity.2007.to.2011.min <- f(min(median.intensity.2007.to.2011)*100, 0)
median.intensity.2007.to.2011.max <- f(max(median.intensity.2007.to.2011)*100, 0)
# Could replace some of the following ..pmed with median.intensity:
median.intensity.above.one.all.years <- names(which(base.model$mcmccalcs$pmed > 1))    # includes > end.yr
median.intensity.above.one.years <- median.intensity.above.one.all.years[
         median.intensity.above.one.all.years < end.yr]  # ones to mention
median.intensity.2010 <- f(base.model$mcmccalcs$pmed["2010"] * 100, 1)
median.intensity.penult.yr <- f(base.model$mcmccalcs$pmed[as.character(end.yr-1)] * 100, 1)
median.relative.bio <- base.model$mcmccalcs$dmed
median.relative.bio.2007.to.2011 <- median.relative.bio[c("2007", "2008", "2009", "2010", "2011")]
median.relative.bio.2007.to.2011.min <- f(min(median.relative.bio.2007.to.2011), 2)
median.relative.bio.2007.to.2011.max <- f(max(median.relative.bio.2007.to.2011), 2)
median.relative.bio.below.target <- median.relative.bio[median.relative.bio < 0.4]     # when below target
median.relative.bio.above.target.since <- as.numeric(max(names(median.relative.bio.below.target)))+1   # has been above target since

################################################################################
## Prob biomass declines next year to year after with zero catch:
zero.catch.prob.bio.down.1 <- f(base.model$risks[[1]][1,2])
## Prob biomass declines year after next to year after that with zero catch:
zero.catch.prob.bio.down.2 <- f(base.model$risks[[2]][1,2])

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

################################################################################
## Estimated numbers at age for fishery for Recruitment section in Exec Summary and main text
##  From make.age.comp.fit.plot() which in turn calls age.fits()
fishery.estimated.age.comp <- base.model$agedbase[base.model$agedbase$Fleet==1,]  #I think that this has ageing error incorporated
year.class.2010.in.2013 <- f(filter(fishery.estimated.age.comp, Yr==2013, Bin==3)$Exp * 100)
year.class.2010.in.2014 <- f(filter(fishery.estimated.age.comp, Yr==2014, Bin==4)$Exp * 100)
year.class.2010.in.2015 <- f(filter(fishery.estimated.age.comp, Yr==2015, Bin==5)$Exp * 100)

tmp <- base.model$catage[base.model$catage$Fleet==1,-(1:10)]  #This does not have ageing error
fishery.estimated.age.comp <- cbind(base.model$catage[base.model$catage$Fleet==1,(1:10)],t(apply(tmp,1,function(x){x/sum(x)})))
year.class.2010.in.2013 <- f(filter(fishery.estimated.age.comp, Yr==2013)$"3" * 100)
year.class.2010.in.2014 <- f(filter(fishery.estimated.age.comp, Yr==2014)$"4" * 100)
year.class.2010.in.2015 <- f(filter(fishery.estimated.age.comp, Yr==2015)$"5" * 100)
year.class.2010.in.2016 <- f(filter(fishery.estimated.age.comp, Yr==2016)$"6" * 100)

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
recruitment.med.since.2010 <- base.model$mcmccalcs$rmed[ which(as.numeric(names(base.model$mcmccalcs$rmed)) > 2010 & as.numeric(names(base.model$mcmccalcs$rmed)) < (end.yr-1))]
years.since.2010.recruitment.med.below.mean <- names(recruitment.med.since.2010[recruitment.med.since.2010  < mean(base.model$mcmccalcs$rmed)])

################################################################################
## Exploitation values
exploitation.med.2010 <- f(base.model$mcmccalcs$fmed["2010"],2)
exploitation.med.penult.yr <- f(base.model$mcmccalcs$fmed[as.character(end.yr-1)],2)

################################################################################
## Survey comparisons of biomass from year to year. Use the table, not the value of survey.end.year
## Next year, we should set survey.end.yr to be what is in the table. Not going to attempt it
##  with only hours left to submission.
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

################################################################################
## Sigma_r, standard deviation of recruitment variability.
sigma.r <- f(base.model$sigma_R_in, 2)

# alternative sigma.r based on all years of recdevs
sigma.r.alt.allyr <- f(base.model$sigma_R_info$alternative_sigma_R[3],2)
sigma.r.alt.main <- f(base.model$sigma_R_info$alternative_sigma_R[1],2)

# range of "main" recdevs
main.recdev.start <- min(base.model$recruit$year[base.model$recruit$era=="Main"])
main.recdev.end <- max(base.model$recruit$year[base.model$recruit$era=="Main"])

# range of "main" bias adjustement period for recdevs
main.recdevbias.start <- min(base.model$recruit$year[base.model$recruit$biasadj==max(base.model$recruit$biasadj)])
main.recdevbias.end <- max(base.model$recruit$year[base.model$recruit$biasadj==max(base.model$recruit$biasadj)])


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
