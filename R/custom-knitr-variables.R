# Put any variables you intend to use in the text here.
# The function f() is for formatting and is defined in
# r-functions/utilities.r

# Allotments ------------------------------------------------------------------
can.allotment.percent <- 26.12
us.allotment.percent <- 73.88

# Attainments------------------------------------------------------------------
ct_last10 <- ct %>% filter(year %in% (end.yr - 10):(end.yr - 1))
ct_last5 <- ct %>% filter(year %in% (end.yr - 5):(end.yr - 1))
ct_last2 <- ct %>% filter(year %in% (end.yr - 2):(end.yr - 1))
ct_last1 <- ct %>% filter(year == end.yr - 1)
ct_secondlast <- ct %>% filter(year == end.yr - 2)
usa.last.5.years.attainment <- ct_last5 %>% pull(us_attain) %>%  mean %>% f(1)
usa.last.2.years.attainment <- ct_last2 %>% pull(us_attain) %>% mean %>% f(0)
can.last.5.years.attainment <- ct_last5 %>% pull(can_attain) %>% mean %>% f(1)
can.last.2.years.attainment <- ct_last2 %>% pull(can_attain) %>% mean %>% f(0)
tot.last.5.years.attainment <- ct_last5 %>% pull(tot_attain)  %>% mean %>% f(1)
tot.last.10.years.attainment <- ct_last10 %>% pull(tot_attain) %>% mean %>% f(1)
tot.last.year.attainment <- ct_last1 %>% pull(tot_attain) %>% mean %>% f(1)
tot.2015.attainment <- ct %>% filter(year == 2015) %>% pull(tot_attain) %>% mean %>% f(1)
tot.9192.attainment <- ct %>% filter(year %in% 1991:1992) %>% pull(tot_attain) %>% mean %>% f(0)
tot.9399.attainment <- ct %>% filter(year %in% 1993:1999) %>% pull(tot_attain) %>% mean %>% f(0)

# Last year allotments (not the Treaty ones in 2020) -------------------
can.allotment.percent.last.year <- f(pull(ct_last1, can_tac) / pull(ct_last1, tot_tac) * 100, 2)
us.allotment.percent.last.year <- f(pull(ct_last1, us_tac) / pull(ct_last1, tot_tac) * 100, 2)

# Recent catch ----------------------------------------------------------------
last.5.years.of.catch.data <- (max(ct$year) - 4):max(ct$year)
last.5.years.total.catch <- ct_last5 %>% pull(tot_catch)
long.term.avge.catch <- mean(ct$tot_catch)
last.5.years.above.avge <- last.5.years.of.catch.data[last.5.years.total.catch > long.term.avge.catch]
last.5.years.below.avge <- last.5.years.of.catch.data[last.5.years.total.catch < long.term.avge.catch]
catch.limit.quantiles <- f(as.numeric(quantile(base.model$mcmc[[paste0("ForeCatch_", end.yr)]],
                                               probs = c(0.025, 0.5, 0.975))))
names(catch.limit.quantiles) <- c("lower", "median", "upper")

# Recent catch, last year -----------------------------------------------------
last.year.landings <- ct_last1 %>% pull(tot_catch) %>% f(0)
last.year.tac <- ct_last1 %>% pull(tot_tac) %>% f(0)
last.year.attained <- ct_last1 %>% pull(tot_attain) %>% f(1)

# Catch over the last 10 years ------------------------------------------------
catch_last_10yrs <- ct %>% slice_tail(n = 10)
catch_mean_10yrs <- f(mean(catch_last_10yrs$tot_catch))
catch_us_mean_10yrs <- f(mean(catch_last_10yrs$us_catch))
catch_can_mean_10yrs <- f(mean(catch_last_10yrs$can_catch))

# US Attainment, catch, and TAC -----------------------------------------------
last.year.us.landings <- ct_last1 %>% pull(us_catch) %>% f(0)
last.year.us.attained <- ct_last1 %>% pull(us_attain) %>% f(1)
last.2year.us.attained.diff <- (ct_last1 %>% pull(us_attain) - ct_secondlast %>% pull(us_attain)) %>% f(1)
last.year.us.not.attained <- (100 - ct_last1 %>% pull(us_attain)) %>% f(1)
last.year.us.not.attained.tonnes <- abs(ct_last1 %>% pull(us_attain) - ct_last1 %>% pull(us_catch))
last.year.us.tac <- ct_last1 %>% pull(us_tac) %>% f(0)
ft <- further.tac
last.year.us.tribal <- ft %>% filter(Year == last.data.yr) %>% pull(us.tribal.quota)
last.year.us.research <- ft %>% filter(Year == last.data.yr) %>% pull(us.research.quota)
last.year.us.non.tribal <- ft %>% filter(Year == last.data.yr) %>% pull(us.nontribal.quota)
last.year.us.tribal.quota.reallocated <- ft %>% filter(Year == last.data.yr) %>% pull(us.tribal.quota.reallocated)
last.year.us.tribal.reallocate.dates <- ft %>% filter(Year == last.data.yr) %>% pull(us.tribal.reallocate.dates)
last.year.us.tribal.reallocate.dates.f <- format(as.Date(as.character(last.year.us.tribal.reallocate.dates),"%d-%b"),"%B %d")
last.year.us.tribal.max.landed <- ft %>% filter(Year == last.data.yr) %>% pull(us.tribal.max.landed)
last.year.us.shore.quota.reallocated <- ft %>% filter(Year == last.data.yr) %>% pull(us.shore.reallocated)
last.year.us.cp.quota.reallocated <- ft %>% filter(Year == last.data.yr) %>% pull(us.cp.reallocated)
last.year.us.ms.quota.reallocated <- ft %>% filter(Year == last.data.yr) %>% pull(us.ms.reallocated)

# US Catch by fleet, last year ------------------------------------------------
last.year.us.research.catch <- ct %>% filter(year == last.data.yr) %>% pull(us_research_xx)
last.year.us.cp.catch <- ct %>% filter(year == last.data.yr) %>% pull(us_cp_xx)
last.year.us.ms.catch <- ct %>% filter(year == last.data.yr) %>% pull(us_ms_xx)
last.year.us.shore.catch <- ct %>% filter(year == last.data.yr) %>% pull(us_shore_xx)
last.year.us.ti.catch <- us.ti.catch.by.month %>% filter(year == last.data.yr) %>% pull(catch) %>% sum
catcher.processor.catch <- ((ct_last1 %>% select(us_cp_xx) %>% pull) / (last.year.us.cp.quota.reallocated) * 100) %>% f(1)
mothership.catch <- ((ct_last1 %>% select(us_ms_xx) %>% pull) / (last.year.us.ms.quota.reallocated) * 100) %>% f(1)
shore.based.catch <- ((ct_last1 %>% select(us_shore_xx) %>% pull) / (last.year.us.shore.quota.reallocated) * 100) %>% f(1)

# US Attainment by fleet, last year ------------------------------------------------
last.year.us.research.catch.percent <- f(last.year.us.research.catch / last.year.us.research * 100, 1)
last.year.us.cp.catch.percent <- f(last.year.us.cp.catch / last.year.us.cp.quota.reallocated * 100, 1)
last.year.us.ms.catch.percent <- f(last.year.us.ms.catch / last.year.us.ms.quota.reallocated * 100, 1)
last.year.us.shore.catch.percent <- f(last.year.us.shore.catch / last.year.us.shore.quota.reallocated * 100, 1)
last.year.us.tribal.catch.percent <- f(last.year.us.tribal.max.landed / last.year.us.tribal.quota.reallocated * 100, 1)

# Canada Attainment, catch, and TAC -------------------------------------------
can.vessels <- c("Viking Enterprise", "Northern Alliance", "Osprey #1",
  "Raw Spirit", "Pacific Legacy #1", "Sunderoey", "Viking Alliance")
last.year.can.carryover <- ft %>% filter(Year == last.data.yr) %>% pull(can.carried.over) %>% f(0)
last.year.can.attained <- ct_last1 %>% pull(can_attain) %>% f(1)
last.2year.can.attained.diff <- ((ct_last1 %>% pull(can_attain)) - (ct_secondlast %>% pull(can_attain))) %>% f(1)
last.year.can.landings <- ct_last1 %>% pull(can_catch) %>% f(0)
last.year.can.tac <- ct_last1 %>% pull(can_tac) %>% f
last.year.can.tac.jv <- ft %>% filter(Year == last.data.yr) %>% pull(can.jv.tac) %>% f
last.year.can.shoreside.tac <- ((ct_last1 %>% pull(can_tac)) - (ft %>% filter(Year == last.data.yr) %>% pull(can.jv.tac))) %>% f(0)
latest.year.can.jv <- ct %>% filter(can_jv_xx > 0) %>% pull(year) %>% max
last.year.can.shore <- ct_last1 %>% pull(can_shore_xx) %>% f(0)
last.year.can.freezer <- ct_last1 %>% pull(can_freeze_xx) %>% f(0)
last.year.can.jv <- ct_last1 %>% pull(can_jv_xx) %>% f(0)
last.year.can.shore.percent <- ((ct_last1 %>% pull(can_shore_xx)) /
                                  (ct_last1 %>% pull(can_catch)) * 100) %>% f(1)
last.year.can.freezer.percent <- ((ct_last1 %>% pull(can_freeze_xx)) /
                                    (ct_last1 %>% pull(can_catch)) * 100) %>% f(1)
last.year.can.jv.percent <- ((ct_last1 %>% select(can_jv_xx) %>% pull) /
                                  (ct_last1 %>% select(can_catch) %>% pull) * 100) %>% f(1)
# Years since 2000 (including 2000) that JV catch has been zero
ch.eq.0.recent <- ct %>% filter(year > 1999) %>% select(year, can_jv_xx) %>% filter(can_jv_xx == 0) %>% nrow
terminal.year.us.jvforeign <- ct %>% select(year, matches("us_[fj]")) %>%
  rowwise() %>% mutate(sumV = sum(c_across(matches("us")))) %>% filter(sumV > 0) %>% select(year) %>% max
first.year.us.atsea <- ct %>% select(year, matches("us_cp|us_ms")) %>%
  rowwise() %>% mutate(sumV = sum(c_across(matches("us")))) %>% filter(sumV > 0) %>% select(year) %>% min


# Survey values ---------------------------------------------------------------
survey.biomass <- survey.history$biomass
names(survey.biomass) <- as.character(survey.history$year)
survey.comps <- base.model$dat$agecomp[base.model$dat$agecomp$FltSvy==2,]
rownames(survey.comps) <- survey.comps$Yr
survey.last.year <- survey.comps[nrow(survey.comps),]
survey.last.year.age <- sort(decreasing = TRUE, round(unlist(prop.table(
  survey.last.year[grep("^a", colnames(survey.last.year))])), 4))
names(survey.last.year.age) <- gsub("^a", "", names(survey.last.year.age))
survey.1.prop.age <- as.numeric(gsub("^a", "", names(survey.last.year.age)[1]))
survey.1.prop <- f(survey.last.year.age[1] * 100, 1)
survey.2.prop.age <- as.numeric(gsub("^a", "", names(survey.last.year.age)[2]))
survey.2.prop <- f(survey.last.year.age[2] * 100, 1)
survey.3.prop.age <- as.numeric(gsub("^a", "", names(survey.last.year.age)[3]))
survey.3.prop <- f(survey.last.year.age[3] * 100, 1)
survey.4.prop.age <- as.numeric(gsub("^a", "", names(survey.last.year.age)[4]))
survey.4.prop <- f(survey.last.year.age[4] * 100, 1)
survey.a2.prop <- f(survey.last.year["a2"], 1)
last.survey.year <- survey.history[nrow(survey.history),]$year
last.survey.year.biomass <- f(survey.history[nrow(survey.history),]$biomass, 2) ## millions of tonnes
penult.survey.year <- survey.history[nrow(survey.history) - 1,]$year
penult.survey.year.biomass <- f(survey.history[nrow(survey.history) - 1,]$biomass, 2)
antepenult.survey.year <- survey.history[nrow(survey.history) - 2,]$year
antepenult.survey.year.biomass <- f(survey.history[nrow(survey.history) - 2,]$biomass, 2)

# How many times higher is the last survey than the one before it?
last.factor.penult <- f(survey.history[nrow(survey.history),]$biomass /
                          survey.history[nrow(survey.history) - 1,]$biomass, 1)
# How many times higher is the last survey than the one that was two before it?
last.factor.antepenult <- f(survey.history[nrow(survey.history),]$biomass /
                              survey.history[nrow(survey.history) - 2,]$biomass, 1)

# Survey extrapolation values -------------------------------------------------
survey.extrap.percent <- 100 * (survey.comparison$with.extrap - survey.comparison$no.extrap) / survey.comparison$with.extrap
names(survey.extrap.percent) <- as.character(survey.comparison$year)
survey.extrap.percent <- survey.extrap.percent[!is.na(survey.extrap.percent)]
survey.largest.extrap.percent <- f(max(survey.extrap.percent), 2)
survey.year.of.largest.extrap <- names(survey.extrap.percent[survey.extrap.percent == max(survey.extrap.percent)])
survey.smallest.extrap.percent <- f(min(survey.extrap.percent), 2)
survey.year.of.smallest.extrap <- names(survey.extrap.percent[survey.extrap.percent == min(survey.extrap.percent)])
survey.average.extrap.percent <- f(mean(survey.extrap.percent), 2)

# Spawning Biomass and Depletion estimates ------------------------------------
curr.depl.lower <- f(base.model$mcmccalcs$dlower[names(base.model$mcmccalcs$dlower) %in% end.yr] * 100, 0)
curr.depl.median <- f(base.model$mcmccalcs$dmed[names(base.model$mcmccalcs$dmed) %in% end.yr] * 100, 0)
curr.depl.upper <- f(base.model$mcmccalcs$dupper[names(base.model$mcmccalcs$dupper) %in% end.yr] * 100, 0)
# These are millions of tons:
curr.bio.lower <- f(base.model$mcmccalcs$slower[names(base.model$mcmccalcs$slower) %in% end.yr], 3)
curr.bio.median <- f(base.model$mcmccalcs$smed[names(base.model$mcmccalcs$smed) %in% end.yr], 3)
curr.bio.upper <- f(base.model$mcmccalcs$supper[names(base.model$mcmccalcs$supper) %in% end.yr], 3)
# These are metric tonnes:
curr.bio.lower.tonnes <- f(base.model$mcmccalcs$slower[names(base.model$mcmccalcs$slower) %in% end.yr] * 1e6, 0)
curr.bio.median.tonnes <- f(base.model$mcmccalcs$smed[names(base.model$mcmccalcs$smed) %in% end.yr] * 1e6, 0)
curr.bio.upper.tonnes <- f(base.model$mcmccalcs$supper[names(base.model$mcmccalcs$supper) %in% end.yr] * 1e6, 0)

# Spawning biomass for previous year (calculated in this assessment) in millions of tonnes and then tonnes----------
prev.bio.lower <- f(base.model$mcmccalcs$slower[names(base.model$mcmccalcs$slower) %in% last.data.yr], 3)      # last.data.yr = end.yr-1
prev.bio.median <- f(base.model$mcmccalcs$smed[names(base.model$mcmccalcs$smed) %in% last.data.yr], 3)
prev.bio.upper <- f(base.model$mcmccalcs$supper[names(base.model$mcmccalcs$supper) %in% last.data.yr], 3)
prev.bio.lower.tonnes <- f(base.model$mcmccalcs$slower[names(base.model$mcmccalcs$slower) %in% last.data.yr] * 1e6, 0)      # last.data.yr = end.yr-1
prev.bio.median.tonnes <- f(base.model$mcmccalcs$smed[names(base.model$mcmccalcs$smed) %in% last.data.yr] * 1e6, 0)
prev.bio.upper.tonnes <- f(base.model$mcmccalcs$supper[names(base.model$mcmccalcs$supper) %in% last.data.yr] * 1e6, 0)



# Spawning biomass for previous year (calculated in last years assessment) ----
prev.bio.lower.last.assess <- f(last.yr.base.model$mcmccalcs$slower[names(base.model$mcmccalcs$slower) %in% last.data.yr], 3)      # last.data.yr = end.yr-1
prev.bio.median.last.assess <- f(last.yr.base.model$mcmccalcs$smed[names(base.model$mcmccalcs$smed) %in% last.data.yr], 3)
prev.bio.upper.last.assess <- f(last.yr.base.model$mcmccalcs$supper[names(base.model$mcmccalcs$supper) %in% last.data.yr], 3)

# First forecast year depletion and spawning biomass estimates ----------------
fore.tac.mcmc.yr1 <- base.model$forecasts[[1]][[catch.tac.ind]]$mcmccalcs
next.depl.lower.tac.based <- f(fore.tac.mcmc.yr1$dlower[names(fore.tac.mcmc.yr1$dlower) %in% (end.yr + 1)] * 100, 1)
next.depl.median.tac.based <- f(fore.tac.mcmc.yr1$dmed[names(fore.tac.mcmc.yr1$dmed) %in% (end.yr + 1)] * 100, 1)
next.depl.upper.tac.based <- f(fore.tac.mcmc.yr1$dupper[names(fore.tac.mcmc.yr1$dupper) %in% (end.yr + 1)] * 100, 1)
next.bio.lower.tac.based <- f(fore.tac.mcmc.yr1$slower[names(fore.tac.mcmc.yr1$slower) %in% (end.yr + 1)] * 100, 1)
next.bio.median.tac.based <- f(fore.tac.mcmc.yr1$smed[names(fore.tac.mcmc.yr1$smed) %in% (end.yr + 1)] * 100, 1)
next.bio.upper.tac.based <- f(fore.tac.mcmc.yr1$supper[names(fore.tac.mcmc.yr1$supper) %in% (end.yr + 1)] * 100, 1)
# Second forecast year depletion and spawning biomass estimates ---------------
fore.tac.mcmc.yr2 <- base.model$forecasts[[2]][[catch.tac.ind]]$mcmccalcs
next2.depl.lower.tac.based <- f(fore.tac.mcmc.yr2$dlower[names(fore.tac.mcmc.yr2$dlower) %in% (end.yr + 2)] * 100, 1)
next2.depl.median.tac.based <- f(fore.tac.mcmc.yr2$dmed[names(fore.tac.mcmc.yr2$dmed) %in% (end.yr + 2)] * 100, 1)
next2.depl.upper.tac.based <- f(fore.tac.mcmc.yr2$dupper[names(fore.tac.mcmc.yr2$dupper) %in% (end.yr + 2)] * 100, 1)
next2.bio.lower.tac.based <- f(fore.tac.mcmc.yr2$slower[names(fore.tac.mcmc.yr2$slower) %in% (end.yr + 2)] * 100, 1)
next2.bio.median.tac.based <- f(fore.tac.mcmc.yr2$smed[names(fore.tac.mcmc.yr2$smed) %in% (end.yr + 2)] * 100, 1)
next2.bio.upper.tac.based <- f(fore.tac.mcmc.yr2$supper[names(fore.tac.mcmc.yr2$supper) %in% (end.yr + 2)] * 100, 1)

# Biomass medians for last year's TAC catch level -----------------------------
.fore.last.yr.tac <- base.model$forecasts[[length(base.model$forecasts)]][catch.tac.ind][[1]]
last.yr.tac.fore.1.biomass <- f(.fore.last.yr.tac$biomass[1,3] * 100)
last.yr.tac.fore.2.biomass <- f(.fore.last.yr.tac$biomass[2,3] * 100)
last.yr.tac.fore.3.biomass <- f(.fore.last.yr.tac$biomass[3,3] * 100)
.risk.last.yr.tac.1 <- base.model$risks[[1]]
.risk.last.yr.tac.2 <- base.model$risks[[2]]
.risk.last.yr.tac.3 <- base.model$risks[[3]]
last.yr.tac.risk.1.biomass.decline <- f(as.numeric(.risk.last.yr.tac.1[catch.tac.ind, 2]))
last.yr.tac.risk.2.biomass.decline <- f(as.numeric(.risk.last.yr.tac.2[catch.tac.ind, 2]))
last.yr.tac.risk.3.biomass.decline <- f(as.numeric(.risk.last.yr.tac.3[catch.tac.ind, 2]))
last.yr.tac.risk.2.bforty <- f(as.numeric(.risk.last.yr.tac.2[catch.tac.ind, 3]))

# Numbers at age calculations for bubble plot caption -------------------------
median.nat.no.year <- dplyr::select(base.model$extra.mcmc$natage_median,
                                                     -c("Yr"))
max.median.nat <- f(max(median.nat.no.year)/1e3, 1) # billions
year.of.max.median.nat.ind <- which(median.nat.no.year == max(median.nat.no.year), arr.ind=TRUE)[1]
year.of.max.median.nat <- base.model$extra.mcmc$natage_median[year.of.max.median.nat.ind, "Yr"]

# Calculations for Executive Summary and Assessment section -------------------
num.mcmc.samples <- dim(base.model$mcmc)[1]
median.bio.min <- f(min(base.model$mcmccalcs$smed[names(base.model$mcmccalcs$smed) %in% start.yr:end.yr]), 3)  # min median biomass
median.bio.min.year <- names(which.min(base.model$mcmccalcs$smed[names(base.model$mcmccalcs$smed) %in% start.yr:end.yr])) # year of min
median.intensity <- base.model$mcmccalcs$pmed
median.intensity.2007.to.2010 <- median.intensity[c("2007", "2008", "2009", "2010")]
median.intensity.2007.to.2010.min <- f(min(median.intensity.2007.to.2010)*100, 0)
median.intensity.2007.to.2010.max <- f(max(median.intensity.2007.to.2010)*100, 0)
median.intensity.2007.to.2011 <- median.intensity[c("2007", "2008", "2009", "2010", "2011")]
median.intensity.2007.to.2011.min <- f(min(median.intensity.2007.to.2011)*100, 0)
median.intensity.2007.to.2011.max <- f(max(median.intensity.2007.to.2011)*100, 0)
median.intensity.above.one.all.years <- names(which(base.model$mcmccalcs$pmed > 1))    # includes > end.yr
median.intensity.above.one.years <- median.intensity.above.one.all.years[
         median.intensity.above.one.all.years < end.yr]  # ones to mention
median.intensity.2010 <- f(base.model$mcmccalcs$pmed["2010"] * 100, 1)
median.intensity.2015 <- f(base.model$mcmccalcs$pmed["2015"] * 100, 1)
median.intensity.2017 <- f(base.model$mcmccalcs$pmed["2017"] * 100, 1)
median.intensity.2018 <- f(base.model$mcmccalcs$pmed["2018"] * 100, 1)
median.intensity.2019 <- f(base.model$mcmccalcs$pmed["2019"] * 100, 1)
median.intensity.2020 <- f(base.model$mcmccalcs$pmed["2020"] * 100, 1)
median.intensity.2021 <- f(base.model$mcmccalcs$pmed["2021"] * 100, 1)
median.intensity.penult.yr <- f(base.model$mcmccalcs$pmed[as.character(end.yr-1)] * 100, 1)
median.relative.bio <- base.model$mcmccalcs$dmed
median.relative.bio.2007.to.2010 <- median.relative.bio[c("2007", "2008", "2009", "2010")]
median.relative.bio.2007.to.2010.min <- f(min(median.relative.bio.2007.to.2010), 2)
median.relative.bio.2007.to.2010.max <- f(max(median.relative.bio.2007.to.2010), 2)
median.relative.bio.2007.to.2011 <- median.relative.bio[c("2007", "2008", "2009", "2010", "2011")]
median.relative.bio.2007.to.2011.min <- f(min(median.relative.bio.2007.to.2011), 2)
median.relative.bio.2007.to.2011.max <- f(max(median.relative.bio.2007.to.2011), 2)
median.relative.bio.below.target <- median.relative.bio[names(median.relative.bio) %in% start.yr:end.yr & median.relative.bio < 0.4]     # when below target
median.relative.bio.above.target.since <- max(as.numeric(names(median.relative.bio.below.target)),na.rm=T)+1   # has been above target since
median.relative.bio.2017 <- f(base.model$mcmccalcs$dmed["2017"] * 100, 1)

# Recruitments in current assessment vs last assessment -----------------------
prev.assess.recruitment.lower  <- bridge.models.1[[1]]$mcmccalcs$rlower
prev.assess.recruitment.med  <- bridge.models.1[[1]]$mcmccalcs$rmed
prev.assess.recruitment.upper <- bridge.models.1[[1]]$mcmccalcs$rupper

# Current assessment w/o final projection year --------------------------------
# since not in previous assessment)
compareablenames <- names(base.model[["mcmccalcs"]][["rlower"]]) %in%
  names(prev.assess.recruitment.lower)
recruitment.lower.to.compare <- base.model[["mcmccalcs"]][["rlower"]][compareablenames]
recruitment.med.to.compare <- base.model[["mcmccalcs"]][["rmed"]][compareablenames]
recruitment.upper.to.compare <- base.model[["mcmccalcs"]][["rhigh"]][compareablenames]

# Prob biomass declines next year to year after with zero catch ---------------
zero.catch.prob.bio.down.1 <- f(base.model$risks[[1]][1,2])

# Prob biomass declines year after next to year after that with zero catch ----
zero.catch.prob.bio.down.2 <- f(base.model$risks[[2]][1,2])

# Prob biomass declines two year's after next to year after that with zero catch ----
zero.catch.prob.bio.down.3 <- f(base.model$risks[[3]][1,2])

# Prob current biomass being above/below B40%, B25%, and B10% -----------------------
probs.curr.bforty <- f(mean(base.model$mcmc[[paste0("Bratio_", assess.yr)]] > 0.40) * 100, 1)
probs.curr.btwentyfive <- f(mean(base.model$mcmc[[paste0("Bratio_", assess.yr)]] > 0.25) * 100, 1)
probs.curr.bten <- f(mean(base.model$mcmc[[paste0("Bratio_", assess.yr)]] > 0.10) * 100, 0)
probs.curr.below.bforty <- f(mean(base.model$mcmc[[paste0("Bratio_", assess.yr)]] < 0.40) * 100, 1)
probs.curr.below.btwentyfive <- f(mean(base.model$mcmc[[paste0("Bratio_", assess.yr)]] < 0.25) * 100, 1)
probs.curr.below.bten <- f(mean(base.model$mcmc[[paste0("Bratio_", assess.yr)]] < 0.10) * 100, 1)

# For reference points next year given largest catch this year ------------
largest.next.catch.index <- which.max(base.model$risks[[1]][, paste0("ForeCatch_", assess.yr)])
largest.next.catch <- f(base.model$risks[[1]][largest.next.catch.index, paste0("ForeCatch_", assess.yr)], 0)
prob.next.over.b10 <- f(100 - as.numeric(base.model$risks[[1]][largest.next.catch.index, paste0("Bratio_", assess.yr + 1, "<0.10")]), 0)
prob.next.over.b40 <- f(100 - as.numeric(base.model$risks[[1]][largest.next.catch.index, paste0("Bratio_", assess.yr + 1, "<0.40")]), 0)

# Canadian (DFO) provisional reference points ---------------------------------
dfo.probs.curr <- base.model$risks[[1]][,(ncol(base.model$risks[[1]])-2):ncol(base.model$risks[[1]])]
dfo.probs.fore <- base.model$risks[[2]][,(ncol(base.model$risks[[2]])-2):ncol(base.model$risks[[2]])]

# Next year DFO probs given largest catch this year ------------
dfo.prob.next.over.40bmsy <- f(dfo.probs.fore[largest.next.catch.index, paste0("SSB_", assess.yr + 1, ">0.4SSB_MSY")])
dfo.prob.next.over.80bmsy <- f(dfo.probs.fore[largest.next.catch.index, paste0("SSB_", assess.yr + 1, ">0.8SSB_MSY")])
dfo.prob.next.over.bmsy <- f(dfo.probs.fore[largest.next.catch.index, paste0("SSB_", assess.yr + 1, ">SSB_MSY")])

# US (PFMC) stock size reference points based on default Treaty harvest control rule
next.treaty.catch <- f(base.model$catch.levels[[catch.default.policy.ind]][[1]][1], 0)
pfmc.prob.next.year.below.b40 <- f(base.model$risks[[1]][catch.default.policy.ind, paste0("Bratio_", assess.yr + 1, "<0.40")], 0)
pfmc.prob.next.year.below.b25 <- f(base.model$risks[[1]][catch.default.policy.ind, paste0("Bratio_", assess.yr + 1, "<0.25")], 0)
same.catch.as.last.year <- f(base.model$catch.levels[[catch.actual.ind]][[1]][1], 0)
same.catch.prob.next.year.below.b40 <- f(base.model$risks[[1]][catch.actual.ind, paste0("Bratio_", assess.yr + 1, "<0.40")], 0)
same.catch.prob.year.after.next.below.b40 <- f(base.model$risks[[2]][catch.actual.ind, paste0("Bratio_", assess.yr + 2, "<0.40")], 0)

# Prob most recent relative fishing intensity is above target of 1 ------------
probs.curr.rel.fish.intens.above.one <-
  f(sum(base.model$mcmc[[paste0("SPRratio_", end.yr-1)]] > 1) /
    nrow(base.model$mcmc) * 100,
    0)
catches.below.200000.since.1986 <- number.to.word(length(filter(ct, tot_catch <= 200000, year > 1986)$year))

# Age composition data for data section ---------------------------------------
survey.age.years <- base.model$dat$agecomp[base.model$dat$agecomp$FltSvy == 2,]$Yr
max.fishery.age.prop <- get_age_comp_limits(base.model, type = 1)
max.survey.age.prop <- get_age_comp_limits(base.model, type = 2)

# Canadian Freezer trawlers age data ------------------------------------------
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

# Canadian Shoreside age data -------------------------------------------------
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

# US age data -----------------------------------------------------------------
us.age.n.cp <- us.cp.age[us.cp.age$year == last.data.yr, "n.hauls"]
us.age.n.ms <- us.ms.age[us.ms.age$year == last.data.yr, "n.hauls"]
us.last.year.age.cp <- us.cp.age[us.cp.age$year == last.data.yr, grep("^a", colnames(us.cp.age))]
us.last.year.age.cp <- us.last.year.age.cp[order(us.last.year.age.cp, decreasing = TRUE)]
us.age.1.prop.age.cp <- as.numeric(gsub("^a", "", names(us.last.year.age.cp)[1]))
us.age.1.prop.cp <- f(us.last.year.age.cp[1]*100,1)
us.age.2.prop.age.cp <- as.numeric(gsub("^a", "", names(us.last.year.age.cp)[2]))
us.age.2.prop.cp <- f(us.last.year.age.cp[2]*100,1)
us.age.3.prop.age.cp <- as.numeric(gsub("^a", "", names(us.last.year.age.cp)[3]))
us.age.3.prop.cp <- f(us.last.year.age.cp[3]*100,1)
us.age.4.prop.age.cp <- as.numeric(gsub("^a", "", names(us.last.year.age.cp)[4]))
us.age.4.prop.cp <- f(us.last.year.age.cp[4]*100,1)
us.last.year.age.ms <- us.ms.age[us.ms.age$year == last.data.yr, grep("^a", colnames(us.ms.age))]
us.last.year.age.ms <- us.last.year.age.ms[order(us.last.year.age.ms, decreasing = TRUE)]
us.age.1.prop.age.ms <- as.numeric(gsub("^a", "", names(us.last.year.age.ms)[1]))
us.age.1.prop.ms <- f(us.last.year.age.ms[1]*100,1)
us.age.2.prop.age.ms <- as.numeric(gsub("^a", "", names(us.last.year.age.ms)[2]))
us.age.2.prop.ms <- f(us.last.year.age.ms[2]*100,1)
us.age.3.prop.age.ms <- as.numeric(gsub("^a", "", names(us.last.year.age.ms)[3]))
us.age.3.prop.ms <- f(us.last.year.age.ms[3]*100,1)
us.age.4.prop.age.ms <- as.numeric(gsub("^a", "", names(us.last.year.age.ms)[4]))
us.age.4.prop.ms <- f(us.last.year.age.ms[4]*100,1)
us.last.year.age.shore <- us.shore.age[us.shore.age$year == last.data.yr, grep("^a", colnames(us.shore.age))]
us.last.year.age.shore <- us.last.year.age.shore[order(us.last.year.age.shore, decreasing = TRUE)]
us.age.1.prop.age.shore <- as.numeric(gsub("^a", "", names(us.last.year.age.shore)[1]))
us.age.1.prop.shore <- f(us.last.year.age.shore[1]*100,1)
us.age.2.prop.age.shore <- as.numeric(gsub("^a", "", names(us.last.year.age.shore)[2]))
us.age.2.prop.shore <- f(us.last.year.age.shore[2]*100,1)
us.age.3.prop.age.shore <- as.numeric(gsub("^a", "", names(us.last.year.age.shore)[3]))
us.age.3.prop.shore <- f(us.last.year.age.shore[3]*100,1)
us.age.4.prop.age.shore <- as.numeric(gsub("^a", "", names(us.last.year.age.shore)[4]))
us.age.4.prop.shore <- f(us.last.year.age.shore[4]*100,1)

# Years median recruitment is below the mean of the median ---------------------
# recruitments for years >2010 and <(end.yr-1) ; end.yr-1 won't be well estimated
recruitment.med.since.2010 <- base.model$mcmccalcs$rmed[which(names(base.model$mcmccalcs$rmed) %in% 2010:end.yr & names(base.model$mcmccalcs$rmed) %in% start.yr:(end.yr-1))]
years.since.2010.recruitment.med.below.mean <- names(recruitment.med.since.2010[recruitment.med.since.2010  < mean(base.model$mcmccalcs$rmed)])

# Est recruitment in 2014 and 2016 in billions --------------------------------
recruitment.med.in.2014 <- f(base.model$mcmccalcs$rmed["2014"], 3)
last.assess.recruitment.med.in.2014 <- f(last.yr.base.model$mcmccalcs$rmed["2014"], 3)
prob.percent.2014.rec.gt.2010.rec <- f(mean(base.model$mcmc$Recr_2014 > base.model$mcmc$Recr_2010) * 100, 0)
prob.percent.2016.rec.gt.2010.rec <- f(mean(base.model$mcmc$Recr_2016 > base.model$mcmc$Recr_2010) * 100, 1)
prob.percent.2014.rec.gt.2016.rec <- f(mean(base.model$mcmc$Recr_2014 > base.model$mcmc$Recr_2016) * 100, 0) #0 dp since it's 99% for 2021
recruitment.lower.in.2016 <- f(base.model$mcmccalcs$rlower["2016"], 3)
recruitment.med.in.2016 <- f(base.model$mcmccalcs$rmed["2016"], 3)
recruitment.upper.in.2016 <- f(base.model$mcmccalcs$rupper["2016"], 3)
prob.percent.2016.rec.gt.2010.rec <- f(mean(base.model$mcmc$Recr_2016 > base.model$mcmc$Recr_2010) * 100, 1)
sd.med.recr.dev.estimates <- f(sd(base.model$mcmccalcs$devmed[names(base.model$mcmccalcs$devmed) >=
                                                                1970 & names(base.model$mcmccalcs$devmed) <= (last.data.yr - 2)]), 2)
prob.percent.2010.rec.gt.1980.rec <- f(mean(base.model$mcmc$Recr_2010 > base.model$mcmc$Recr_1980) * 100, 0)
prob.percent.2010.rec.gt.1980.rec.last.year.assess <-
  f(mean(bridge.models.1[[1]]$mcmc$Recr_2010 > bridge.models.1[[1]]$mcmc$Recr_1980) * 100, 0)

# Exploitation ----------------------------------------------------------------
exploitation.med.2010 <- f(base.model$mcmccalcs$fmed["2010"],2)
exploitation.med.2012 <- f(base.model$mcmccalcs$fmed["2012"],2)
exploitation.med.2011 <- f(base.model$mcmccalcs$fmed["2011"],2)
exploitation.med.2015 <- f(base.model$mcmccalcs$fmed["2015"],2)
exploitation.med.2017 <- f(base.model$mcmccalcs$fmed["2017"],2)
exploitation.med.2018 <- f(base.model$mcmccalcs$fmed["2018"],2)
exploitation.med.2019 <- f(base.model$mcmccalcs$fmed["2019"],2)
exploitation.med.penult.yr <- f(base.model$mcmccalcs$fmed[as.character(last.data.yr)], 2)

# Priors settings from the control file ---------------------------------------
param.details <- make.parameters.estimated.summary.table(base.model,
                                                         start.rec.dev.yr = recruit.dev.start.yr,
                                                         end.rec.dev.yr = end.yr - 1,
                                                         return.xtable = FALSE)
m.prior <- split.prior.info(param.details[rownames(param.details) == "m.vals",][4],
                            dec.points = 2,
                            first.to.lower = TRUE)
# Now, in document, use m.prior[1] for name of prior, m.prior[1] for mean, and m.prior[3] for SD.
effn.prior <- c(NA, unlist(
  base.model$parameters[grep("DM_theta", base.model$parameters$Label), c("Prior", "Pr_SD")][1, ]))
sel.Phi.val <- base.model$parameters[base.model$parameters$Label=="AgeSel_P3_Fishery(1)_dev_se", "Value"]

# Cohort catch ----------------------------------------------------------------
cohort.catch.1999 <- sum(cohort.catch(base.model, 1999, trim.end.year = end.yr))
cohort.catch.2010 <- sum(cohort.catch(base.model, 2010, trim.end.year = end.yr))
cohort.catch.2014 <- sum(cohort.catch(base.model, 2014, trim.end.year = end.yr))
cohort.catch.2016 <- sum(cohort.catch(base.model, 2016, trim.end.year = end.yr))
cohort.catch.2017 <- sum(cohort.catch(base.model, 2017, trim.end.year = end.yr))

# ... Cumulative sums of Cohorts for use in JMC presentation ------------------
cohortCumSum1999 <- cumsum(cohort.catch(base.model, 1999, trim.end.year = end.yr))
cohortCumSum2010 <- cumsum(cohort.catch(base.model, 2010, trim.end.year = end.yr))
cohortCumSum2014 <- cumsum(cohort.catch(base.model, 2014, trim.end.year = end.yr))
cohortCumSum2016 <- cumsum(cohort.catch(base.model, 2016, trim.end.year = end.yr))
cohortCumSum2017 <- cumsum(cohort.catch(base.model, 2017, trim.end.year = end.yr))
ages1999 <- as.numeric(names(cohortCumSum1999)) - 1999
ages2010 <- as.numeric(names(cohortCumSum2010)) - 2010
ages2014 <- as.numeric(names(cohortCumSum2014)) - 2014
ages2016 <- as.numeric(names(cohortCumSum2016)) - 2016
ages2017 <- as.numeric(names(cohortCumSum2017)) - 2017

# Estimated prop at age (numbers) of the catch in first forecast year ---------
fore.catch.prop <- as.data.frame( t(as.numeric(f(apply(base.model$extra.mcmc$natsel.prop, 2, median)* 100))))
names(fore.catch.prop) <- paste0("Age", 0:20)

# Credible intervals for age5 -------------------------------------------------
# (pick the biggest cohort from fore.catch.prop; note natsel.prop columns start with age-0).
fore.catch.prop.age7.lower <- quantile(base.model$extra.mcmc$natsel.prop[,8], 0.025) * 100
fore.catch.prop.age7.upper <- quantile(base.model$extra.mcmc$natsel.prop[,8], 0.975) * 100

# Estimated prop at age (catch) of catch in first forecast year ---------------
fore.catch.prop.wt.age3.median <- median(base.model$extra.mcmc$natselwt.prop[,4]) * 100
fore.catch.prop.wt.age4.median <-  median(base.model$extra.mcmc$natselwt.prop[,5]) * 100
fore.catch.prop.wt.age5.median <- median(base.model$extra.mcmc$natselwt.prop[,6]) * 100
fore.catch.prop.wt.age10.median <- median(base.model$extra.mcmc$natselwt.prop[,11]) * 100
fore.catch.prop.wt.age11.median <- median(base.model$extra.mcmc$natselwt.prop[,12]) * 100

# Sigma_r, standard deviation of recruitment variability ----------------------
sigma_r <- f(base.model$sigma_R_in, 2)

# Alternative sigma_r based on all years of recdevs ---------------------------
sigma_r_info <- extract_sigma_r(c(list(base.model), sens.models.1),
                                c("base", sens.model.names.1),
                                base.model$sigma_R_in)
sigma_r_hi_main <- sigma_r_info %>%
  filter(model == "Sigma R 1.6", period == "Main") %>%
  pull(SD_of_devs) %>%
  f(2)
sigma_r_lo_main <- sigma_r_info %>%
  filter(model == "Sigma R 1.0", period == "Main") %>%
  pull(SD_of_devs) %>%
  f(2)
sigma_r_alt_allyr <- sigma_r_info %>%
  filter(model == "base", period == "Early+Main+Late") %>%
  pull(alternative_sigma_R) %>%
  f(2)

# Range of "main" recdevs -----------------------------------------------------
main.recdev.start <- min(base.model$recruit$Yr[base.model$recruit$era=="Main"])
main.recdev.end <- max(base.model$recruit$Yr[base.model$recruit$era=="Main"])
main.recdev.early <- min(base.model$recruit$Yr[base.model$recruit$era=="Early"])

# Range of "main" bias adjustement period for recdevs -------------------------
main.recdevbias.start <- min(base.model$recruit$Yr[base.model$recruit$biasadjuster==max(base.model$recruit$biasadjuster)])
main.recdevbias.end <- max(base.model$recruit$Yr[base.model$recruit$biasadjuster==max(base.model$recruit$biasadjuster)])

# Weight-at-age for the base model --------------------------------------------
wt.at.age <- base.model$wtatage[, !grepl("comment", colnames(base.model$wtatage ))] %>%
  filter(Yr %in% start.yr.age.comps:(end.yr - 1),
         Fleet == 2) %>%
  select(-c(Seas, Sex, Bio_Pattern, BirthSeas, Fleet)) %>%
  rename(year = Yr)

# Retrospective setup for the document ----------------------------------------
retro.model.names <- c(base.model.name,
                       map_chr(plot.retro.yrs, ~{paste0("-", .x, ifelse(.x == 1, " year", " years"))}))
# Assemble the retrospective list with the base as the first element
retro.list <- list(base.model)
for(i in plot.retro.yrs){
  retro.list[[i + 1]] <- base.model$retros[[i]]
}
retro.list.age1 <- list(sens.models.2[[1]])
for(i in plot.retro.yrs){
  retro.list.age1[[i + 1]] <- sens.models.2[[1]]$retros[[i]]
}
# Adding the age-1 index for a sensitivity case -------------------------------
retro.model.names.age1 <- c(sens.model.names.2[1],
                       map_chr(plot.retro.yrs, ~{paste0("-", .x, ifelse(.x == 1, " year", " years"))}))

# Define number of 'recent' years for several tables --------------------------
num.recent.yrs <- 10

# Dirichlet-Multinomial data weighting parameters MLE -------------------------
log.theta.fishery <- round(base.model$parameters["ln(EffN_mult)_1", "Value"], 3)
log.theta.survey <- round(base.model$parameters["ln(EffN_mult)_2", "Value"], 3)
theta.fishery <- exp(base.model$parameters["ln(EffN_mult)_1", "Value"])
theta.survey <- exp(base.model$parameters["ln(EffN_mult)_2", "Value"])
# Approximate MLE weights
DM.weight.fishery <- round(theta.fishery/(1 + theta.fishery), 3)
DM.weight.survey <- round(theta.survey/(1 + theta.survey), 3)
# MCMC medians for the fishery and survey, and quantiles (and low and high)
col.effn <- grep("DM_theta.*_1", colnames(base.model$mcmc), perl = TRUE)
# Probably shouldn't really round these values before then using them in the
#  weight calculations. Should use f() for values to be in document not round.
#  No time to look into now (Andy).
log.theta.fishery.median <- round(median(base.model$mcmc[, col.effn]),3)
log.theta.fishery.025    <- round(quantile(base.model$mcmc[, col.effn],
                                           probs = 0.025),
                                  3)
log.theta.fishery.975    <- round(quantile(base.model$mcmc[, col.effn],
                                           probs = 0.975),
                                  3)
DM.weight.fishery.median <- f(median(exp(base.model$mcmc[, col.effn]) /
                                           ( 1 + exp(base.model$mcmc[, col.effn]))), 3)
DM.weight.fishery.025    <- f(exp(log.theta.fishery.025) /
                                    (1 + exp(log.theta.fishery.025)),
                                  3)
DM.weight.fishery.975    <- f(exp(log.theta.fishery.975) /
                                    ( 1 + exp(log.theta.fishery.975)),
                                  3)

col.effn <- grep("DM_theta.*_2", colnames(base.model$mcmc), perl = TRUE)
log.theta.survey.median <- round(median(base.model$mcmc[, col.effn]), 3)
log.theta.survey.025    <- round(quantile(base.model$mcmc[, col.effn],
                                           probs = 0.025),
                                  3)
log.theta.survey.975    <- round(quantile(base.model$mcmc[, col.effn],
                                           probs = 0.975),
                                  3)
DM.weight.survey.median <- f(median(exp(base.model$mcmc[, col.effn]) /
                                          (1 + exp(base.model$mcmc[, col.effn]))), 3)
DM.weight.survey.025    <- f(exp(log.theta.survey.025) /
                                  ( 1 + exp(log.theta.survey.025)),
                                  3)
DM.weight.survey.975    <- f(exp(log.theta.survey.975) /
                                  ( 1 + exp(log.theta.survey.975)),
                                  3)
DM.weight.survey.median <- f(median(exp(base.model$mcmc[, col.effn]) /
                                          (1 + exp(base.model$mcmc[, col.effn]))), 3)
DM.weight.survey.low <- f(min(exp(base.model$mcmc[, col.effn]) /
                                (1 + exp(base.model$mcmc[, col.effn+1]))), 2)
DM.weight.survey.high <- f(max(exp(base.model$mcmc[, col.effn]) /
                                 (1 + exp(base.model$mcmc[, col.effn+1]))), 2)

# MCMC parameter estimates for base model -------------------------------------
# Need to change indexing if sensitivity models order changes in model-setup.R

# ... Natural mortality -------------------------------------------------------
nat_m <- quantile(base.model$mcmc$NatM_uniform_Fem_GP_1, probs = c(0.025, 0.5, 0.975))
nat_m_01 <- quantile(sens.models.1[[5]]$mcmc$NatM_uniform_Fem_GP_1, probs = c(0.025, 0.5, 0.975))
nat_m_03 <- quantile(sens.models.1[[6]]$mcmc$NatM_uniform_Fem_GP_1, probs = c(0.025, 0.5, 0.975))

# ... Steepness ---------------------------------------------------------------
steep <- quantile(base.model$mcmc$SR_BH_steep, probs = c(0.025, 0.5, 0.975))
steep_prior_05 <- quantile(sens.models.1[[1]]$mcmc$SR_BH_steep, probs = c(0.025, 0.5, 0.975))

# ... Bratio ------------------------------------------------------------------
bratio_curr <- quantile(base.model$mcmc[[paste0("Bratio_", assess.yr)]], probs = c(0.025, 0.5, 0.975))
bratio_age1 <- quantile(sens.models.2[[1]]$mcmc[[paste0("Bratio_", assess.yr)]], probs = c(0.025, 0.5, 0.975))

# ... Depletion ---------------------------------------------------------------
depl_curr <- base.model$mcmccalcs$dmed[names(base.model$mcmccalcs$dmed) == assess.yr]
# depl_no_ageerr <- sens.models.5$mcmccalcs$dmed[names(base.model$mcmccalcs$dmed) == assess.yr]

# ... Joint probability -------------------------------------------------------
# (%age) of being being both above the target relative fishing intensity in \Sexpr{end.yr-1}
# and below the $\Bforty$ (40\% of $B_0$) reference point at the start of \Sexpr{end.yr}
joint.percent.prob.above.below <- f(sum(base.model$mcmc[[paste0("Bratio_", end.yr)]] < 0.4 &
                                        base.model$mcmc[[paste0("SPRratio_", end.yr-1)]] > 1) / nrow(base.model$mcmc) * 100,
                                  0)

# Cohort medians, credible intervals ------------------------------------------
rec_2010 <- get_rec_ci(last.yr.base.model, base.model, 2010)
rec_2014 <- get_rec_ci(last.yr.base.model, base.model, 2014)
rec_2016 <- get_rec_ci(last.yr.base.model, base.model, 2016)
rec_2017 <- get_rec_ci(last.yr.base.model, base.model, 2017)

# Cohort biomass-at-age -------------------------------------------------------
baa <- get_baa(base.model, assess.yr)
baa_large <- baa %>%
  arrange(desc(Median))
baa_2010 <- baa %>% filter(Cohort == 2010) %>% pull(Median) * 100
baa_2014 <- baa %>% filter(Cohort == 2014) %>% pull(Median) * 100
baa_2016 <- baa %>% filter(Cohort == 2016) %>% pull(Median) * 100

# Probabilities for historical performance analyses
historical.probs.tibble <- combine_historical_probs(model = base.model,
                                                    end = assess.yr-1) %>% as_tibble()

prob.decline.from.2019.to.2020.historic <-
  dplyr::filter(historical.probs.tibble,
                Year == 2019) %>%
  dplyr::select("P_decline") %>%
  as.numeric() %>%
  f()

prob.decline.from.2019.to.2020.curr <-
  dplyr::filter(historical.probs.tibble,
                Year == 2019) %>%
  dplyr::select("P_decline_curr") %>%
  as.numeric() %>%
  f()

prob.decline.from.2012.to.2013.historic <-
  dplyr::filter(historical.probs.tibble,
                Year == 2012) %>%
  dplyr::select("P_decline") %>%
  as.numeric() %>%
  f()

prob.decline.from.2012.to.2013.curr <-
  dplyr::filter(historical.probs.tibble,
                Year == 2012) %>%
  dplyr::select("P_decline_curr") %>%
  as.numeric() %>%
  f()
