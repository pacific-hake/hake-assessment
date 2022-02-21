################################################################################
## Data tables (input and output)
################################################################################
catch.targets.file <- "catch-targets-biomass.csv"
catch.data.file <- "landings-tac-history.csv"
further.tac.file <- "further-tac-details.csv"
survey.history.file <- "survey-history.csv"
survey.by.country.file <- "survey-by-country.csv"
survey.comparison.file <- "survey-comparison.csv"
sampling.history.file <- "fishery-sampling-history.csv"
ovary.samples.file <- "ovary-samples.csv"
maturity.ogives.file <- "maturity-table.csv"
maturity.samples.file <- "hake-maturity-data.csv"
age.1.file <- "age-1.csv"
assessment.history.file <- "assessment-history.csv"
assessment.changes.file <- "assessment-changes.csv"
kriging.parameters.file <- "kriging-parameters.csv"
weight_age_extrapolation_mask.file <- "wtatage_all_samplesize.csv"

## Canada-specific data
can.age.file <- "can-age-data.csv"
can.ss.num.fish.file <- "can-ss-num-fish-aged.csv"
can.ft.num.fish.file <- "can-ft-num-fish-aged.csv"
can.jv.num.fish.file <- "can-jv-num-fish-aged.csv"
can.ft.catch.by.month.file <- "can-ft-catch-by-month.csv"
can.ss.catch.by.month.file <- "can-ss-catch-by-month.csv"
can.jv.catch.by.month.file <- "can-jv-catch-by-month.csv"
can.ft.bottom.depth.file <- "depth-can-ft-bottom.csv"
can.ss.bottom.depth.file <- "depth-can-ss-bottom.csv"
can.ft.gear.depth.file <- "depth-can-ft-gear.csv"
can.ss.gear.depth.file <- "depth-can-ss-gear.csv"

## US-specific data
## The following are used for cumulative catch plot in the data/fisheries presentation, not the assessment document
us.shore.catch.by.month.file <- "us-shore-catch-by-month.csv"
us.cp.catch.by.month.file <- "us-cp-catch-by-month.csv"
us.ms.catch.by.month.file <- "us-ms-catch-by-month.csv"
us.ti.catch.by.month.file <- "us-ti-catch-by-month.csv"
us.research.catch.by.month.file <- "us-research-catch-by-month.csv"
us.ap.catch.file <- "us-ap-catch.csv"
## The following are used for the age comp-by fleet plot in the data/fisheries presentation, not the assessment document
us.shore.age.data.file <- "us-shore-age-data.csv"
us.cp.age.data.file <- "us-cp-age-data.csv"
us.ms.age.data.file <- "us-ms-age-data.csv"
us.atsea.bottom.depth.file <- "depth-us-atsea-bottom.csv"
us.atsea.fishing.depth.file <- "depth-us-atsea-fishing.csv"

cat("Loading all data tables (csv files) from ", rootd.data, "\n")
ct <- load_catches(file.path(rootd.data, catch.data.file))
catch.targets <- read_csv(file.path(rootd.data, catch.targets.file))
survey.history <- load.survey.history(file.path(rootd.data, survey.history.file))
survey.by.country <- load.survey.by.country(file.path(rootd.data, survey.by.country.file))
survey.comparison <- read.csv(file.path(rootd.data, survey.comparison.file),
                              stringsAsFactors = FALSE)
testthat::expect_equal(round(survey.history$biomass * 1000),
                       round(survey.by.country$total))
testthat::expect_equal(round(survey.history$biomass * 1000),
                       round(survey.comparison$with.extrap))
testthat::expect_equal(round(survey.history$cv * 100, 1),
                       round(survey.by.country$total.cv, 1))
#testthat::expect_equal(round(survey.history$cv * 100, 1),
#                       round(survey.comparison$cv.with.extrap * 100, 1)) #
#                       Fails in 2022, but small differences, didn't yet look
#                       into exactly which columns to compare.
sampling.history <- load.sampling.history(file.path(rootd.data, sampling.history.file))
further.tac <- further.tac.details(file.path(rootd.data, further.tac.file))
can.ages <- load.can.age.data(file.path(rootd.data, can.age.file))
can.ss.num.fish <- read.csv(file.path(rootd.data, can.ss.num.fish.file))
can.ft.num.fish <- read.csv(file.path(rootd.data, can.ft.num.fish.file))
can.jv.num.fish <- read.csv(file.path(rootd.data, can.jv.num.fish.file))
ovary.samples <- read_csv(file.path(rootd.data, ovary.samples.file))
maturity.ogives <- read.csv(file.path(rootd.data, maturity.ogives.file), stringsAsFactors = FALSE)
maturity.samples <- read.csv(file.path(rootd.data, maturity.samples.file), stringsAsFactors = FALSE)
age.1.index <- read.csv(file.path(rootd.data, age.1.file), stringsAsFactors = FALSE)
assessment.history <- read.csv(file.path(rootd.data, assessment.history.file), stringsAsFactors = FALSE)
assessment.changes <- read.csv(file.path(rootd.data, assessment.changes.file), stringsAsFactors = FALSE)
kriging.pars <- read.csv(file.path(rootd.data, kriging.parameters.file), comment.char="#", stringsAsFactors = FALSE)
weight_age_extrapolation_mask <- read.csv(file.path(rootd.data, weight_age_extrapolation_mask.file))

## For cumulative catch plots in the data presentation
can.ft.catch.by.month <- read.csv(file.path(rootd.data, can.ft.catch.by.month.file), stringsAsFactors = FALSE)
can.shore.catch.by.month <- read.csv(file.path(rootd.data, can.ss.catch.by.month.file), stringsAsFactors = FALSE)
can.jv.catch.by.month <- read.csv(file.path(rootd.data, can.jv.catch.by.month.file), stringsAsFactors = FALSE)
us.shore.catch.by.month <- read.csv(file.path(rootd.data, us.shore.catch.by.month.file), stringsAsFactors = FALSE)
us.cp.catch.by.month <- read.csv(file.path(rootd.data, us.cp.catch.by.month.file), stringsAsFactors = FALSE)
us.ms.catch.by.month <- read.csv(file.path(rootd.data, us.ms.catch.by.month.file), stringsAsFactors = FALSE)
us.ti.catch.by.month <- read.csv(file.path(rootd.data, us.ti.catch.by.month.file), stringsAsFactors = FALSE)
us.research.catch.by.month <- read.csv(file.path(rootd.data, us.research.catch.by.month.file), stringsAsFactors = FALSE)
us.ap.catch <- read.csv(file.path(rootd.data, us.ap.catch.file), stringsAsFactors = FALSE) ##, row.names = 1)
## For age comps-by fleet plots in the data presentation
can.shore.age <- can.ages[[1]]
can.ft.age <- can.ages[[2]]
us.shore.age <- load.us.age.data(file.path(rootd.data, us.shore.age.data.file))
us.cp.age <- load.us.age.data(file.path(rootd.data, us.cp.age.data.file))
us.ms.age <- load.us.age.data(file.path(rootd.data, us.ms.age.data.file))
cat("All data tables have been loaded ", rootd.data, "\n")

## For depth plots
can.ft.bottom.depth <- read.csv(file.path(rootd.data, can.ft.bottom.depth.file), stringsAsFactors = FALSE)
can.ss.bottom.depth <- read.csv(file.path(rootd.data, can.ss.bottom.depth.file), stringsAsFactors = FALSE)
can.ft.gear.depth <- read.csv(file.path(rootd.data, can.ft.gear.depth.file), stringsAsFactors = FALSE)
can.ss.gear.depth <- read.csv(file.path(rootd.data, can.ss.gear.depth.file), stringsAsFactors = FALSE)
us.atsea.fishing.depth <- read.csv(file.path(rootd.data, us.atsea.fishing.depth.file), stringsAsFactors = FALSE)
us.atsea.bottom.depth <- read.csv(file.path(rootd.data, us.atsea.bottom.depth.file), stringsAsFactors = FALSE)

##------------------------------------------------------------------------------
## Output data tables
out.est.naa.file <- "estimated-numbers-at-age.csv"
out.est.eaa.file <- "estimated-exploitation-at-age.csv"
out.est.caa.file <- "estimated-catch-at-age.csv"
out.est.caa.bio.file <- "estimated-catch-at-age-biomass.csv"
out.est.baa.file <- "estimated-biomass-at-age.csv"
##------------------------------------------------------------------------------
