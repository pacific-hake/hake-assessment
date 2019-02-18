################################################################################
## Data tables (input and output)
################################################################################
catch.data.file <- "landings-tac-history.csv"
further.tac.file <- "further-tac-details.csv"
survey.history.file <- "survey-history.csv"
survey.comparison.file <- "survey-comparison.csv"
survey.summary.file <- "survey-summary.csv"
sampling.history.file <- "fishery-sampling-history.csv"
ovary.samples.file <- "ovary-samples.csv"
maturity.ogives.file <- "maturity-table.csv"
maturity.samples.file <- "hake-maturity-data.csv"
age.1.file <- "age-1.csv"
assessment.history.file <- "assessment-history.csv"
assessment.changes.file <- "assessment-changes.csv"
kriging.parameters.file <- "kriging-parameters.csv"

## Canada-specific data
can.age.file <- "can-age-data.csv"
can.ft.catch.by.month.file <- "can-ft-catch-by-month.csv"
can.ss.catch.by.month.file <- "can-ss-catch-by-month.csv"
can.ft.bottom.depth.file <- "depth-can-freezer-trawlers.csv"
can.ss.bottom.depth.file <- "depth-can-shoreside.csv"

## US-specific data
## The following are used for cumulative catch plot in the data/fisheries presentation, not the assessment document
us.shore.catch.by.month.file <- "us-shore-catch-by-month.csv"
us.cp.catch.by.month.file <- "us-cp-catch-by-month.csv"
us.ms.catch.by.month.file <- "us-ms-catch-by-month.csv"
us.research.catch.by.month.file <- "us-research-catch-by-month.csv"
us.ap.catch.file <- "us-ap-catch.csv"
## The following are used for the age comp-by fleet plot in the data/fisheries presentation, not the assessment document
us.shore.age.data.file <- "us-shore-age-data.csv"
us.cp.age.data.file <- "us-cp-age-data.csv"
us.ms.age.data.file <- "us-ms-age-data.csv"
us.atsea.bottom.depth.file <- "depth-us-atsea-bottom.csv"
us.atsea.fishing.depth.file <- "depth-us-atsea-fishing.csv"

cat("Loading all data tables (csv files) from ", rootd.data, "\n")
catches <- load.catches(file.path(rootd.data, catch.data.file))
landings.vs.tac <- catches[[2]]
catches <- catches[[1]]
survey.history <- load.survey.history(file.path(rootd.data, survey.history.file))
survey.comparison <- read.csv(file.path(rootd.data, survey.comparison.file), stringsAsFactors = FALSE)
survey.summary <- read.csv(file.path(rootd.data, survey.summary.file), stringsAsFactors = FALSE)
sampling.history <- load.sampling.history(file.path(rootd.data, sampling.history.file))
further.tac <- further.tac.details(file.path(rootd.data, further.tac.file))
can.ages <- load.can.age.data(file.path(rootd.data, can.age.file))
ovary.samples <- read.csv(file.path(rootd.data, ovary.samples.file), stringsAsFactors = FALSE)
maturity.ogives <- read.csv(file.path(rootd.data, maturity.ogives.file), stringsAsFactors = FALSE)
maturity.samples <- read.csv(file.path(rootd.data, maturity.samples.file), stringsAsFactors = FALSE)
age.1.index <- read.csv(file.path(rootd.data, age.1.file), stringsAsFactors = FALSE)
assessment.history <- read.csv(file.path(rootd.data, assessment.history.file), stringsAsFactors = FALSE)
assessment.changes <- read.csv(file.path(rootd.data, assessment.changes.file), stringsAsFactors = FALSE)
kriging.pars <- read.csv(file.path(rootd.data, kriging.parameters.file), comment.char="#", stringsAsFactors = FALSE)

## For cumulative catch plots in the data presentation
can.ft.catch.by.month <- read.csv(file.path(rootd.data, can.ft.catch.by.month.file), stringsAsFactors = FALSE)
can.shore.catch.by.month <- read.csv(file.path(rootd.data, can.ss.catch.by.month.file), stringsAsFactors = FALSE)
us.shore.catch.by.month <- read.csv(file.path(rootd.data, us.shore.catch.by.month.file), stringsAsFactors = FALSE)
us.cp.catch.by.month <- read.csv(file.path(rootd.data, us.cp.catch.by.month.file), stringsAsFactors = FALSE)
us.ms.catch.by.month <- read.csv(file.path(rootd.data, us.ms.catch.by.month.file), stringsAsFactors = FALSE)
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
