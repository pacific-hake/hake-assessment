################################################################################
## Data table loading
################################################################################

data.path <- file.path("..","..","data")

can.age.file <- "canadian-age-data.csv"
catch.data.file <- "landings-tac-history.csv"
further.tac.file <- "further-tac-details.csv"
survey.history.file <- "survey-history.csv"
survey.comparison.file <- "survey-comparison.csv"
sampling.history.file <- "fishery-sampling-history.csv"
ovary.samples.file <- "ovary-samples.csv"
age.1.file <- "age-1.csv"
assessment.history.file <- "assessment-history.csv"

cat("Loading all data tables (csv files) from ", data.path, "\n")
catches <- load.catches(file.path(data.path, catch.data.file))
landings.vs.tac <- catches[[2]]
catches <- catches[[1]]
survey.history <- load.survey.history(file.path(data.path, survey.history.file))
survey.comparison <- read.csv(file.path(data.path, survey.comparison.file), stringsAsFactors = FALSE)
sampling.history <- load.sampling.history(file.path(data.path, sampling.history.file))
further.tac <- further.tac.details(file.path(data.path, further.tac.file))
can.ages <- load.can.age.data(file.path(data.path, can.age.file))
ovary.samples <- read.csv(file.path(data.path, ovary.samples.file), stringsAsFactors = FALSE)
age.1.index <- read.csv(file.path(data.path, age.1.file), stringsAsFactors = FALSE)
assessment.history <- read.csv(file.path(data.path, assessment.history.file), stringsAsFactors = FALSE)
cat("All data tables have been loaded ", data.path, "\n")
