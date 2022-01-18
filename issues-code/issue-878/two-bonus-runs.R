# two-bonus-runs.R - From Issue #878. Chris ran two more runs, downloaded
#  locally with:
#  s3_download(c("models/2021.00.04_base_2022_code",
#                "models/2022.01.10_base_12_cpus"),
#                ext = "rds")

# Want to check diagnostics:

models_loaded <- FALSE   # don't need the other models, but want functionality
source(here::here("R/all.R"))
if(!(exists("models_loaded") && models_loaded)){
  load_models_rds()
}

source(file.path(rootd.R, "custom-knitr-variables.R"))

# Rerun 2021 base model using 2022 code (adnuts automation etc).
last.yr.base.this.yr.code.dir.name <- "2021.00.04_base_2022_code"
last.yr.base.this.yr.code <- load_models(last.yr.base.this.yr.code.dir.name)

plot_mcmc_param_stats(last.yr.base.this.yr.code)

# This year's base model with 12 cpus.
base.model.12.cpus <- load_models("2022.01.10_base_12_cpus")
plot_mcmc_param_stats(base.model.12.cpus)
