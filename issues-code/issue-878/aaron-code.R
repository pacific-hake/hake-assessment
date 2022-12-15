# aaron-code.R From Issue #878. Gives error when doing my_mcmc(). Am thinking it
#  needs files from mcmc/ folder also (which is what the .rds is trying to
#  avoid). Committing this to share, but then we can delete it.

# Run these line-by-line as necessary

# Not fully needed but makes it easy to use functionality:
source(here::here("R/all.R"))
if(!(exists("models_loaded") && models_loaded)){
  load_models_rds()
}

models_loaded <- TRUE
source(file.path(rootd.R, "custom-knitr-variables.R"))

# Run this once, from hake-assessment/
#s3_download(c(paste0("models/", base_model_dir_name, "/Report.sso"),
#              paste0("models/", base_model_dir_name, "/CompReport.sso"),
#              paste0("models/", base_model_dir_name, "/posteriors.sso")))

# Aaron used these r4ss commands, Andy adding in base_model_dir_name:
my_mcmc <-SSgetMCMC(dir = paste0(rootd, "/models/", base_model_dir_name),
                    verbose = TRUE,
                    writecsv = TRUE,
                    csv1 = "keyposteriors.csv",
                    csv2 = "nuisanceposteriors.csv",
                    keystrings = c("SSB_2019",
                                   "Bratio_2019",
                                   "SR_LN(R0)",
                                   "SR_BH_steep",
                                   "RecrDev_2014",
                                   "EffN_mult"),
                    nuisancestrings = c("Objective_function",
                                        "SSB_",
                                        "Bratio_",
                                        "InitAge",
                                        "RecrDev"))

Andy got to here.
pdf("SummaryDiagnostics.pdf")
mcmc.nuisance(dir="...\models\",run="mymodel\",printstats=TRUE)
dev.off()

Aarong:

plot_mcmc_param_stats says it is a recoded version of the mcmc.nuisance function used above.

and got using the base model output on a run I did on my remote desktop (7cores):
