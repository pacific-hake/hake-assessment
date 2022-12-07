# Run this to setup your workspace to run custom ss compilations and tests
# See bash_scripts/compile_ss.sh

get_os <- adnuts::get_os
Sys.setenv(PATH = paste0("/home/cgrandin/ss:", Sys.getenv("PATH")))

run_adnuts('models/2022.01.10_base_local_nutsscript', adapt_delta = 0.6, n_final = 20, warmup_final = 1)

#posteriors_dir <- "models/2022.01.10_base_12000_recompiled/mcmc/sso"
posteriors_dir <- "models/2022.01.10_base_local_nutsscript"
model <- list()
tmp <- readLines(file.path(posteriors_dir, "posteriors.sso"), n = 1)
tmp <- stringr::str_split(tmp, "[:space:]")[[1]]
# Remove Empty string, Iter and Objective_function as they are not parameters
model$post_names <- tmp[!tmp %in% c("", "Iter", "Objective_function")]
fix.posteriors(posteriors_dir)
model$mcmc <- SSgetMCMC(dir = posteriors_dir,
                        writecsv = FALSE,
                        verbose = FALSE)
# replace any SPB with SSB
names(model$mcmc) <- gsub(pattern="SPB", replacement="SSB", names(model$mcmc))

plot_mcmc_param_stats(model)
