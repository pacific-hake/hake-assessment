#' Run the adnuts MCMC for the model found in the directory given
#'
#' @param path Directory where the model files reside
#' @param n_cores The number of cores to use in parallel MCMC
#' @param seed The random seed
#' @param n_final The number of final samples
#' @param warmup_final The warmup samples (equivalent of burnin)
#' @param adapt_delta The target acceptance rate. See [adnuts::sample_nuts()]
#' @param run_mle Run the MLE before running the MCMC
#' @param check_issues Run [adnuts::launch_shinyadmb()] after imitial short run to discover issues. This will
#' stop the function before the main iterations are done, so you will have to re-run the function again
#' with this set to `FALSE`.
#' @param save_image Save the output as an RData file
#'
#' @return Nothing
run_adnuts <- function(path,
                       n_cores = NA,
                       seed = 352,
                       n_final = 8000,
                       warmup_final = 250,
                       adapt_delta = 0.9,
                       run_mle = TRUE,
                       check_issues = FALSE,
                       save_image = TRUE){

  tic()
  # Chains to run in parallel
  num_machine_cores <- detectCores()
  reps <- ifelse(is.na(n_cores), num_machine_cores - 1, n_cores)
  stopifnot(num_machine_cores >= reps)
  set.seed(seed)
  seeds <- sample(1:1e4, size = reps)
  mcmc_path <- file.path(path, "mcmc")
  if(dir.exists(mcmc_path)){
    stop("The MCMC directory ", mcmc_path, " exists. Delete it if manually you want to run this and run again.",
         call. = FALSE)
  }
  dir.create(mcmc_path, showWarnings = FALSE)
  files <- file.path(path, dir(path))
  files_to <- file.path(mcmc_path, dir(path))
  file.copy(files, files_to)

  exe <- "ss"
  curr_path <- getwd()

  if(run_mle){
    setwd(path)
    system(exe)
    setwd(curr_path)
  }

  # Run ADNUTS MCMC
  rdata_file <- "hake.Rdata"
  # First re-optimize to get the correct mass matrix for NUTS (and without
  # bias adjustment turned on). Note the -hbf 1 argument.
  # This is a technical requirement b/c NUTS uses a different set of bounding
  # functions and thus the mass matrix will be different.
  setwd(mcmc_path)
  system(paste0(exe, " -hbf 1 -nox -iprint 200 -mcmc 15"))
  save.image(file = rdata_file)
  # Use default MLE covariance (mass matrix) and short parallel NUTS chains
  # started from the MLE. Recall iter is number per core running in parallel.
  nuts_mle <- sample_nuts(model = exe,
                          iter = 100,
                          init = NULL,
                          seeds = seeds,
                          chains = reps,
                          warmup = 50,
                          path = ".",
                          cores = reps,
                          control = list(metric = "mle",
                                         adapt_delta = adapt_delta))
  save.image(file = rdata_file)
  # Check for issues like slow mixing, divergences, max treedepths with
  # ShinyStan and pairs_admb as above. Fix using the shiny app and rerun this part as needed.
  if(check_issues){
    launch_shinyadmb(nuts_mle)
    stop("Checked issues. Run function again with check_issues = FALSE to complete run.",
         call. = FALSE)
  }
  # Once acceptable, run again for inference using updated mass matrix. Increase
  # adapt_delta toward 1 if you have divergences (runs will take longer).
  # Note this is in unbounded parameter space
  mass <- nuts_mle$covar.est
  inits <- sample_inits(nuts_mle, reps)
  iterations <- ceiling(((reps * warmup_final) + n_final) / reps)
  # The following, nuts_updated, is used for inferences
  nuts_updated <- sample_nuts(model = exe,
                              iter = iterations,
                              init = inits,
                              seeds = seeds,
                              chains = reps,
                              warmup = warmup_final,
                              path = ".",
                              cores = reps,
                              mceval = TRUE,
                              control = list(metric = mass,
                                             adapt_delta = adapt_delta))
  save.image(file = rdata_file)
  system(paste0(exe, " -mceval"))
  setwd(curr_path)
  toc()
}