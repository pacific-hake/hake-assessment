#' Run the adnuts MCMC for the model found in the directory given
#'
#' @details
#' `path` is the directory in which the MLE will be run, a subdirectory of this called `mcmc` is
#' where the MCMC will be run using the NUTS algorithm. Inside the `mcmc` directory, several
#' temporary subdirectories will be created, one for each MCMC chain labeled `chain_*`, AkA
#' CPU number used in the parallel execution. These will disappear once the run has completed and
#' the output has been merged.
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


  # Chains to run in parallel
  num_machine_cores <- detectCores()
  chains <- ifelse(is.na(n_cores), num_machine_cores - 1, n_cores)
  stopifnot(num_machine_cores >= chains)
  set.seed(seed)
  seeds <- sample(1:1e4, size = chains)
  mcmc_path <- file.path(path, "mcmc")
  if(dir.exists(mcmc_path)){
    if(interactive()){
      ovrw <- menu(c("Yes", "No"), title = paste0(mcmc_path, " directory exists. Overwrite?"))
    }else{
      ovrw <- 1
    }
    if(ovrw == 1){
      unlink(mcmc_path, recursive = TRUE, force = TRUE)
    }else{
      stop("The MCMC directory ",
           mcmc_path,
           " exists and was not modified. Delete it if you want to run the procedure.",
           call. = FALSE)
    }
  }
  dir.create(mcmc_path, showWarnings = FALSE)
  dir.create(file.path(mcmc_path, "sso"), showWarnings = FALSE)
  files <- file.path(path, dir(path))
  files_to <- file.path(mcmc_path, dir(path))
  file.copy(files, files_to)

  exe <- "ss"

  if(run_mle){
    system_(paste0("cd ", path, " && ", exe))
  }

  # Run ADNUTS MCMC
  rdata_file <- "hake.Rdata"
  # First re-optimize to get the correct mass matrix for NUTS (and without
  # bias adjustment turned on). Note the -hbf 1 argument.
  # This is a technical requirement b/c NUTS uses a different set of bounding
  # functions and thus the mass matrix will be different.
  system_(paste0("cd ", mcmc_path, " && ", exe, " -hbf 1 -nox -iprint 200 -mcmc 15"))
  save.image(file = rdata_file)
  # Use default MLE covariance (mass matrix) and short parallel NUTS chains
  # started from the MLE. Recall iter is number per core running in parallel.
  nuts_mle <- sample_admb(model = exe,
                          path = mcmc_path,
                          algorithm = "NUTS",
                          iter = 100,
                          init = NULL,
                          seeds = seeds,
                          chains = chains,
                          warmup = 50,
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
  inits <- sample_inits(nuts_mle, chains)
  iterations <- ceiling(((chains * warmup_final) + n_final) / chains)
  # The following, nuts_updated, is used for inferences
  nuts_updated <- sample_admb(model = exe,
                              path = mcmc_path,
                              algorithm = "NUTS",
                              iter = iterations,
                              init = inits,
                              seeds = seeds,
                              chains = chains,
                              warmup = warmup_final,
                              mceval = TRUE,
                              control = list(metric = mass,
                                             adapt_delta = adapt_delta))
  save.image(file = rdata_file)
  system_(paste0("cd ", mcmc_path, " && ", exe, " -mceval"))

}