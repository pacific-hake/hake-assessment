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
#' @param check_issues Run [adnuts::launch_shinyadmb()] after initial short run to discover issues. This will
#' stop the function before the main iterations are done, so you will have to re-run the function again
#' with this set to `FALSE`.
#' @param save_image Save the output as an RData file
#' @param extra_mcmc If `TRUE`, run SS extra mcmc option which outputs files into the `sso` subdirectory. If
#'`FALSE`, those files will not be created and the `posteriors.sso` and `dervied_posteriors.sso` files
#' will be in the running directory
#' @param exe The name of the executable
#' @param input_files The input files for SS
#'
#' @return Nothing
run_adnuts <- function(path,
                       n_cores = NA,
                       seed = 352,
                       n_final = 8000,
                       warmup_final = 250,
                       adapt_delta = 0.9,
                       parallel = TRUE,
                       run_mle = TRUE,
                       check_issues = FALSE,
                       save_image = TRUE,
                       extra_mcmc = FALSE,
                       hess_step = FALSE,
                       exe = ifelse(exists("ss_executable"), ss_executable, "ss"),
                       input_files = c(ifelse(exists("starter_file_name"), starter_file_name, "starter.ss"),
                                       ifelse(exists("starter_file_name"), forecast_file_name, "forecast.ss"),
                                       ifelse(exists("weight_at_age_file_name"), weight_at_age_file_name, "wtatage.ss"),
                                       ifelse(exists("control_file_name"), control_file_name, "hake_control.ss"),
                                       ifelse(exists("data_file_name"), data_file_name, "hake_data.ss"))){

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
           " exists and was not modified. Delete it if you want to run the procedure.")
    }
  }
  dir.create(mcmc_path, showWarnings = FALSE)

  if(extra_mcmc){
    dir.create(file.path(mcmc_path, "sso"), showWarnings = FALSE)
  }
  if(hess_step){
    input_files <- c(input_files, "admodel.cov", "admodel.hes", "ss.bar")
  }
  files <- file.path(path, input_files)
  file.copy(files, mcmc_path)

  if(extra_mcmc){
    modify_starter_mcmc_type(mcmc_path, 2)
  }else{
    modify_starter_mcmc_type(mcmc_path, 1)
  }

  rdata_file <- file.path(mcmc_path, "hake.Rdata")

  # First optimize the model to make sure the Hessian is good (and optimize without
  # bias adjustment turned on - i.e., mcmc used)
  system_(paste0("cd ", path, " && ", exe,  " -nox -iprint 200 -mcmc 15"))

  # Then run parallel RWM chains as a first test to ensure
  # mcmc itself is working properly, or that model is converging in mcmc space
  # Start chains from MLE
  pilot <- sample_rwm(model = exe,
                      iter = iter,
                      thin = 10,
                      seeds = seeds,
                      init = NULL,
                      chains = 100 * 10, # 100 * thin
                      warmup = (100 * 10) / 4,  # chains / 4
                      path = pth,
                      cores = reps)
  if(check_issues){
    # Check convergence and slow mixing parameters
    save(list = ls(all.names = TRUE), file = rdata_file, envir = environment())
    mon <- monitor(pilot$samples,
                   warmup = pilot$warmup,
                   print = FALSE)
    # max(mon[,'Rhat'])
    # min(mon[,'n_eff'])
    # Examine the slowest mixing parameters
    slow <- names(sort(mon[,"n_eff"]))[1:8]
    pairs_admb(fit = pilot, pars = slow)
    pairs_admb(fit = pilot, pars = c("MGparm[1]", "SR_parm[1]", "SR_parm[2]"))
  }
  # After regularizing run NUTS chains. First re-optimize to get the
  # correct mass matrix for NUTS. Note the -hbf 1 argument. This is a
  # technical requirement b/c NUTS uses a different set of bounding
  # functions and thus the mass matrix will be different.
  system_(paste0("cd ", path, " && ", exe,  " -hbf 1 -nox -iprint 200 -mcmc 15"))

  if(run_mle){
    system_(paste0("cd ", path, " && ", exe))
    if(hess_step){
      system_(paste0("cd ", path, " && ", exe, " -hbf 1 -nox -iprint 200 -mcmc 15 -hess_step 10 -binp ss.bar"))
    }else{
      # First re-optimize to get the correct mass matrix for NUTS (and without
      # bias adjustment turned on). Note the -hbf 1 argument.
      # This is a technical requirement b/c NUTS uses a different set of bounding
      # functions and thus the mass matrix will be different.
      system_(paste0("cd ", mcmc_path, " && ", exe, " -hbf 1 -nox -iprint 200 -mcmc 15"))
    }
  }

  # Run ADNUTS MCMC
  # Use default MLE covariance (mass matrix) and short parallel NUTS chains
  # started from the MLE. Recall iter is number per core running in parallel.
  nuts_mle <- sample_admb(model = exe,
                          path = mcmc_path,
                          algorithm = "NUTS",
                          iter = 500,
                          init = NULL,
                          seeds = seeds,
                          parallel = parallel,
                          chains = chains,
                          warmup = 100,
                          control = list(metric = "mle",
                                         adapt_delta = adapt_delta),
                          hess_step = hess_step)
  # Check for issues like slow mixing, divergences, max tree depths with
  # ShinyStan and pairs_admb as above. Fix using the shiny app and rerun this part as needed.
  if(check_issues){
    save(list = ls(all.names = TRUE), file = rdata_file, envir = environment())
    launch_shinyadmb(nuts_mle)
    stop("Checked issues. Run function again with check_issues = FALSE to complete run.")
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
                              parallel = parallel,
                              chains = chains,
                              warmup = warmup_final,
                              mceval = TRUE,
                              control = list(metric = mass,
                                             adapt_delta = adapt_delta),
                              hess_step = hess_step)

  save(list = ls(all.names = TRUE), file = rdata_file, envir = environment())
  system_(paste0("cd ", mcmc_path, " && ", exe, " -mceval"))

}
