#' Run the adnuts MCMC for the model found in the directory given
#'
#' @details
#' `path` is the directory in which the MLE will be run, a subdirectory of
#' this called `mcmc` is where the MCMC will be run using the `NUTS`
#' algorithm. Inside the `mcmc` directory, several temporary subdirectories
#' will be created, one for each MCMC chain labeled `chain_*`, AkA CPU
#' number used in the parallel execution. These will disappear once the
#' run has completed and the output has been merged.
#'
#' @param path Directory where the model files reside
#' @param num_chains The number of chains to run in parallel. If `NULL`,
#' 1 less than the number of cores on the machine will be used
#' 1 less than the number of cores on the machine will be used
#' @param seed The random seed used to draw the random seeds for each chain
#' @param num_samples The number of samples to output
#' @param num_warmup_samples The warmup samples (equivalent of burnin)
#' @param adapt_delta The target acceptance rate. See [adnuts::sample_nuts()]
#' @param check_issues Run [adnuts::launch_shinyadmb()] after initial short
#' run to discover issues. This will stop the function before the main
#' iterations are done, so you will have to re-run the function again
#' with this set to `FALSE`.
#' @param save_image Save the output as an RData file
#' @param run_extra_mcmc If `TRUE`, run SS extra mcmc option which outputs
#' files into the `sso` subdirectory. If `FALSE`, those files will not be
#' created and the `posteriors.sso` and `dervied_posteriors.sso` files
#' will be in the running directory
#' @param fn_exe The name of the executable which was built using ADMB
#' @param overwrite Logical. If `TRUE`, don't ask user if they want to
#' overwrite if the directory already exists, just do it
#' @param input_files The input files for SS
#'
#' @importFrom crayon green
#' @importFrom cli symbol
#' @importFrom lubridate seconds_to_period
#'
#' @return Nothing
run_adnuts <- function(path,
                       num_chains = NULL,
                       seed = 42,
                       num_samples = 8000,
                       num_warmup_samples = 250,
                       adapt_delta = 0.8,
                       check_issues = FALSE,
                       save_image = TRUE,
                       run_extra_mcmc = FALSE,
                       hess_step = FALSE,
                       duration = NULL,
                       fn_exe = ifelse(exists("ss_executable"),
                                    ss_executable,
                                    "ss3"),
                       overwrite = FALSE,
                       fn_logfile = "model_output.log"){

  # Determine if the caller is calling from an Rstudio session
  is_rstudio <- Sys.getenv("RSTUDIO") == "1"

  # Chains to run in parallel
  num_machine_cores  <- detectCores()
  if(is.null(num_chains)){
    num_chains <- num_machine_cores - 1
  }
  if(num_machine_cores <= num_chains){
    msg <- paste0("The number of available cores (", num_machine_cores, ") ",
                  "is less than or equal to the number of chains ",
                  "requested (", num_chains, ")")
    if(is_rstudio){
      message(red(symbol$cross, msg))
      stop_quietly(call. = FALSE)
    }else{
      stop(msg, call. = FALSE)
    }
  }

  set.seed(seed)
  seeds <- sample(1:1e4, size = num_chains)
  mcmc_path <- file.path(path, "mcmc")
  if(dir.exists(mcmc_path)){
    ovrw <- 1
    if(!overwrite && interactive()){
      ovrw <- menu(c("Yes", "No"),
                   title = paste0(mcmc_path, " directory exists. Overwrite?"))
    }
    if(ovrw == 1){
      unlink(mcmc_path, recursive = TRUE, force = TRUE)
    }else{
      msg <- paste0("The `mcmc` directory `", mcmc_path, "` exists and was ",
                    "not modified. Delete it or set `ovrwrite` to `TRUE` if ",
                    "you want to run the procedure.")
      if(is_rstudio){
        message(red(symbol$cross, msg))
        stop_quietly(, call. = FALSE)
      }else{
        stop(msg, call. = FALSE)
      }
    }
  }
  dir.create(mcmc_path, showWarnings = FALSE)

  rdata_file <- file.path(mcmc_path, "hake.Rdata")

  message("\n")
  if(!is.null(fn_logfile)){
    msg <- paste0("The logfile name `", fn_logfile, "` was supplied so no ",
                  "ADMB model output will appear in the console while the ",
                  "adnuts procedure is running\n")
    if(is_rstudio){
      message(green(symbol$circle_double, msg))
    }else{
      message(msg)
    }
  }

  msg <- paste0("Running optimization MCMC (chain length 15) ",
                "to ensure hessian is good ",
                "and optimize without bias adjustment turned on\n")
  if(is_rstudio){
    message(green(symbol$info, msg))
  }else{
    message(msg)
  }
  cmd <- paste0("cd ", path, " && ", fn_exe,  " -nox -iprint 200 -mcmc 15")
  if(!is.null(fn_logfile)){
    cmd <- paste0(cmd, " > ", fn_logfile, " 2>&1")
  }
  system_(cmd, intern = TRUE, wait = TRUE)

  # Then run parallel RWM chains as a first test to ensure
  # mcmc itself is working properly, or that model is converging in mcmc space
  # Start chains from MLE
  msg <- "Running Pilot RWM MCMC (chain length 100, warmup 25)...\n"
  if(is_rstudio){
    message(green(symbol$info, msg))
  }else{
    message(msg)
  }
  pilot <- sample_admb(model = fn_exe,
                       path = path,
                       num_samples = 100,
                       init = NULL,
                       num_chains = num_chains,
                       warmup = 25,
                       seeds = seeds,
                       algorithm = "rwm",
                       duration = duration,
                       fn_logfile = fn_logfile)

  # Copy all input/output from the MLE run to the `mcmc` directory
  # This way, all input to the mcmc run will be based on the MLE outputs
  if(run_extra_mcmc){
    dir.create(file.path(mcmc_path, "sso"), showWarnings = FALSE)
  }
  mle_files <- list.files(path, all.files = TRUE, full.names = TRUE)
  file.copy(mle_files, mcmc_path)

  if(run_extra_mcmc){
    modify_starter_mcmc_type(mcmc_path, 2)
  }else{
    modify_starter_mcmc_type(mcmc_path, 1)
  }

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

  # The -hbf 1 argument is a technical requirement because NUTS uses a
  # different set of bounding functions and thus the mass matrix will be
  # different
  # First, run MLE
  cmd <- paste0("cd ", path, " && ", fn_exe)
  if(!is.null(fn_logfile)){
    cmd <- paste0(cmd, " > ", fn_logfile, " 2>&1")
  }
  system_(cmd, intern = TRUE, wait = TRUE)
  if(hess_step){
    msg <- paste0("Running re-optimization MCMC (chain length 15) ",
                  "to get the correct mass ",
                  "matrix for the NUTS run (use `-hbf 1`) and the ",
                  "`hess_step` algorithm\n")
    if(is_rstudio){
      message(green(symbol$info, msg))
    }else{
      message(msg)
    }
    cmd <- paste0("cd ", path, " && ", fn_exe,
                  " -hbf 1 -nox -iprint 200 -mcmc 15 -hess_step 10 ",
                  "-binp ss.bar")
    if(!is.null(fn_logfile)){
      cmd <- paste0(cmd, " > ", fn_logfile, " 2>&1")
    }
    system_(cmd, intern = TRUE, wait = TRUE)
  }else{
    msg <- paste0("Running re-optimization MCMC (chain length 15) ",
                  "to get the correct mass matrix ",
                  "for the NUTS run (use `-hbf 1`)\n")
    if(is_rstudio){
      message(green(symbol$info, msg))
    }else{
      message(msg)
    }
    cmd <- paste0("cd ", mcmc_path, " && ",
                  fn_exe, " -hbf 1 -nox -iprint 200 -mcmc 15")
    if(!is.null(fn_logfile)){
      cmd <- paste0(cmd, " > ", fn_logfile, " 2>&1")
    }
    system_(cmd, intern = TRUE, wait = TRUE)
  }

  # Run ADNUTS MCMC
  msg <- paste0("Running Initial NUTS MCMC (chain length 500, warmup 100) ",
                "with MLE mass matrix\n")
  if(is_rstudio){
    message(green(symbol$info, msg))
  }else{
    message(msg)
  }
  nuts_initial <- sample_admb(model = fn_exe,
                              path = mcmc_path,
                              algorithm = "nuts",
                              num_samples = 500,
                              seeds = seeds,
                              num_chains = num_chains,
                              warmup = 100,
                              control = list(metric = "mle",
                                             adapt_delta = adapt_delta),
                              fn_logfile = fn_logfile)

  # Check for issues like slow mixing, divergences, max tree depths with
  # ShinyStan and pairs_admb as above. Fix using the shiny app and rerun
  # this part as needed.
  if(check_issues){
    save(list = ls(all.names = TRUE), file = rdata_file, envir = environment())
    launch_shinyadmb(nuts_initial)
    msg <- paste0("Stopped execution becuse `check_issues` is `TRUE`. Once ",
                  "you have ensured the hessian is positive definite, ",
                  "checked the slow mixing parameters, and looked at the ",
                  "output using `shiny`, then re-run with ",
                  "`check_issues` set to `FALSE`\n")
    if(is_rstudio){
      message(red(symbol$cross, msg))
      stop_quietly(call. = FALSE)
    }else{
      stop(msg, call. = FALSE)
    }
  }
  # Once acceptable, run again for inference using updated mass matrix.
  # Increase adapt_delta toward 1 if you have divergences (runs will take
  # longer). Note this is in unbounded parameter space
  mass <- nuts_initial$covar.est
  inits <- sample_inits(nuts_initial, num_chains)
  num_iters <- ceiling(((num_chains * num_warmup_samples) + num_samples) / num_chains)

  msg <- paste0("Running updated NUTS MCMC for inference, acceptance ",
                "ratio (adapt_delta) = ", adapt_delta, "\n")
  if(is_rstudio){
    message(green(symbol$info, msg))
  }else{
    message(msg)
  }
  nuts_updated <- sample_admb(model = fn_exe,
                              path = mcmc_path,
                              algorithm = "nuts",
                              num_samples = num_iters,
                              init = inits,
                              seeds = seeds,
                              num_chains = num_chains,
                              warmup = num_warmup_samples,
                              mceval = TRUE,
                              control = list(metric = mass,
                                             adapt_delta = adapt_delta),
                              fn_logfile = fn_logfile)

  save(list = ls(all.names = TRUE), file = rdata_file, envir = environment())

  cmd <- paste0("cd ", mcmc_path, " && ", fn_exe, " -mceval")
  if(!is.null(fn_logfile)){
    cmd <- paste0(cmd, " > ", fn_logfile, " 2>&1")
  }
  system_(cmd, intern = TRUE, wait = TRUE)

  msg <- "Finished `run_adnuts()`\n"
  if(is_rstudio){
    message(green(symbol$info, msg))
  }else{
    message(msg)
  }

  invisible(nuts_updated)
}
