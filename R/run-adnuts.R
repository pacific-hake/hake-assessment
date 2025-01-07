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
#' @param adapt_delta The target acceptance rate. See [adnuts::sample_admb()]
#' @param run_extra_mcmc If `TRUE`, run SS extra mcmc option which outputs
#' files into the `sso` subdirectory. If `FALSE`, those files will not be
#' created and the `posteriors.sso` and `dervied_posteriors.sso` files
#' will be in the running directory
#' @param fn_exe The name of the executable which was built using ADMB
#' @param overwrite Logical. If `TRUE` and in an interactive session,
#' don't ask user if they want to overwrite if the directory already exists,
#' just do it. If non-interactive, the output will be overwritten and this
#' will be ignored
#' @param input_files The input files for SS
#' @param hess_step Logical. If `TRUE`, use the `hess_step` algorithm`
#' @param fn_logfile The filename of the logfile
#'
#' @return Nothing
#' @export
run_adnuts <- function(path,
                       num_chains = NULL,
                       seed = 42,
                       num_samples = 8000,
                       num_warmup_samples = 250,
                       adapt_delta = 0.95,
                       run_extra_mcmc = FALSE,
                       hess_step = TRUE,
                       fn_exe = ss_executable,
                       overwrite = TRUE,
                       fn_logfile = model_output_log_fn,
                       input_files = ss_input_files){

  # Determine if the caller is calling from an Rstudio session
  is_rstudio <- Sys.getenv("RSTUDIO") == "1"

  # Get the actual full path of the executable on whatever machine you're on
  fn_exe <- get_model_executable(fn_exe)

  # Chains to run in parallel
  num_machine_cores  <-availableCores()
  if(is.null(num_chains)){
    num_chains <- num_machine_cores - 1
  }
  if(num_machine_cores <= num_chains){
    msg <- paste0("The number of available cores (", num_machine_cores, ") ",
                  "is less than or equal to the number of chains ",
                  "requested (", num_chains, ")")
    if(is_rstudio){
      message(red(symbol$cross, msg))
      stop_quietly()
    }else{
      stop(msg)
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
        stop_quietly()
      }else{
        stop(msg)
      }
    }
  }
  dir.create(mcmc_path, showWarnings = FALSE)
  file_chmod(mcmc_path, output_permissions)
  if(hess_step){
    input_files <- c(input_files, "admodel.cov", "admodel.hes", "ss.bar")
  }

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

  # Run MLE and optimization MCMC (path) ----
  msg <- paste0("Running optimization MCMC (chain length 15) ",
                "to ensure hessian is good ",
                "and optimize without bias adjustment turned on\n")
  if(is_rstudio){
    message(green(symbol$info, msg))
  }else{
    message(msg)
  }
  # Run MLE to create .ss_new files
  cmd <- paste0("cd ", path, " && ", fn_exe)
  if(!is.null(fn_logfile)){
    cmd <- paste0(cmd, " > ", fn_logfile, " 2>&1")
  }
  ss_new_run <- system_(cmd, intern = TRUE, wait = TRUE)
  if(!is.null(attributes(ss_new_run)) && attr(ss_new_run, "status")){
    msg <- paste0("System command returned an error (status 1):\n", cmd)
    if(is_rstudio){
      message(red(symbol$cross, msg))
      stop_quietly()
    }else{
      stop(msg)
    }
  }

  cmd <- paste0("cd ", path, " && ", fn_exe,  " -nox -iprint 200 -mcmc 15")
  if(!is.null(fn_logfile)){
    cmd <- paste0(cmd, " > ", fn_logfile, " 2>&1")
  }
  initial_run <- system_(cmd, intern = TRUE, wait = TRUE)
  if(!is.null(attributes(initial_run)) && attr(initial_run, "status")){
    msg <- paste0("System command returned an error (status 1):\n", cmd)
    if(is_rstudio){
      message(red(symbol$cross, msg))
      stop_quietly()
    }else{
      stop(msg)
    }
  }

  # Run initial MCMC (mcmc_path) ----
  input_files <- file.path(path, input_files)
  bar_file_name <- grep("bar", input_files, value = TRUE)
  if (!fs::file_exists(bar_file_name)) {
    good_bar_file_name <- fs::dir_ls(path, regexp = "\\.bar")
    stopifnot(length(good_bar_file_name) == 1)
    input_files[which(input_files == bar_file_name)] <- good_bar_file_name
  }
  file.copy(input_files, mcmc_path, overwrite = TRUE)
  if(run_extra_mcmc){
    dir.create(file.path(mcmc_path, "sso"), showWarnings = TRUE)
    file_chmod(file.path(mcmc_path, "sso"), output_permissions)
    modify_starter_mcmc_type(mcmc_path, 2)
  }else{
    modify_starter_mcmc_type(mcmc_path, 1)
  }
  rdata_file <- file.path(mcmc_path, "hake.Rdata")
  # Run MLE to create .ss_new files
  cmd <- paste0("cd ", mcmc_path, " && ", fn_exe)
  if(!is.null(fn_logfile)){
    cmd <- paste0(cmd, " > ", fn_logfile, " 2>&1")
  }
  ss_new_run <- system_(cmd, intern = TRUE, wait = TRUE)
  if(!is.null(attributes(ss_new_run)) && attr(ss_new_run, "status")){
    msg <- paste0("System command returned an error (status 1):\n", cmd)
    if(is_rstudio){
      message(red(symbol$cross, msg))
      stop_quietly()
    }else{
      stop(msg)
    }
  }
  # Copy .ss_new files to .ss files in the mcmc directory
  copy_SS_inputs(dir.old = mcmc_path,
                 dir.new = mcmc_path,
                 use_ss_new = TRUE,
                 overwrite = TRUE,
                 verbose = FALSE)

  # The -hbf 1 argument is a technical requirement because NUTS uses a
  # different set of bounding functions and thus the mass matrix will be
  # different
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
    bar_name <- basename(fs::dir_ls(mcmc_path, regexp = "\\.bar"))
    cmd <- paste0("cd ", mcmc_path, " && ", fn_exe,
                  " -hbf 1 -nox -iprint 200 -mcmc 15 -hess_step 10 ",
                  "-binp ", bar_name)
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

  # Run again for inference using updated mass matrix.
  # Increase adapt_delta toward 1 if you have divergences (runs will take
  # longer). Note this is in unbounded parameter space
  mass <- nuts_initial$covar_est
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

  # Find name of par file and match psv file to it
  good_psv_name <- gsub(
    "\\.par",
    ".psv",
    basename(fs::dir_ls(mcmc_path, regexp = "\\.par"))
  )
  fs::file_copy(
    fs::dir_ls(path = mcmc_path, regexp = "\\.psv"),
    fs::path(mcmc_path, good_psv_name)
  )
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
