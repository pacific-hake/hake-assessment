#' Overwrite [adnuts:::.sample_admb()] so that executables on the path can be used instead of a local dir executable only
.sample_admb <- function(model,
                         path = getwd(),
                         iter = 2000,
                         init = NULL,
                         chains = 3,
                         warmup = NULL,
                         seeds = NULL,
                         thin = 1,
                         mceval = FALSE,
                         duration = NULL,
                         parallel = FALSE,
                         cores = NULL,
                         control = NULL,
                         algorithm = "NUTS",
                         skip_optimization = TRUE,
                         skip_monitor = FALSE,
                         skip_unbounded = TRUE,
                         admb_args = NULL){
  if (is.null(cores))
    cores <- parallel::detectCores() - 1
  cores.max <- parallel::detectCores()
  if (cores > cores.max) {
    cores <- cores.max - 1
    warning(paste("Specified cores larger than available, using total-1=",
                  cores))
  }
  stopifnot(is.numeric(cores))
  if (!is.null(control) & !is.list(control))
    stop("control argument invalid, must be a list")
  if (cores < 1)
    stop(paste("Cores must be >=1, but is", cores))
  parallel <- ifelse(cores == 1 | chains == 1, FALSE, TRUE)
  if (parallel & cores < chains)
    message(paste("Recommend using chains < cores=", cores))
  stopifnot(thin >= 1)
  stopifnot(chains >= 1)
  if (is.null(seeds))
    seeds <- sample.int(1e+07, size = chains)
  if (iter < 1 | !is.numeric(iter))
    stop("iter must be > 1")
  stopifnot(is.character(path))
  stopifnot(is.character(model))
  if (!dir.exists(path))
    stop(paste("Folder", path, "does not exist. Check argument 'path'"))

  model <- get_model_executable(model, path)

  v <- .check_ADMB_version(model = model, path = path, warn = algorithm ==
                             "NUTS")
  if (v <= 12 & !skip_unbounded) {
    warning(paste("Version", v, "of ADMB is incompatible with skip_unbounded=FALSE, ignoring"))
    skip_unbounded <- TRUE
  }
  if (is.null(warmup))
    warmup <- floor(iter / 2)
  if (!(algorithm %in% c("NUTS", "RWM")))
    stop("Invalid algorithm specified")
  if (algorithm == "NUTS")
    control <- .update_control(control)
  if (is.null(init)) {
    warning("Using MLE inits for each chain -- strongly recommended to use dispersed inits")
  }
  else if (is.function(init)) {
    init <- lapply(1:chains, function(x) init())
  }
  else if (!is.list(init)) {
    stop("init must be NULL, a list, or a function")
  }
  if (!is.null(init) & length(init) != chains) {
    stop("Length of init does not equal number of chains.")
  }
  trash <- suppressWarnings(file.remove(list.files(path)[grep(".psv",
                                                              x = list.files())]))
  trash <- suppressWarnings(file.remove(file.path(path, "adaptation.csv"),
                                        file.path(path, "unbounded.csv")))
  if (!parallel) {
    if (algorithm == "NUTS") {
      mcmc.out <- lapply(1:chains, function(i) sample_admb_nuts(path = path,
                                                                model = model, warmup = warmup, duration = duration,
                                                                iter = iter, init = init[[i]], chain = i, seed = seeds[i],
                                                                thin = thin, control = control, admb_args = admb_args,
                                                                skip_optimization = skip_optimization))
    }
    else {
      mcmc.out <- lapply(1:chains, function(i) sample_admb_rwm(path = path,
                                                               model = model, warmup = warmup, duration = duration,
                                                               iter = iter, init = init[[i]], chain = i, seed = seeds[i],
                                                               thin = thin, control = control, skip_optimization = skip_optimization,
                                                               admb_args = admb_args))
    }
  }
  else {
    snowfall::sfStop()
    snowfall::sfInit(parallel = TRUE, cpus = cores)
    if (length(ls(envir = globalenv())) > 0)
      snowfall::sfExportAll()
    on.exit(snowfall::sfStop())
    mcmc.out <- snowfall::sfLapply(1:chains, function(i) sample_admb_parallel(parallel_number = i,
                                                                              path = path, model = model, duration = duration,
                                                                              algorithm = algorithm, iter = iter, init = init[[i]],
                                                                              warmup = warmup, seed = seeds[i], thin = thin, control = control,
                                                                              skip_optimization = skip_optimization, admb_args = admb_args))
  }
  warmup <- mcmc.out[[1]]$warmup
  mle <- .read_mle_fit(model = model, path = path)
  if (is.null(mle)) {
    par.names <- dimnames(mcmc.out[[1]]$samples)[[2]]
    par.names <- par.names[-length(par.names)]
  }
  else {
    par.names <- mle$par.names
  }
  iters <- unlist(lapply(mcmc.out, function(x) dim(x$samples)[1]))
  if (any(iters != iter/thin)) {
    N <- min(iters)
    warning(paste("Variable chain lengths, truncating to minimum=",
                  N))
  }
  else {
    N <- iter/thin
  }
  samples <- array(NA, dim = c(N, chains, 1 + length(par.names)),
                   dimnames = list(NULL, NULL, c(par.names, "lp__")))
  if (!skip_unbounded) {
    samples.unbounded <- samples
  }
  else {
    samples.unbounded = NULL
  }
  for (i in 1:chains) {
    samples[, i, ] <- mcmc.out[[i]]$samples[1:N, ]
    if (!skip_unbounded)
      samples.unbounded[, i, ] <- cbind(mcmc.out[[i]]$unbounded[1:N,
      ], mcmc.out[[i]]$samples[, 1 + length(par.names)])
  }
  if (algorithm == "NUTS")
    sampler_params <- lapply(mcmc.out, function(x) x$sampler_params[1:N,
    ])
  else sampler_params <- NULL
  time.warmup <- unlist(lapply(mcmc.out, function(x) as.numeric(x$time.warmup)))
  time.total <- unlist(lapply(mcmc.out, function(x) as.numeric(x$time.total)))
  cmd <- unlist(lapply(mcmc.out, function(x) x$cmd))
  if (N < warmup)
    warning("Duration too short to finish warmup period")
  message(paste("... Merging post-warmup chains into main folder:",
                path))
  samples2 <- do.call(rbind, lapply(1:chains, function(i) samples[-(1:warmup),
                                                                  i, -dim(samples)[3]]))
  .write_psv(fn = model, samples = samples2, model.path = path)
  unbounded <- do.call(rbind, lapply(mcmc.out, function(x) x$unbounded))
  oldwd <- getwd()
  on.exit(setwd(oldwd))
  setwd(path)
  write.table(unbounded, file = "unbounded.csv", sep = ",",
              col.names = FALSE, row.names = FALSE)
  if (mceval) {
    message("... Running -mceval on merged chains")
    system(paste(model, "-mceval -noest -nohess"), ignore.stdout = FALSE)
  }
  covar.est <- cov(unbounded)
  if (!skip_monitor) {
    if (!requireNamespace("rstan", quietly = TRUE))
      stop("Package 'rstan' is required to calculate diagnostics.\n Install it and try again, or set skip_monitor=FALSE.")
    message("Calculating ESS and Rhat (skip_monitor=TRUE will skip)...")
    mon <- rstan::monitor(samples, warmup, print = FALSE)
  }
  else {
    message("Skipping ESS and Rhat statistics..")
    mon <- NULL
  }
  result <- list(samples = samples, sampler_params = sampler_params,
                 samples_unbounded = samples.unbounded, time.warmup = time.warmup,
                 time.total = time.total, algorithm = algorithm, warmup = warmup,
                 model = model, max_treedepth = mcmc.out[[1]]$max_treedepth,
                 cmd = cmd, init = init, covar.est = covar.est, mle = mle,
                 monitor = mon)
  result <- adfit(result)
  return(invisible(result))
}