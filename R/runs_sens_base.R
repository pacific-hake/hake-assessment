#' Run standard sensitivities for Pacific Hake
#'
#' The SRG requests that the following sensitivities are run as standard
#' protocol for the base model on an annual basis.
#'
#' @param dirbase A directory where the base model is located. A full
#' path is preferred.
#' @param run A logical value specifying if the sensitivities should
#' be ran using the Stock Synthesis executable found in the folder.
#' If \code{FALSE}, then the folders will be created but the analyses
#' are not started. The default is to just copy the files.
#' @param niters An integer value specifying how many times to perform
#' the iterative process to weight the composition data when running
#' the McAllister-Ianelli and Francis weighting approaches. The default
#' is to run each four times.
#'
#' @author Kelli Faye Johnson
#' @return The function writes files to the disk and does not return
#' anything.
#'
runs_sens_base <- function(dirbase, run = FALSE, niters = 6) {
  # 2019.03.01_h_prior_mean_low
  # 2019.03.02_h_fix_high
  # 2019.03.03_sigmaR_fix_low
  # 2019.03.04_sigmaR_fix_high
  # 2019.03.05_M_0.2SD
  # 2019.03.06_M_0.3SD
  # 2019.03.07_age1Survey
  # 2019.03.08_compWeight_HarmonicMean
  # 2019.03.09_compWeight_Francis
 
  number <- paste(strsplit(strsplit(
    basename(dirbase), "_")[[1]][1], "\\.")[[1]][1:2],
    collapse = ".")
  number2 <- 15
  main <- dirname(normalizePath(dirbase))
  file.exe <- dir(dirbase, pattern = "exe$")
  if (length(file.exe) > 1) {
    stop("More than one executable was found in ", dirbase)
  }
  starter <- r4ss::SS_readstarter(file.path(dirbase, "starter.ss"),
    verbose = FALSE)
  dat <- r4ss::SS_readdat(file.path(dirbase, starter[["datfile"]]),
    verbose = FALSE)
  ctl <- r4ss::SS_readctl(file.path(dirbase, starter[["ctlfile"]]),
    use_datlist = TRUE, datlist = dat, verbose = FALSE,
    version = type.convert(dat[["ReadVersion"]], as.is = TRUE))
  # todo: remove hardwire to survey
  age1 <- c(
    "1995" = 231940,
    "1998" = 107000,
    "2003" = 24266.583,
    "2005" = 8651.674,
    "2007" = 1029090.399,
    "2009" = 3396343.372,
    "2011" = 5948669.130,
    "2012" = 64440.675,
    "2013" = 421579.674,
    "2015" = 4665070.061,
    "2017" = 1238038.108,
    "2019" = 733636.645)

  setup_sensitivity <- function(
    number.addtobase = 15,
    number.sens,
    path.base = dirbase,
    name.sens
    ) {
    # Check input path and make it full
    path.N <- normalizePath(path.base)
    main <- dirname(path.N)
    number <- paste(collapse = ".",
      strsplit(strsplit(basename(path.N), "_")[[1]][1], "\\.")[[1]][1:2]
      )
    # Make the sensitivity path name
    aa <- file.path(
      main,
      paste0(number, ".", number.addtobase + number.sens, "_", name.sens)
      )
    # Copy inputs
    r4ss::copy_SS_inputs(dir.old = path.N,
      dir.new = aa,
      create.dir = TRUE, overwrite = TRUE, recursive = FALSE,
      use_ss_new = FALSE, copy_exe = TRUE, copy_par = FALSE,
      verbose = FALSE)
    # Return the file path
    return(aa)
  }

  setup_ctl <- function(path.sens) {
    file.starter <- file.path(path.sens, "starter.ss")
    starter <- r4ss::SS_readstarter(file.starter, verbose = FALSE)
    dat <- r4ss::SS_readdat(file.path(path.sens, starter[["datfile"]]),
      verbose = FALSE)
    ctl <- r4ss::SS_readctl(file.path(path.sens, starter[["ctlfile"]]),
      use_datlist = TRUE, datlist = dat, verbose = FALSE,
      version = type.convert(dat[["ReadVersion"]], as.is = TRUE))
    return(ctl)
  }

  mymcmc <- function(data) {
    minyear <- min(as.numeric(gsub("SSB_", "",
      grep("SSB_[0-9]+", value = TRUE, colnames(data)))))
    data <- data %>%
      dplyr::select(dplyr::matches("InitAge|RecrDev|ForeRec")) %>%
      tidyr::gather(key = "rows", value = "value", tidyr::everything()) %>%
      dplyr::group_by(.data[["rows"]]) %>%
      dplyr::summarise(.groups = "keep",
        Value = mean(.data[["value"]]),
        Parm_StDev = stats::sd(.data[["value"]]),
      ) %>%
      dplyr::mutate(
        type = gsub("_[0-9]+", "", .data[["rows"]]),
        number = as.numeric(gsub("[a-zA-z_]+_", "", .data[["rows"]]))
      ) %>%
      dplyr::mutate(min = minyear - .data[["number"]]) %>%
      dplyr::mutate(
        Yr = dplyr::case_when(
          !grepl("InitAge", .data[["rows"]]) ~ .data[["number"]],
          TRUE ~ .data[["min"]])
      ) %>% tibble::column_to_rownames(var = "rows") %>%
      dplyr::select(.data[["Value"]]:.data[["type"]], .data[["Yr"]])
  }

  aa <- setup_sensitivity(number.sens = 0, name.sens = "h_prior_mean_low")
  ctl <- setup_ctl(aa)
  ctl[["SR_parms"]]["SR_BH_steep", "PRIOR"] <- 0.5
  r4ss::SS_writectl(ctl, ctl[["sourcefile"]], verbose = FALSE, overwrite = TRUE)

  aa <- setup_sensitivity(number.sens = 1, name.sens = "h_fix_high")
  ctl <- setup_ctl(aa)
  ctl[["SR_parms"]]["SR_BH_steep", "INIT"] <- 1 #todo think about it being on the bound
  ctl[["SR_parms"]]["SR_BH_steep", "HI"] <- 1
  ctl[["SR_parms"]]["SR_BH_steep", "PHASE"] <- -4
  r4ss::SS_writectl(ctl, ctl[["sourcefile"]], verbose = FALSE, overwrite = TRUE)

  aa <- setup_sensitivity(number.sens = 5, name.sens = "M_0.2SD")
  ctl <- setup_ctl(aa)
  ctl[["MG_parms"]]["NatM_p_1_Fem_GP_1", "PR_SD"] <- 0.2
  r4ss::SS_writectl(ctl, ctl[["sourcefile"]], verbose = FALSE, overwrite = TRUE)

  aa <- setup_sensitivity(number.sens = 6, name.sens = "M_0.3SD")
  ctl <- setup_ctl(aa)
  ctl[["MG_parms"]]["NatM_p_1_Fem_GP_1", "PR_SD"] <- 0.3
  r4ss::SS_writectl(ctl, ctl[["sourcefile"]], verbose = FALSE, overwrite = TRUE)

  aa <- setup_sensitivity(number.sens = 7, name.sens = "M_hamel_prior")
  ctl <- setup_ctl(aa)
  ctl[["MG_parms"]]["NatM_p_1_Fem_GP_1", "PR_SD"] <- 0.3
  r4ss::SS_writectl(ctl, ctl[["sourcefile"]], verbose = FALSE, overwrite = TRUE)

  aa <- setup_sensitivity(number.sens = 8, name.sens = "age1Survey")
  ctl <- setup_ctl(aa)
  filename.dat <- file.path(aa, starter[["datfile"]])
  dat <- r4ss::SS_readdat(filename.dat, verbose = FALSE)
  dat[["Nfleets"]] <- 3
  dat[["fleetnames"]][dat[["Nfleets"]]] <- "Age1Index"
  dat[["fleetinfo"]][dat[["Nfleets"]], ] <- c(3, 7, 1, 2, 0, "Age1Index")
  dat[["surveytiming"]][dat[["Nfleets"]]] <- 7
  dat[["units_of_catch"]][dat[["Nfleets"]]] <- 2
  dat[["CPUEinfo"]][dat[["Nfleets"]], ] <- c(3, 0, 0, 0)
  dat[["Nsurveys"]] <- sum(dat[["fleetinfo"]][,"type"] == 3)
  # todo: find what to do with these
  dat[["fleetinfo1"]]
  dat[["fleetinfo2"]]
  dat[["CPUE"]] <- rbind(dat[["CPUE"]],
    merge(
      data.frame("year" = as.numeric(names(age1)), "seas" = 7, "index" = 3,
      "obs" = age1, "se_log" = 0.5),
      data.frame("year" = (1995:dat[["endyr"]])[!1995:dat[["endyr"]] %in% names(age1)],
      "seas" = 7, "index" = -3, "obs" = 1, "se_log" = 1), all = TRUE))
  dat[["len_info"]][dat[["fleetnames"]][dat[["Nfleets"]]], ] <-
    c(-1, 0.001, 0, 0, 0, 0, 0.001)
  dat[["age_info"]][dat[["fleetnames"]][dat[["Nfleets"]]], ] <-
    c(-1, 0.001, 0, 0, 1, 2, 0.001)
  r4ss::SS_writedat(dat, filename.dat, verbose = FALSE, overwrite = TRUE)
  ctl[["Q_options"]][dat[["fleetnames"]][dat[["Nfleets"]]], ] <-
    c(3, 1, 0, 1, 0, 1)
  ctl[["Q_parms"]][paste0(
    "LnQ_base_",
    dat[["fleetnames"]][dat[["Nfleets"]]],
    "(", dat[["Nfleets"]], ")"), ] <-
    ctl[["Q_parms"]][grep("^LnQ_base_Ac", rownames(ctl[["Q_parms"]])), ]
  ctl[["Q_parms"]][paste0(
    "Q_extraSD_",
    dat[["fleetnames"]][dat[["Nfleets"]]],
    "(", dat[["Nfleets"]], ")"), ] <-
    ctl[["Q_parms"]][grep("^Q_extraSD_Ac", rownames(ctl[["Q_parms"]])), ]
  ctl[["size_selex_types"]][dat[["fleetnames"]][dat[["Nfleets"]]], ] <-
   rep(0, 4)
  ctl[["age_selex_types"]][dat[["fleetnames"]][dat[["Nfleets"]]], ] <-
   c(11, rep(0, 3))
  ctl[["dirichlet_parms"]][paste0(
    "ln(DM_theta)_", dat[["Nfleets"]])] <- ctl[["dirichlet_parms"]][1, ]
  r4ss::SS_writectl(ctl, ctl[["sourcefile"]], verbose = FALSE, overwrite = TRUE)

  aa <- mapply(setup_sensitivity,
    number.sens = 9:10,
    name.sens = c("compWeight_HarmonicMean", "compWeight_Francis"))
  parm_info <- mapply(FUN = SS_tune_comps,
     option = c("MI", "Francis"),
     dir = aa,
     MoreArgs = list(
     niters_tuning = ifelse(run, niters, 0), # 0 means the model will not be run.
     allow_up_tuning = TRUE,
     init_run = run,
     model = "ss",
     # extras = "-nohess",
     verbose = FALSE
     ), SIMPLIFY = FALSE)

  aa <- setup_sensitivity(number.sens = 11, name.sens = "semiPara_tvSelect_sig0695")
  ctl <- setup_ctl(aa)
  #todo Set this one up
  r4ss::SS_writectl(ctl, ctl[["sourcefile"]], verbose = FALSE, overwrite = TRUE)
  aa <- setup_sensitivity(number.sens = 12, name.sens = "tvSelect_phi_extralow")
  ctl <- setup_ctl(aa)
  #todo Set this one up
  r4ss::SS_writectl(ctl, ctl[["sourcefile"]], verbose = FALSE, overwrite = TRUE)
  aa <- setup_sensitivity(number.sens = 13, name.sens = "tvSelect_phi_low")
  ctl <- setup_ctl(aa)
  #todo Set this one up
  r4ss::SS_writectl(ctl, ctl[["sourcefile"]], verbose = FALSE, overwrite = TRUE)
  aa <- setup_sensitivity(number.sens = 14, name.sens = "tvSelect_phi_high")
  ctl <- setup_ctl(aa)
  #todo Set this one up
  r4ss::SS_writectl(ctl, ctl[["sourcefile"]], verbose = FALSE, overwrite = TRUE)
  aa <- setup_sensitivity(number.sens = 15, name.sens = "noCohort_ageError")
  ctl <- setup_ctl(aa)
  #todo Set this one up
  r4ss::SS_writectl(ctl, ctl[["sourcefile"]], verbose = FALSE, overwrite = TRUE)

  aa <- setup_sensitivity(number.sens = 16, name.sens = "M_Lorenzen")
  ctl <- setup_ctl(aa)
  ctl[["natM_type"]] <- 2
  ctl[["Lorenzen_refage"]] <- 4 ## Reference age for Lorenzen M
  r4ss::SS_writectl(ctl, ctl[["sourcefile"]], verbose = FALSE, overwrite = TRUE)

  aa <- setup_sensitivity(number.sens = 17, name.sens = "M_agespecific")
  ctl <- setup_ctl(aa)
  # todo: change to age-specific M
  r4ss::SS_writectl(ctl, ctl[["sourcefile"]], verbose = FALSE, overwrite = TRUE)
  aa <- setup_sensitivity(number.sens = 18, name.sens = "maxSel_Age5")
  ctl <- setup_ctl(aa)
  # todo: set up max age
  r4ss::SS_writectl(ctl, ctl[["sourcefile"]], verbose = FALSE, overwrite = TRUE)
  aa <- setup_sensitivity(number.sens = 18, name.sens = "maxSel_Age7")
  ctl <- setup_ctl(aa)
  # todo: set up max age
  r4ss::SS_writectl(ctl, ctl[["sourcefile"]], verbose = FALSE, overwrite = TRUE)
  aa <- setup_sensitivity(number.sens = 18, name.sens = "maxSel_Age8")
  ctl <- setup_ctl(aa)
  # todo: set up max age
  r4ss::SS_writectl(ctl, ctl[["sourcefile"]], verbose = FALSE, overwrite = TRUE)

  # Remove historical age data
  aa <- setup_sensitivity(number.sens = 26, name.sens = "removehistoricalcomps")
  filename.dat <- file.path(aa, starter[["datfile"]])
  dat <- r4ss::SS_readdat(filename.dat, verbose = FALSE)
  allyears <- dat[["agecomp"]][dat[["agecomp"]][, "FltSvy"] == 1, "Yr"]
  allyears <- allyears[allyears < type.convert(as.is = TRUE, format(Sys.Date(), "%Y")) - 30]
  for (ii in seq_along(allyears)) {
    bb <- file.path(aa, paste(basename(aa), allyears[ii], sep = "_"))
    r4ss::copy_SS_inputs(aa, bb, create.dir = TRUE, verbose = FALSE)
    file.copy(file.path(main, dirbase, "ss.exe"), file.path(bb, "ss.exe"))
    remove_data.comps(dir = bb, fleets = 1, removeyears = allyears[1:ii])
  }
  # Remove historical data
  aa <- setup_sensitivity(number.sens = 27, name.sens = "removehistoricaldata")
  filename.dat <- file.path(aa, starter[["datfile"]])
  dat <- r4ss::SS_readdat(filename.dat, verbose = FALSE)
  allyears <- dat[["agecomp"]][dat[["agecomp"]][, "FltSvy"] == 1, "Yr"]
  allyears <- allyears[allyears < type.convert(as.is = TRUE, format(Sys.Date(), "%Y")) - 30]
  for (ii in seq_along(allyears)) {
    bb <- file.path(aa, paste(basename(aa), allyears[ii], sep = "_"))
    r4ss::copy_SS_inputs(aa, bb, create.dir = TRUE, verbose = FALSE)
    file.copy(file.path(main, dirbase, "ss.exe"), file.path(bb, "ss.exe"))
    remove_data.comps(dir = bb, fleets = 1, removeyears = allyears[1:ii], removewtatage = TRUE)
  }
  for(ii in 2:15) run_adnuts(rev(dir(aa, pattern = "remove", full.names = TRUE))[ii])

  # profile over sigmaR
  sigmavals <- seq(0.5, 1.8, by = 0.1)
  aa <- mapply(setup_sensitivity,
    name.sens = sprintf("profile_sigmaR_%1.1f", sigmavals),
    MoreArgs = list(number.sens = 65)
    )
  for (ii in seq_along(aa)) {
    ctl <- setup_ctl(aa)
    ctl[["SR_parms"]]["SR_sigmaR", "INIT"] <- sigmavals[ii]
    ctl[["SR_parms"]]["SR_sigmaR", "HI"] <- sigmavals[ii] * 1.5
    r4ss::SS_writectl(ctl, ctl[["sourcefile"]], verbose = FALSE, overwrite = TRUE)
  }

  # run the model with no prior on sigmaR
  aa <- mapply(setup_sensitivity,
    name.sens = sprintf("sigmaR_%sprior", c("w", "wo")),
    number.sens = 66:67)
    )
  for (ii in seq_along(aa)) {
    ctl <- setup_ctl(aa)
    ctl[["SR_parms"]]["SR_sigmaR", "INIT"] <- 1.4
    ctl[["SR_parms"]]["SR_sigmaR", "PHASE"] <- abs(ctl[["SR_parms"]]["SR_sigmaR", "PHASE"])
    ctl[["SR_parms"]]["SR_sigmaR", "LO"] <- c(0.6, 0.0)[ii]
    ctl[["SR_parms"]]["SR_sigmaR", "HI"] <- c(1.8, 3.0)[ii]
    r4ss::SS_writectl(ctl, ctl[["sourcefile"]], verbose = FALSE, overwrite = TRUE)
  }

  # zero sum rec dev constraint
  aa <- setup_sensitivity(number.sens = 85, name.sens = "zerosumcontraint")
  ctl <- setup_ctl(aa)
  ctl[["do_recdev"]] <- 1
  r4ss::SS_writectl(ctl, ctl[["sourcefile"]], verbose = FALSE, overwrite = TRUE)


  mnames <- c(dirbase, dir(aa, pattern = "_removehistoricalcomps", include.dirs = TRUE, full.names = TRUE))
  compare <- lapply(mnames,
    r4ss::SS_output, verbose = FALSE, printstats = FALSE,
    covar = FALSE, dir.mcmc = "mcmc")
  test <- lapply(r4ss::SSsummarize(compare)$mcmc, mymcmc) %>% bind_rows(.id = "model")
  test$name <- gsub("[0-9]{4}\\.[0-9]{2}\\.[0-9]{2}_","",basename(mnames))[as.numeric(test$model)]
  ggplot(test, aes(Yr, Value, col =  factor(name))) + geom_point()
  r4ss::SSplotComparisons(r4ss::SSsummarize(compare), pdf = FALSE, plotdir = aa,
    legendlabels = gsub("[0-9]{4}\\.[0-9]{2}\\.[0-9]{2}_","",basename(mnames)),
    legendncol = 2, subplot = 11, mcmcVec = TRUE)

  aa <- setup_sensitivity(number.sens = 86, name.sens = "removetvselectivity")
  ctl <- setup_ctl(aa)
  ctl[["age_selex_parms"]][, c("dev_link", "dev_minyr", "dev_maxyr")] <- 0
  ctl[["age_selex_parms_tv"]] <- NULL
  ctl[["stddev_reporting_specs"]][1] <- 1
  r4ss::SS_writectl(ctl, ctl[["sourcefile"]], verbose = FALSE, overwrite = TRUE)
  compare2 <- r4ss::SS_output(aa, verbose = FALSE, printstats = FALSE, dir.mcmc = "mcmc")

  bb <- setup_sensitivity(number.sens = 87, name.sens = "removetvselectivity1",
    path.base = dir(
      pattern = "age1", 
      full.name = TRUE,
      dirname(normalizePath(dirbase))
      )
    )
  ctl <- setup_ctl(bb)
  ctl[["age_selex_parms"]][, c("dev_link", "dev_minyr", "dev_maxyr")] <- 0
  ctl[["age_selex_parms_tv"]] <- NULL
  ctl[["stddev_reporting_specs"]][1] <- 1
  r4ss::SS_writectl(ctl, ctl[["sourcefile"]], verbose = FALSE, overwrite = TRUE)
  compare3 <- r4ss::SS_output(bb, verbose = FALSE, printstats = FALSE, dir.mcmc = "mcmc")
  r4ss::SS_plots(compare3)

  compare <- r4ss::SS_output(dirbase, verbose = FALSE, printstats = FALSE, dir.mcmc = "mcmc")
  compare4 <- SSsummarize(list(compare, compare2, compare3), verbose = FALSE)
  r4ss::SSplotComparisons(compare4, pdf = FALSE, plotdir = aa,
  legendlabels = c("base", "Remove tv selectivity", "Remove tv selectivity Age 1"),
  legendncol = 2, subplot = 2, mcmcVec = TRUE, xlim = c(1990,2022))
}
