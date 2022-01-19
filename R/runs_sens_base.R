#' Run standard sensitivities for Pacific Hake
#'
#' The SRG requests that the following sensitivities are run as standard
#' protocol for the base model on an annual basis.
#' This function copies and alters the input files from a base model
#' setting up all of the required sensitivity runs.
#'
#' @param dirbase A directory where the base model is located. A full
#' path is preferred.
#'
#' @author Kelli F. Johnson
#' @return The function writes files to the disk and does not return
#' anything.
#'
runs_sens_base <- function(dirbase) {
  # xxxx.xx.15_h_prior_mean_low
  # xxxx.xx.16_h_fix_high
  # xxxx.xx.17_sigmaR_fix_low
  # xxxx.xx.18_sigmaR_fix_high
  # xxxx.xx.19_sigmaR_extra
  # xxxx.xx.20_M_0.2SD
  # xxxx.xx.21_M_0.3SD
  # xxxx.xx.22_M_hamel_prior
  # xxxx.xx.23_age1Survey
  # xxxx.xx.24_compWeight_HarmonicMean
  # xxxx.xx.27_tvSelect_phi_extralow
  # xxxx.xx.28_tvSelect_phi_low
  # xxxx.xx.29_tvSelect_phi_high
  # xxxx.xx.43_maxSel_Age5
  # xxxx.xx.44_maxSel_Age7
  # xxxx.xx.45_maxSel_Age8

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

  setup_sensitivity <- function(number.addtobase = 15,
                                number.sens,
                                path.base = dirbase,
                                name.sens) {
    # Check input path and make it full
    path.N <- normalizePath(path.base)
    main <- dirname(path.N)
    number <- paste(collapse = ".",
      strsplit(strsplit(basename(path.N), "_")[[1]][1], "\\.")[[1]][1:2]
    )
    # Make the sensitivity path name
    pathnew <- file.path(
      main,
      paste0(number, ".", number.addtobase + number.sens, "_", name.sens)
    )
    # Copy inputs
    r4ss::copy_SS_inputs(
      dir.old = path.N,
      dir.new = pathnew,
      create.dir = TRUE,
      overwrite = TRUE,
      copy_exe = TRUE,
      verbose = FALSE
    )
    # Return the file path
    return(pathnew)
  }

  setup_ctl <- function(path.sens, return = c("ctl", "dat")) {
    return <- match.arg(return)
    file.starter <- file.path(path.sens, "starter.ss")
    starter <- r4ss::SS_readstarter(file.starter, verbose = FALSE)
    dat <- r4ss::SS_readdat(file.path(path.sens, starter[["datfile"]]),
      verbose = FALSE)
    if (return == "dat") return(dat)
    ctl <- r4ss::SS_readctl(file.path(path.sens, starter[["ctlfile"]]),
      use_datlist = TRUE, datlist = dat, verbose = FALSE,
      version = type.convert(dat[["ReadVersion"]], as.is = TRUE))
    return(ctl)
  }

  aa <- setup_sensitivity(number.sens = 0, name.sens = "h_prior_mean_low")
  ctl <- setup_ctl(aa)
  ctl[["SR_parms"]]["SR_BH_steep", "PRIOR"] <- 0.5
  r4ss::SS_writectl(ctl, ctl[["sourcefile"]], verbose = FALSE, overwrite = TRUE)

  aa <- setup_sensitivity(number.sens = 1, name.sens = "h_fix_high")
  ctl <- setup_ctl(aa)
  ctl[["SR_parms"]]["SR_BH_steep", "INIT"] <- 1
  ctl[["SR_parms"]]["SR_BH_steep", "HI"] <- 1
  ctl[["SR_parms"]]["SR_BH_steep", "PHASE"] <- -4
  r4ss::SS_writectl(ctl, ctl[["sourcefile"]], verbose = FALSE, overwrite = TRUE)

  # profile over sigmaR
  sigmavals <- c(1.0, 1.6)
  aa <- mapply(setup_sensitivity,
    name.sens = paste0("sigmR_fix_", c("low", "high")),
    number.sens = 2:3
    )
  for (ii in seq_along(aa)) {
    ctl <- setup_ctl(aa[ii])
    ctl[["SR_parms"]]["SR_sigmaR", "INIT"] <- sigmavals[ii]
    ctl[["SR_parms"]]["SR_sigmaR", "HI"] <- sigmavals[ii] * 1.5
    r4ss::SS_writectl(ctl, ctl[["sourcefile"]], verbose = FALSE, overwrite = TRUE)
  }

  aa <- setup_sensitivity(number.sens = 5, name.sens = "M_0.2SD")
  ctl <- setup_ctl(aa)
  ctl[["MG_parms"]]["NatM_uniform_Fem_GP_1", "PR_SD"] <- 0.2
  r4ss::SS_writectl(ctl, ctl[["sourcefile"]], verbose = FALSE, overwrite = TRUE)
  aa <- setup_sensitivity(number.sens = 6, name.sens = "M_0.3SD")
  ctl <- setup_ctl(aa)
  ctl[["MG_parms"]]["NatM_uniform_Fem_GP_1", "PR_SD"] <- 0.3
  r4ss::SS_writectl(ctl, ctl[["sourcefile"]], verbose = FALSE, overwrite = TRUE)
  aa <- setup_sensitivity(number.sens = 7, name.sens = "M_hamel_prior")
  ctl <- setup_ctl(aa)
  ctl[["MG_parms"]]["NatM_uniform_Fem_GP_1", c("PRIOR", "PR_SD")] <- c(-1.53245582, 0.4384383)
  r4ss::SS_writectl(ctl, ctl[["sourcefile"]], verbose = FALSE, overwrite = TRUE)

  aa <- setup_sensitivity(number.sens = 8, name.sens = "age1Survey")
  ctl <- setup_ctl(aa)
  dat <- setup_ctl(aa, return = "dat")
  dat[["Nfleets"]] <- 2
  dat[["fleetinfo"]] <- dat[["fleetinfo"]][1:dat[["Nfleets"]], ]
  dat[["CPUEinfo"]] <- dat[["CPUEinfo"]][1:dat[["Nfleets"]], ]
  dat[["Nsurveys"]] <- sum(dat[["fleetinfo"]][, "type"] == 3)
  dat[["fleetinfo1"]] <- dat[["fleetinfo1"]][, 1:dat[["Nfleets"]]]
  dat[["fleetinfo2"]] <- dat[["fleetinfo2"]][, 1:dat[["Nfleets"]]]
  dat[["CPUE"]] <- dat[["CPUE"]] %>%
    dplyr::filter(abs(index) %in% seq_along(dat[["fleetinfo"]][, 1])) %>%
    data.frame
  dat[["len_info"]] <- dat[["len_info"]][1:dat[["Nfleets"]], ]
  dat[["age_info"]] <- dat[["age_info"]][1:dat[["Nfleets"]], ]
  r4ss::SS_writedat(dat, dat[["sourcefile"]], verbose = FALSE, overwrite = TRUE)
  ctl[["Nfleets"]] <- dat[["Nfleets"]]
  ctl[["Q_options"]] <- ctl[["Q_options"]][1:dat[["Nsurveys"]], ]
  ctl[["Q_parms"]] <- ctl[["Q_parms"]][1:(dat[["Nsurveys"]] * 2), ]
  ctl[["size_selex_types"]] <- ctl[["size_selex_types"]][1:dat[["Nfleets"]], ]
  ctl[["age_selex_types"]] <- ctl[["age_selex_types"]][1:dat[["Nfleets"]], ]
  ctl[["size_selex_types"]] <- ctl[["size_selex_types"]][1:dat[["Nfleets"]], ]
  ctl[["age_selex_parms"]] <- ctl[["age_selex_parms"]][
    !grepl("Age1", row.names(ctl[["age_selex_parms"]])),
  ]
  ctl[["dirichlet_parms"]] <- ctl[["dirichlet_parms"]][1:dat[["Nfleets"]], ]
  wtfilename <- file.path(dirname(ctl[["sourcefile"]]), "wtatage.ss")
  wtatage <- r4ss::SS_readwtatage(wtfilename) %>%
    dplyr::filter(Fleet != 3)
  r4ss::SS_writewtatage(wtatage, dirname(wtfilename),
    overwrite = TRUE,
    verbose = FALSE,
    warn = FALSE
  )
  r4ss::SS_writectl(ctl, ctl[["sourcefile"]], verbose = FALSE, overwrite = TRUE)

  aa <- setup_sensitivity(
    number.sens = 9,
    name.sens = "compWeight_HarmonicMean"
  )
  ctl <- setup_ctl(aa)
  dat <- setup_ctl(aa, return = "dat")
  dat[["age_info"]][, "CompError"] <- 0
  dat[["age_info"]][, "ParmSelect"] <- 0
  ctl[["dirichlet_parms"]] <- NULL
  ctl[["Variance_adjustment_list"]] <- data.frame(
    Factor = c(5, 5),
    Fleet = seq(1:2),
    Value = c(0.14, 0.46)
  )
  ctl[["DoVar_adjust"]] <- 1
  r4ss::SS_writectl(ctl, ctl[["sourcefile"]], verbose = FALSE, overwrite = TRUE)
  r4ss::SS_writedat(dat, dat[["sourcefile"]], verbose = FALSE, overwrite = TRUE)

  aa <- setup_sensitivity(number.sens = 12, name.sens = "tvSelect_phi_extralow")
  ctl <- setup_ctl(aa)
  rows <- grep("dev_se", rownames(ctl[["age_selex_parms_tv"]]))
  ctl[["age_selex_parms_tv"]][rows, "INIT"] <- 0.21
  r4ss::SS_writectl(ctl, ctl[["sourcefile"]], verbose = FALSE, overwrite = TRUE)
  aa <- setup_sensitivity(number.sens = 13, name.sens = "tvSelect_phi_low")
  ctl <- setup_ctl(aa)
  rows <- grep("dev_se", rownames(ctl[["age_selex_parms_tv"]]))
  ctl[["age_selex_parms_tv"]][rows, "INIT"] <- 0.70
  r4ss::SS_writectl(ctl, ctl[["sourcefile"]], verbose = FALSE, overwrite = TRUE)
  aa <- setup_sensitivity(number.sens = 14, name.sens = "tvSelect_phi_high")
  ctl <- setup_ctl(aa)
  rows <- grep("dev_se", rownames(ctl[["age_selex_parms_tv"]]))
  ctl[["age_selex_parms_tv"]][rows, "INIT"] <- 2.10
  r4ss::SS_writectl(ctl, ctl[["sourcefile"]], verbose = FALSE, overwrite = TRUE)

  aa <- setup_sensitivity(number.sens = 28, name.sens = "maxSel_Age5")
  ctl <- setup_ctl(aa)
  rows <- grep("\\(2\\)", rownames(ctl[["age_selex_parms"]]))
  ctl[["age_selex_parms"]][rows[7:length(rows)], "PHASE"] <- -2
  r4ss::SS_writectl(ctl, ctl[["sourcefile"]], verbose = FALSE, overwrite = TRUE)
  aa <- setup_sensitivity(number.sens = 29, name.sens = "maxSel_Age7")
  ctl <- setup_ctl(aa)
  rows <- grep("\\(2\\)", rownames(ctl[["age_selex_parms"]]))
  ctl[["age_selex_parms"]][rows[6:8], "PHASE"] <- 2
  r4ss::SS_writectl(ctl, ctl[["sourcefile"]], verbose = FALSE, overwrite = TRUE)
  aa <- setup_sensitivity(number.sens = 30, name.sens = "maxSel_Age8")
  ctl <- setup_ctl(aa)
  rows <- grep("\\(2\\)", rownames(ctl[["age_selex_parms"]]))
  ctl[["age_selex_parms"]][rows[6:9], "PHASE"] <- 2
  r4ss::SS_writectl(ctl, ctl[["sourcefile"]], verbose = FALSE, overwrite = TRUE)

  # zero sum rec dev constraint
  aa <- setup_sensitivity(number.sens = 85, name.sens = "zerosumcontraint")
  ctl <- setup_ctl(aa)
  ctl[["do_recdev"]] <- 1
  r4ss::SS_writectl(ctl, ctl[["sourcefile"]], verbose = FALSE, overwrite = TRUE)

}
