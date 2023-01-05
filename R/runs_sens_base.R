#' Run standard sensitivities for Pacific Hake
#'
#' The SRG requests that the following sensitivities are run as standard
#' protocol for the base model on an annual basis.
#' This function copies and alters the input files from a base model
#' setting up all of the required sensitivity runs.
#'
#' @param dir_version A file path to the current model version of interest. For
#'   example, `"models/2023/01-version"`. This directory will contain
#'   subdirectories that store folders for the base model and a separate folder
#'   for sensitivities.
#'
#' @author Kelli F. Johnson
#' @return A string is returned providing the path to the sensitivity directory.
#' Files are written to the disk inside the following directories:
#' * 01_h_prior_mean_low
#' * 02_h_fix_high
#' * 03_sigmaR_fix_low
#' * 04_sigmaR_fix_high
#' * _sigmaR_extra
#' * 05.20_M_0.2SD
#' * 06_M_0.3SD
#' * 07_M_hamel_prior
#' * 08_age1Survey
#' * 09_compWeight_HarmonicMean
#' * 10_tvSelect_phi_extralow
#' * 11_tvSelect_phi_low
#' * 12_tvSelect_phi_high
#' * 13_maxSel_Age5
#' * 14_maxSel_Age7
#' * 15_maxSel_Age8
#' * 16_zerosumcontraint
#'
runs_sens_base <- function(dir_version) {
  main <- normalizePath(dir_version)
  dir_base <- fs::path(dir_version, "01-base-models", "01-base")
  file_exe <- fs::dir_ls(dir_base, regex = "exe$")
  if (length(file_exe) > 1 | length(file_exe) == 0) {
    message(
      glue::glue("{length(file_exe)} executable(s) were found in ", dir_base)
    )
  }
  input <- r4ss::SS_read(dir_base, verbose = FALSE)
  starter <- input[["starter"]]
  dat <- input[["dat"]]
  ctl <- input[["ctl"]]

  setup_sensitivity <- function(prefix_number,
                                path_version = dir_version,
                                suffix_string) {
    main <- fs::path(path_version, "03-sensitivity-models")
    # Make the sensitivity path name
    path_new <- fs::path(
      main,
      sprintf("%02d_%s", prefix_number, suffix_string)
    )
    # Copy inputs
    r4ss::copy_SS_inputs(
      dir.old = fs::path(dir_version, "01-base-models", "01-base"),
      dir.new = path_new,
      create.dir = TRUE,
      overwrite = TRUE,
      copy_exe = FALSE,
      verbose = FALSE
    )
    # Return the file path
    return(path_new)
  }

  setup_ctl <- function(path_sens, return = c("ctl", "dat")) {
    return <- match.arg(return)
    input_list <- r4ss::SS_read(dir = path_sens)
    file.starter <- file.path(path_sens, "starter.ss")
    starter <- r4ss::SS_readstarter(file.starter, verbose = FALSE)
    if (return == "dat") return(input_list[["dat"]])
    return(input_list[["ctl"]])
  }

  aa <- setup_sensitivity(prefix_number = 1, suffix_string = "h_prior_mean_low")
  ctl <- setup_ctl(aa)
  ctl[["SR_parms"]]["SR_BH_steep", "PRIOR"] <- 0.5
  r4ss::SS_writectl(ctl, ctl[["sourcefile"]], verbose = FALSE, overwrite = TRUE)

  aa <- setup_sensitivity(prefix_number = 2, suffix_string = "h_fix_high")
  ctl <- setup_ctl(aa)
  ctl[["SR_parms"]]["SR_BH_steep", "INIT"] <- 1
  ctl[["SR_parms"]]["SR_BH_steep", "HI"] <- 1
  ctl[["SR_parms"]]["SR_BH_steep", "PHASE"] <- -4
  r4ss::SS_writectl(ctl, ctl[["sourcefile"]], verbose = FALSE, overwrite = TRUE)

  # profile over sigmaR
  sigmavals <- c(1.0, 1.6)
  aa <- mapply(setup_sensitivity,
    suffix_string = paste0("sigmR_fix_", c("low", "high")),
    prefix_number = 3:4
    )
  for (ii in seq_along(aa)) {
    ctl <- setup_ctl(aa[ii])
    ctl[["SR_parms"]]["SR_sigmaR", "INIT"] <- sigmavals[ii]
    ctl[["SR_parms"]]["SR_sigmaR", "HI"] <- sigmavals[ii] * 1.5
    r4ss::SS_writectl(ctl, ctl[["sourcefile"]], verbose = FALSE, overwrite = TRUE)
  }

  aa <- setup_sensitivity(prefix_number = 5, suffix_string = "M_0.2SD")
  ctl <- setup_ctl(aa)
  ctl[["MG_parms"]]["NatM_p_1_Fem_GP_1", "PR_SD"] <- 0.2
  r4ss::SS_writectl(ctl, ctl[["sourcefile"]], verbose = FALSE, overwrite = TRUE)
  aa <- setup_sensitivity(prefix_number = 6, suffix_string = "M_0.3SD")
  ctl <- setup_ctl(aa)
  ctl[["MG_parms"]]["NatM_p_1_Fem_GP_1", "PR_SD"] <- 0.3
  r4ss::SS_writectl(ctl, ctl[["sourcefile"]], verbose = FALSE, overwrite = TRUE)
  aa <- setup_sensitivity(prefix_number = 7, suffix_string = "M_hamel_prior")
  ctl <- setup_ctl(aa)
  ctl[["MG_parms"]]["NatM_p_1_Fem_GP_1", c("PRIOR", "PR_SD")] <- c(-1.53245582, 0.4384383)
  r4ss::SS_writectl(ctl, ctl[["sourcefile"]], verbose = FALSE, overwrite = TRUE)

  aa <- setup_sensitivity(prefix_number = 8, suffix_string = "age1Survey")
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
    verbose = FALSE
  )
  r4ss::SS_writectl(ctl, ctl[["sourcefile"]], verbose = FALSE, overwrite = TRUE)

  aa <- setup_sensitivity(
    prefix_number = 9,
    suffix_string = "compWeight_HarmonicMean"
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

  aa <- setup_sensitivity(prefix_number = 10, suffix_string = "tvSelect_phi_extralow")
  ctl <- setup_ctl(aa)
  rows <- grep("dev_se", rownames(ctl[["age_selex_parms_tv"]]))
  ctl[["age_selex_parms_tv"]][rows, "INIT"] <- 0.21
  r4ss::SS_writectl(ctl, ctl[["sourcefile"]], verbose = FALSE, overwrite = TRUE)
  aa <- setup_sensitivity(prefix_number = 11, suffix_string = "tvSelect_phi_low")
  ctl <- setup_ctl(aa)
  rows <- grep("dev_se", rownames(ctl[["age_selex_parms_tv"]]))
  ctl[["age_selex_parms_tv"]][rows, "INIT"] <- 0.70
  r4ss::SS_writectl(ctl, ctl[["sourcefile"]], verbose = FALSE, overwrite = TRUE)
  aa <- setup_sensitivity(prefix_number = 12, suffix_string = "tvSelect_phi_high")
  ctl <- setup_ctl(aa)
  rows <- grep("dev_se", rownames(ctl[["age_selex_parms_tv"]]))
  ctl[["age_selex_parms_tv"]][rows, "INIT"] <- 2.10
  r4ss::SS_writectl(ctl, ctl[["sourcefile"]], verbose = FALSE, overwrite = TRUE)

  aa <- setup_sensitivity(prefix_number = 13, suffix_string = "maxSel_Age5")
  ctl <- setup_ctl(aa)
  rows <- grep("\\(2\\)", rownames(ctl[["age_selex_parms"]]))
  ctl[["age_selex_parms"]][rows[7:length(rows)], "PHASE"] <- -2
  r4ss::SS_writectl(ctl, ctl[["sourcefile"]], verbose = FALSE, overwrite = TRUE)
  aa <- setup_sensitivity(prefix_number = 14, suffix_string = "maxSel_Age7")
  ctl <- setup_ctl(aa)
  rows <- grep("\\(2\\)", rownames(ctl[["age_selex_parms"]]))
  ctl[["age_selex_parms"]][rows[6:8], "PHASE"] <- 2
  r4ss::SS_writectl(ctl, ctl[["sourcefile"]], verbose = FALSE, overwrite = TRUE)
  aa <- setup_sensitivity(prefix_number = 15, suffix_string = "maxSel_Age8")
  ctl <- setup_ctl(aa)
  rows <- grep("\\(2\\)", rownames(ctl[["age_selex_parms"]]))
  ctl[["age_selex_parms"]][rows[6:9], "PHASE"] <- 2
  r4ss::SS_writectl(ctl, ctl[["sourcefile"]], verbose = FALSE, overwrite = TRUE)

  # zero sum rec dev constraint
  aa <- setup_sensitivity(prefix_number = 16, suffix_string = "zerosumcontraint")
  ctl <- setup_ctl(aa)
  ctl[["do_recdev"]] <- 1
  r4ss::SS_writectl(ctl, ctl[["sourcefile"]], verbose = FALSE, overwrite = TRUE)

}
