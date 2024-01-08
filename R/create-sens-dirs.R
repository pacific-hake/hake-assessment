#' Create standard sensitivity directories
#'
#' The SRG requests that the following sensitivities are run as standard
#' protocol for the base model on an annual basis.
#' This function copies and alters the input files from a base model
#' setting up all of the required sensitivity runs.
#'
#' @param dir_version A file path to the current model version of interest. For
#'   example, `/srv/hake/models/2023/01-version`. This directory will contain
#'   subdirectories that store folders for the base model and a separate folder
#'   for sensitivities.
#' @param sens_dir_name Name of the sensitivity directory found in the `dir_version`
#' directory
#'
#' @author Kelli F. Johnson
#' @return A string is returned providing the path to the sensitivity directory.
#' Files are written to the disk inside the following directories:
#' * 01-h-prior-mean-low
#' * 02-h-fix-high
#' * 03-sigma-r-fix-low
#' * 04-sigma-r-fix-high
#' * 05-m-02-sd
#' * 06-m-03-sd
#' * 07-m-hamel-prior
#' * 08-age-1-survey,
#' * 09-comp-weight-harmonic-mean
#' * 10-tv-select-phi-extra-low
#' * 11-tv-select-phi-low
#' * 12-tv-select-phi-high
#' * 13-max-sel-age-'5
#' * 14-max-sel-age-7
#' * 15-max-sel-age-8
#' * 16-zero-sum-constraint
#'
create_sens_dirs <- function(dir_version,
                             sens_dir_name = "03-sensitivity-models") {
  main <- normalizePath(dir_version)
  dir_base <- fs::path(dir_version, "01-base-models", "01-base")
  sens_dir <- fs::path(dir_version, sens_dir_name)
  if(dir.exists(sens_dir)){
    fns <- list.files(sens_dir, all.files = TRUE, recursive = TRUE, no.. = TRUE, include.dirs = TRUE)
    if(length(fns)){
      stop("\nDirectory `", sens_dir, "` exists and contains files and/or directories. ",
           "Delete these or the whole directory and try again\n")
    }
  }
  dir.create(sens_dir)

  file_exe <- fs::dir_ls(dir_base, regex = "exe$")
  if (length(file_exe) > 1 | length(file_exe) == 0) {
    message(
      glue("{length(file_exe)} executable(s) were found in ", dir_base)
    )
  }
  input <- r4ss::SS_read(dir_base, verbose = FALSE)
  starter <- input[["starter"]]
  dat <- input[["dat"]]
  ctl <- input[["ctl"]]

  setup_sensitivity <- function(prefix_number,
                                path_version = dir_version,
                                suffix_string) {
    main <- fs::path(path_version, sens_dir_name)
    # Make the sensitivity path name
    path_new <- fs::path(
      main,
      sprintf("%02d-%s", prefix_number, suffix_string)
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
    path_new
  }

  setup_ctl <- function(path_sens, return = c("ctl", "dat")) {
    return <- match.arg(return)
    input_list <- r4ss::SS_read(dir = path_sens)
    file.starter <- file.path(path_sens, "starter.ss")
    starter <- r4ss::SS_readstarter(file.starter, verbose = FALSE)
    if(return == "dat"){
      input_list[["dat"]]
    }else{
      input_list[["ctl"]]
    }
  }

  aa <- setup_sensitivity(prefix_number = 1, suffix_string = "h-prior-mean-low")
  ctl <- setup_ctl(aa)
  ctl[["SR_parms"]]["SR_BH_steep", "PRIOR"] <- 0.5
  r4ss::SS_writectl(ctl, ctl[["sourcefile"]], verbose = FALSE, overwrite = TRUE)

  aa <- setup_sensitivity(prefix_number = 2, suffix_string = "h-fix-high")
  ctl <- setup_ctl(aa)
  ctl[["SR_parms"]]["SR_BH_steep", "INIT"] <- 1
  ctl[["SR_parms"]]["SR_BH_steep", "HI"] <- 1
  ctl[["SR_parms"]]["SR_BH_steep", "PHASE"] <- -4
  r4ss::SS_writectl(ctl, ctl[["sourcefile"]], verbose = FALSE, overwrite = TRUE)

  # profile over sigmaR
  sigmavals <- c(1.0, 1.6)
  aa <- mapply(setup_sensitivity,
    suffix_string = paste0("sigma-r-fix-", c("low", "high")),
    prefix_number = 3:4
    )
  for (ii in seq_along(aa)) {
    ctl <- setup_ctl(aa[ii])
    ctl[["SR_parms"]]["SR_sigmaR", "INIT"] <- sigmavals[ii]
    ctl[["SR_parms"]]["SR_sigmaR", "HI"] <- sigmavals[ii] * 1.5
    r4ss::SS_writectl(ctl, ctl[["sourcefile"]], verbose = FALSE, overwrite = TRUE)
  }

  aa <- setup_sensitivity(prefix_number = 5, suffix_string = "m-02-sd")
  ctl <- setup_ctl(aa)
  ctl[["MG_parms"]]["NatM_p_1_Fem_GP_1", "PR_SD"] <- 0.2
  r4ss::SS_writectl(ctl, ctl[["sourcefile"]], verbose = FALSE, overwrite = TRUE)
  aa <- setup_sensitivity(prefix_number = 6, suffix_string = "m-03-sd")
  ctl <- setup_ctl(aa)
  ctl[["MG_parms"]]["NatM_p_1_Fem_GP_1", "PR_SD"] <- 0.3
  r4ss::SS_writectl(ctl, ctl[["sourcefile"]], verbose = FALSE, overwrite = TRUE)
  aa <- setup_sensitivity(prefix_number = 7, suffix_string = "m-hamel-prior")
  ctl <- setup_ctl(aa)
  ctl[["MG_parms"]]["NatM_p_1_Fem_GP_1", c("PRIOR", "PR_SD")] <- c(-1.53245582, 0.4384383)
  r4ss::SS_writectl(ctl, ctl[["sourcefile"]], verbose = FALSE, overwrite = TRUE)

  aa <- setup_sensitivity(prefix_number = 8, suffix_string = "age-1-survey")
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
    suffix_string = "comp-weight-harmonic-mean"
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

  aa <- setup_sensitivity(prefix_number = 10, suffix_string = "tv-select-phi-extra-low")
  ctl <- setup_ctl(aa)
  rows <- grep("dev_se", rownames(ctl[["age_selex_parms_tv"]]))
  ctl[["age_selex_parms_tv"]][rows, "INIT"] <- 0.21
  r4ss::SS_writectl(ctl, ctl[["sourcefile"]], verbose = FALSE, overwrite = TRUE)
  aa <- setup_sensitivity(prefix_number = 11, suffix_string = "tv-select-phi-low")
  ctl <- setup_ctl(aa)
  rows <- grep("dev_se", rownames(ctl[["age_selex_parms_tv"]]))
  ctl[["age_selex_parms_tv"]][rows, "INIT"] <- 0.70
  r4ss::SS_writectl(ctl, ctl[["sourcefile"]], verbose = FALSE, overwrite = TRUE)
  aa <- setup_sensitivity(prefix_number = 12, suffix_string = "tv-select-phi-high")
  ctl <- setup_ctl(aa)
  rows <- grep("dev_se", rownames(ctl[["age_selex_parms_tv"]]))
  ctl[["age_selex_parms_tv"]][rows, "INIT"] <- 2.10
  r4ss::SS_writectl(ctl, ctl[["sourcefile"]], verbose = FALSE, overwrite = TRUE)

  change_max_age_selectivity <- function(ctl_list, age_max, fishery = 1) {
    max_parameter_int <- ctl_list[["age_selex_parms"]] |>
      tibble::rownames_to_column(var = "name") |>
      dplyr::filter(grepl(paste0("\\(", fishery, "\\)"), name)) |>
      dplyr::pull(name) |>
      strsplit(split = "_") |>
      purrr::map(3) |>
      as.numeric() |>
      max()
    min_parameter_int <- ctl_list[["age_selex_parms"]] |>
      tibble::rownames_to_column(var = "name") |>
      dplyr::filter(grepl(paste0("\\(", fishery, "\\)"), name)) |>
      dplyr::filter(PHASE >= 0) |>
      dplyr::pull(name) |>
      strsplit(split = "_") |>
      purrr::map(3) |>
      as.numeric() |>
      min()
    # Turn off parameters
    regex_string <- purrr::map(
      list(
        c(age_max:max_parameter_int + 2),
        c(min_parameter_int:(age_max + 1))
      ),
      .f = \(x) paste0(
        "_P_", x,
        "_.+\\(", fishery, "\\)$",
        collapse = "|"
      )
    )
    cols_zero <- c("INIT", "dev_minyr", "dev_maxyr", "dev_PH")
    rows <- grep(regex_string[[1]], rownames(ctl_list[["age_selex_parms"]]))
    ctl_list[["age_selex_parms"]][rows, cols_zero] <- 0
    ctl_list[["age_selex_parms"]][rows, "PHASE"] <- -2
    rows <- grep(regex_string[[2]], rownames(ctl_list[["age_selex_parms"]]))
    ctl_list[["age_selex_parms"]][rows, "PHASE"] <- 2
    # This only works with selectivity #17, i.e., not generalized for all species
    if (any(ctl_list[["age_selex_parms"]][rows, "dev_PH"] > 0)) {
      ctl_list[["age_selex_parms"]][rows, "dev_PH"] <- max(
        ctl_list[["age_selex_parms"]][rows, "dev_PH"]
      )
      ctl_list[["age_selex_parms"]][rows, "dev_minyr"] <- max(
        ctl_list[["age_selex_parms"]][rows, "dev_minyr"]
      )
      ctl_list[["age_selex_parms"]][rows, "dev_maxyr"] <- max(
        ctl_list[["age_selex_parms"]][rows, "dev_maxyr"]
      )
    }
    # Turn on parameters
    return(ctl_list)
  }
  aa <- setup_sensitivity(prefix_number = 13, suffix_string = "max-sel-age-5")
  ctl <- setup_ctl(aa) |>
    change_max_age_selectivity(age_max = 5)
  r4ss::SS_writectl(ctl, ctl[["sourcefile"]], verbose = FALSE, overwrite = TRUE)
  aa <- setup_sensitivity(prefix_number = 14, suffix_string = "max-sel-age-7")
  ctl <- setup_ctl(aa) |>
    change_max_age_selectivity(age_max = 7)
  r4ss::SS_writectl(ctl, ctl[["sourcefile"]], verbose = FALSE, overwrite = TRUE)
  aa <- setup_sensitivity(prefix_number = 15, suffix_string = "max-sel-age-8")
  ctl <- setup_ctl(aa) |>
    change_max_age_selectivity(age_max = 8)
  r4ss::SS_writectl(ctl, ctl[["sourcefile"]], verbose = FALSE, overwrite = TRUE)

  # zero sum rec dev constraint
  aa <- setup_sensitivity(prefix_number = 16, suffix_string = "zero-sum-constraint")
  ctl <- setup_ctl(aa)
  ctl[["do_recdev"]] <- 1
  r4ss::SS_writectl(ctl, ctl[["sourcefile"]], verbose = FALSE, overwrite = TRUE)

  message("Created new sensitivity directories in ",
          sens_dir)
}
