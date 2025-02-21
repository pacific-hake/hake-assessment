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
#' * 18-tv-maturity-182
#' * 19-m-at-age
#' * 20-m-at-age-fixed
#'
create_sens_dirs <- function(dir_version,
                             sens_dir_name = "03-sensitivity-models") {
  # Inputs and directories ----
  main <- normalizePath(dir_version)
  dir_base <- fs::path(dir_version, "01-base-models", "01-base")
  sens_dir <- fs::path(dir_version, sens_dir_name)
  if (dir.exists(sens_dir)) {
    fns <- list.files(
      sens_dir,
      all.files = TRUE,
      recursive = TRUE,
      no.. = TRUE,
      include.dirs = TRUE
    )
    if (length(fns) > 0) {
      cli::cli_abort(c(
        "x" = "Directory {sens_dir} exists and is populated.",
        "i" = "Delete these files or the whole directory and try again."
      ))
    }
  }
  fs::dir_create(sens_dir)

  input <- r4ss::SS_read(dir_base, verbose = FALSE)
  starter <- input[["starter"]]
  dat <- input[["dat"]]
  ctl <- input[["ctl"]]
  rm(input)

  # Helper functions ----
  # Set up the directory for a single sensitivity and copy inputs into it and
  # return the path name of the directory
  setup_sensitivity <- function(prefix_number,
                                path_version = dir_version,
                                suffix_string,
                                the_name = sens_dir_name) {
    path_new <- fs::path(
      fs::path(path_version, the_name),
      sprintf("%02d-%s", prefix_number, suffix_string)
    )
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

  # It is easier to use r4ss::SS_read() and read in the entire model than to use
  # r4ss::SS_readctl because it relies on the data file. This function reads in
  # the entire model, which takes a little bit longer when you only need the dat
  # file but that is okay, and then just returns the portions that you want.
  # The default is to return the control file.
  setup_ctl <- function(path_sens, return_section = c("ctl", "dat")) {
    return_section <- match.arg(return_section)
    input_list <- r4ss::SS_read(dir = path_sens)
    return(input_list[[return_section]])
  }

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
    ctl_list[["age_selex_parms"]] <- ctl_list[["age_selex_parms"]] |>
      dplyr::mutate(dev_link = ifelse(dev_minyr > 0, 2, 0))
    # Deal with tv parameters
    tv_names <- ctl_list[["age_selex_parms"]] |>
      dplyr::filter(dev_link > 0) |>
      tibble::rownames_to_column() |>
      dplyr::pull(rowname)
    ctl_list[["age_selex_parms_tv"]] <- ctl_list[["age_selex_parms_tv"]] |>
      tibble::rownames_to_column() |>
      tidyr::separate_wider_delim(
        rowname,
        names = c("fleet", "type"),
        delim = c(rowname = "_dev_")
      ) |>
      dplyr::arrange(type) |>
      dplyr::filter(fleet %in% tv_names) |>
      dplyr::group_by(type) |>
      tidyr::complete(fleet = tv_names) |>
      tidyr::fill(-type, -fleet) |>
      dplyr::ungroup() |>
      dplyr::arrange(fleet, dplyr::desc(type)) |>
      dplyr::mutate(
        rowname = paste(fleet, type, sep = "_dev_"),
        .before = type
      ) |>
      dplyr::select(-type, -fleet) |>
      tibble::column_to_rownames()
    return(ctl_list)
  }

  # Set up each sensitivity run by manipulating an object called `aa` and then
  # use r4ss::SS_write*() to write the manipulated portion back to the
  # appropriate folder.

  # Steepness ----
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

  # sigma_r ----
  sigmavals <- c(1.0, 1.6)
  aa <- purrr::map2(
    .x = 3:4,
    .y = paste0("sigma-r-fix-", c("low", "high")),
    .f = \(x, y) setup_sensitivity(prefix_number = x, suffix_string = y)
  )
  for (ii in seq_along(aa)) {
    ctl <- setup_ctl(aa[[ii]])
    ctl[["SR_parms"]]["SR_sigmaR", "LO"] <- sigmavals[ii] * 0.5
    ctl[["SR_parms"]]["SR_sigmaR", "INIT"] <- sigmavals[ii]
    ctl[["SR_parms"]]["SR_sigmaR", "HI"] <- sigmavals[ii] * 1.5
    r4ss::SS_writectl(
      ctl,
      ctl[["sourcefile"]],
      verbose = FALSE,
      overwrite = TRUE
    )
  }

  # Natural mortality ----
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
  ctl[["MG_parms"]]["NatM_p_1_Fem_GP_1", c("PRIOR", "PR_SD")] <- c(
    -1.53245582,
    0.31
  )
  r4ss::SS_writectl(ctl, ctl[["sourcefile"]], verbose = FALSE, overwrite = TRUE)

  # Age-one survey ----
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
  wtatage <- r4ss::SS_readwtatage(wtfilename) |>
    dplyr::filter(fleet <= 2)
  r4ss::SS_writewtatage(
    wtatage,
    dirname(wtfilename),
    overwrite = TRUE,
    verbose = FALSE
  )
  r4ss::SS_writectl(ctl, ctl[["sourcefile"]], verbose = FALSE, overwrite = TRUE)

  # Data weighting ----
  aa <- setup_sensitivity(
    prefix_number = 9,
    suffix_string = "comp-weight-harmonic-mean"
  )
  ctl <- setup_ctl(aa)
  dat <- setup_ctl(aa, return = "dat")
  dat[["age_info"]][, "CompError"] <- 0
  dat[["age_info"]][, "ParmSelect"] <- 0
  ctl[["dirichlet_parms"]] <- NULL
  # TODO: this one might need to change because there are three fleets
  ctl[["Variance_adjustment_list"]] <- data.frame(
    Factor = c(5, 5),
    Fleet = seq(1:2),
    Value = c(0.14, 0.46)
  )
  ctl[["DoVar_adjust"]] <- 1
  r4ss::SS_writectl(ctl, ctl[["sourcefile"]], verbose = FALSE, overwrite = TRUE)
  r4ss::SS_writedat(dat, dat[["sourcefile"]], verbose = FALSE, overwrite = TRUE)

  # Selectivity ----
  aa <- setup_sensitivity(
    prefix_number = 10,
    suffix_string = "tv-select-phi-extra-low"
  )
  ctl <- setup_ctl(aa)
  rows <- grep("dev_se", rownames(ctl[["age_selex_parms_tv"]]))
  ctl[["age_selex_parms_tv"]][rows, "INIT"] <- 0.21
  r4ss::SS_writectl(ctl, ctl[["sourcefile"]], verbose = FALSE, overwrite = TRUE)
  aa <- setup_sensitivity(
    prefix_number = 11,
    suffix_string = "tv-select-phi-low"
  )
  ctl <- setup_ctl(aa)
  rows <- grep("dev_se", rownames(ctl[["age_selex_parms_tv"]]))
  ctl[["age_selex_parms_tv"]][rows, "INIT"] <- 0.70
  r4ss::SS_writectl(ctl, ctl[["sourcefile"]], verbose = FALSE, overwrite = TRUE)
  aa <- setup_sensitivity(
    prefix_number = 12,
    suffix_string = "tv-select-phi-high"
  )
  ctl <- setup_ctl(aa)
  rows <- grep("dev_se", rownames(ctl[["age_selex_parms_tv"]]))
  ctl[["age_selex_parms_tv"]][rows, "INIT"] <- 2.10
  r4ss::SS_writectl(ctl, ctl[["sourcefile"]], verbose = FALSE, overwrite = TRUE)

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

  # Maturity ----
  # Time-varying maturity in the middle of the year
  aa <- setup_sensitivity(prefix_number = 18, suffix_string = "tv-maturity-182")
  write_wtatage_file(
    file = fs::path(aa, "wtatage.ss"),
    data = pad_weight_at_age(weight_at_age_estimates_df),
    maturity = maturity_estimates_df |>
      dplyr::filter(doy == 182) |>
      dplyr::summarize(p_mature = mean(p_mature), .by = c(age)) |>
      dplyr::pull(p_mature),
    n_fleets = dat[["Nfleets"]]
  )
  # Read that weight-at-age data back in and update it with the
  # time-varying maturity and write back out
  weight_at_age <- r4ss::SS_readwtatage(fs::path(aa, "wtatage.ss"))
  inputs <- r4ss::SS_read(aa)
  inputs[["wtatage"]] <- update_ss3_maturity(
    maturity = maturity_estimates_df |>
      dplyr::filter(
        model == "Spatial + temperature",
        !is.na(p_mature),
        doy == 182
      ),
    weight_at_age = weight_at_age
  )
  fishery_age_composition <- calc_fishery_ages(
    weight_at_age = inputs[["wtatage"]]
  )
  colnames(fishery_age_composition) <- colnames(inputs[["dat"]][["agecomp"]])
  inputs[["dat"]][["agecomp"]] <- dplyr::bind_rows(
    dplyr::filter(
      inputs[["dat"]][["agecomp"]],
      !(fleet == 1 & year >= 2008)
    ),
    fishery_age_composition |>
      dplyr::filter(year >= 2008)
  ) |>
    dplyr::arrange(-fleet, year)
  inputs[["par"]] <- NULL
  r4ss::SS_write(
    inputlist = inputs,
    dir = aa,
    overwrite = TRUE,
    verbose = FALSE
  )
  # Chris wrote this function and I am not sure what it does but the files
  # will not run without doing it
  purrr::walk(aa, .f = \(x) system(glue::glue("cd {x} && clean_ss3")))

  # Mortality ----
  aa <- setup_sensitivity(prefix_number = 19, suffix_string = "m-at-age")
  inputs <- r4ss::SS_read(aa)
  inputs[["ctl"]] <- change_mortality_ctl(inputs[["ctl"]])
  inputs[["dat"]] <- change_mortality_dat(
    inputs[["dat"]],
    data = m_at_age_df,
    years = inputs[["dat"]][["styr"]]:inputs[["dat"]][["endyr"]]
  )
  r4ss::SS_write(
    inputs,
    dir = aa,
    overwrite = TRUE,
    verbose = FALSE
  )
  purrr::walk(aa, .f = \(x) system(glue::glue("cd {x} && clean_ss3")))
  aa <- setup_sensitivity(prefix_number = 20, suffix_string = "m-at-age-fixed")
  inputs <- r4ss::SS_read(aa)
  inputs[["ctl"]] <- change_mortality_ctl(inputs[["ctl"]], estimate = FALSE)
  inputs[["dat"]] <- change_mortality_dat(
    inputs[["dat"]],
    data = m_at_age_df,
    years = inputs[["dat"]][["styr"]]:inputs[["dat"]][["endyr"]]
  )
  r4ss::SS_write(
    inputs,
    dir = aa,
    overwrite = TRUE,
    verbose = FALSE
  )
  purrr::walk(aa, .f = \(x) system(glue::glue("cd {x} && clean_ss3")))

  cli::cli_alert(c(
    "i" = "Created new sensitivity directories in {sens_dir}"
  ))
}
