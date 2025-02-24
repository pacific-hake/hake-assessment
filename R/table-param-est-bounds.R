#' Creates a table summarizing lead parameters
#'
#' @rdname table_biomass
#' @param start_rec_dev_yr First year of estimated recruitment devs
#' @param end_rec_dev_yr Last year of estimated recruitment devs
#' @param section_sep_lines Logical. If `TRUE`, place a line under the
#' sections in each cell as a way to separate them vertically from the
#' years above and below
#' @param ret_df If `TRUE`, return a data frame of the results, if `FALSE`,
#' return a [kableExtra::kbl()] table
#'
#' @return Either a [tibble::tibble()] or [kableExtra::kbl()] object
#' @export
table_param_est_bounds <- function(model,
                                   start_rec_dev_yr,
                                   end_rec_dev_yr,
                                   section_sep_lines = FALSE,
                                   digits = 2,
                                   font_size = 8,
                                   header_font_size = 10,
                                   header_vert_spacing = 12,
                                   header_vert_scale = 1.2,
                                   ret_df = FALSE,
                                   ...){

  ctl <- model$ctl
  if(is.null(ctl)){
    stop("No control file information fopund in the `model` list")
  }
  if(!"SR_parms" %in% names(ctl)){
    stop("`SR_parms` not found in the control file list (`model$ctl`)")
  }

  sr_params <- convert_ctl_file_param_dfs(ctl, "SR_parms") |>
    mutate(param = gsub("sr_", "", param)) |>
    mutate(param = ifelse(grepl("ln", param), "ro", param)) |>
    mutate(param = ifelse(grepl("steep", param), "h", param)) |>
    mutate(param = ifelse(grepl("sigmar", param), "sigma_r", param)) |>
    dplyr::filter(!param %in% c("autocorr", "regime"))

  # R0 ----
  row <- sr_params[sr_params$param == "ro", ]
  row$latex_nm <- paste0("Log (",
                         latex_subscr(latex_italics("R"), "0"),
                         ")")
  row$num_param <- "1"
  row$bounds <- paste0("(", row$lo, ", ", row$hi, ")")
  row$prior_str <- get_prior_string(row, digits)
  sr_params[sr_params$param == "ro", ] <- row

  # Steepness (h) ----
  row <- sr_params[sr_params$param == "h", ]
  row$latex_nm <- paste0("Steepness (",
                         latex_italics("h"),
                         ")")
  row$num_param <- "1"
  row$bounds <- paste0("(", row$lo, ", ", row$hi, ")")
  row$prior_str <- get_prior_string(row, digits)
  sr_params[sr_params$param == "h", ] <- row

  # Recruitment variability (sigma_r) ----
  row <- sr_params[sr_params$param == "sigma_r", ]
  row$latex_nm <- paste0("Recruitment variability (",
                         latex_italics("$\\sigma_r$"),
                         ")")
  if(row$type < 0 && row$phase > 0){
    row$num_param <- "1"
    row$bounds <- paste0("(", row$lo, ", ", row$hi, ")")
  }else{
    row$num_param <- "--"
    row$bounds <- "--"
  }
  row$prior_str <- get_prior_string(row, digits)
  sr_params[sr_params$param == "sigma_r", ] <- row

  # Recruitment deviations ----
  # The number of them comes from the arguments to this function (for now)
  row <- tibble(param = "recdev",
                hi = as.character(ctl$max_rec_dev),
                lo = as.character(ctl$min_rec_dev),
                type = "3",
                phase = as.character(ctl$recdev_phase),
                init = "0",
                mean = "0",
                sd = "$\\sigma_r$",
                latex_nm = paste0("Log recruitment deviations: ",
                                  start_rec_dev_yr, "--", end_rec_dev_yr),
                num_param = end_rec_dev_yr - start_rec_dev_yr + 1,
                bounds = paste0("(", ctl$min_rec_dev, ", ",
                                ctl$max_rec_dev, ")"),
                prior_str = NA) |>
    mutate(across(everything(), as.character))
  row$prior_str <- get_prior_string(row, digits)
  sr_params <- sr_params |>
    bind_rows(row)


  # Natural mortality ----
  mg_params <- convert_ctl_file_param_dfs(ctl, "MG_parms")

  row <- mg_params |>
    dplyr::filter(grepl("natm", param))

  if(nrow(row) != 1){
    stop("Either the natural mortality parameter was not found in the ",
         "control file or there is more than one of them")
  }
  row$param <- "m"
  row$num_param <- ifelse(row$phase <= 0 , "--", "1")
  row$bounds <- paste0("(", row$lo, ", ", row$hi, ")")
  row$latex_nm <- paste0("Natural mortality (", latex_italics("M"), ")")
  row$prior_str <- get_prior_string(row, digits)
  sr_params <- sr_params |>
    bind_rows(row)

  # Survey additional value for SE ----
  q_params <- convert_ctl_file_param_dfs(ctl, "Q_parms")

  row <- q_params |>
    dplyr::filter(grepl("extrasd_acoustic", param))

  if(nrow(row) != 1){
    stop("Either the 'extra SD acoustic survey' parameter was not found ",
         "in the control file or there is more than one of them")
  }

  row$param <- "extra_sd_survey"
  row$num_param <- ifelse(row$phase <= 0 , "--", "1")
  row$bounds <- paste0("(", row$lo, ", ", row$hi, ")")
  row$latex_nm <- "Additional variance for survey log(SE)"
  row$prior_str <- get_prior_string(row, digits)
  sr_params <- sr_params |>
    bind_rows(row)

  # Survey selectivity parameters ----
  sel_params <- convert_ctl_file_param_dfs(ctl, "age_selex_parms") |>
    dplyr::filter(grepl("acoustic_survey", param))

  ages <- as.character(as.numeric(gsub("agesel_p_([0-9]+).*",
                                       "\\1",
                                       sel_params$param)) - 1)
  sel_params <- sel_params |>
    mutate(param = ages)

  if(!nrow(sel_params)){
    stop("The 'selectivity acoustic survey' parameters was not found ",
         "in the control file")
  }

  ages_estimated_df <- sel_params |>
    dplyr::filter(phase > 0)
  row <- ages_estimated_df |>
    slice(1)
  if(nrow(ages_estimated_df)){
    row$num_param <- as.character(nrow(ages_estimated_df))
  }else{
    row$num_param <- "--"
  }
  row$param <- "age_sel_survey"
  row$bounds <- paste0("(", row$lo, ", ", row$hi, ")")
  row$latex_nm <- paste0("Non-parametric age-based selectivity: ages ",
                         min(ages_estimated_df$param),
                         "--",
                         max(ages_estimated_df$param))

  row$prior_str <- get_prior_string(row, digits)
  sr_params <- sr_params |>
    bind_rows(row)

  # Age-1 survey additional value for SE ----
  q_params <- convert_ctl_file_param_dfs(ctl, "Q_parms")

  row <- q_params |>
    dplyr::filter(grepl("extrasd_age1", param))

  if(nrow(row) != 1){
    warning("Either the 'extra SD age 1' parameter was not found ",
            "in the control file or there is more than one of them")
  }

  if(nrow(row) == 1){
    row$param <- "extra_sd_age1"
    row$num_param <- ifelse(row$phase <= 0 , "--", "1")
    row$bounds <- paste0("(", row$lo, ", ", row$hi, ")")
    row$latex_nm <- "Additional variance for age-1 index log(SE)"
    row$prior_str <- get_prior_string(row, digits)
    sr_params <- sr_params |>
      bind_rows(row)
  }

  # Fishery selectivity parameters ----
  sel_params <- convert_ctl_file_param_dfs(ctl, "age_selex_parms") |>
    dplyr::filter(grepl("fishery", param))

  ages <- as.character(as.numeric(gsub("agesel_p_([0-9]+).*",
                                       "\\1",
                                       sel_params$param)) - 1)
  sel_params <- sel_params |>
    mutate(param = ages)

  if(!nrow(sel_params)){
    stop("The 'selectivity acoustic survey' parameters was not found ",
         "in the control file")
  }

  ages_estimated_df <- sel_params |>
    dplyr::filter(phase > 0)
  # Assumes all estimated age selectivities have same starting conditions
  row <- ages_estimated_df |>
    slice(1)
  if(nrow(ages_estimated_df)){
    row$num_param <- as.character(nrow(ages_estimated_df))
  }else{
    row$num_param <- "--"
  }
  row$param <- "age_sel_fishery"
  row$bounds <- paste0("(", row$lo, ", ", row$hi, ")")
  row$latex_nm <- paste0("Non-parametric age-based selectivity: ages ",
                         min(ages_estimated_df$param),
                         "--",
                         max(ages_estimated_df$param))

  row$prior_str <- get_prior_string(row, digits)
  sr_params <- sr_params |>
    bind_rows(row)

  # Selectivity deviations for fishery ----
  sel_devs <- model$parameter_priors$`Fishery recruitment deviations`
  sel_devs_tv <- ctl$age_selex_parms_tv |>
    as_tibble(rownames = "param")

  sel_dev_mean <- sel_devs_tv |>
    dplyr::filter(grepl("dev_auto", param)) |>
    first() |>
    pull(INIT) |>
    as.character()

  sel_dev_sd <- sel_devs_tv |>
    dplyr::filter(grepl("dev_se", param)) |>
    first() |>
    pull(INIT) |>
    as.character()

  sel_dev_yrs <- gsub(".*DEVadd_([0-9]+)$", "\\1", sel_devs$label)
  start_sel_dev_yr <- min(sel_dev_yrs)
  end_sel_dev_yr <- max(sel_dev_yrs)

  prior_type <- first(sel_devs$pr_type)
  if(prior_type == "dev"){
    prior_type <- "4"
  }

  row <- tibble(param = "seldevs",
                hi = as.character(first(sel_devs$min)),
                lo = as.character(first(sel_devs$max)),
                type = prior_type,
                phase = as.character(ctl$recdev_phase),
                init = as.character(first(sel_devs$init)),
                mean = sel_dev_mean,
                sd = sel_dev_sd,
                latex_nm = paste0("Selectivity deviations (",
                                  start_sel_dev_yr, "--", end_sel_dev_yr,
                                  ", ages ",
                                  min(ages_estimated_df$param),
                                  "--",
                                  max(ages_estimated_df$param),
                                  ")"),
                num_param = nrow(sel_devs),
                bounds = paste0("(", first(sel_devs$min), ", ",
                                first(sel_devs$max), ")"),
                prior_str = NA) |>
    mutate(across(everything(), as.character))
  row$prior_str <- get_prior_string(row, digits)
  sr_params <- sr_params |>
    bind_rows(row)

  # Dirichlet-Multinomial - fishery ----
  dm_params <- convert_ctl_file_param_dfs(ctl, "dirichlet_parms")

  row <- dm_params |>
    dplyr::filter(grepl(".*1$", param))

  if(nrow(row) != 1){
    stop("Either the 'Dirichlet multinomial fishery' parameter was not found ",
         "in the control file or there is more than one of them")
  }

  row$param <- "dm_fishery"
  row$num_param <- ifelse(row$phase <= 0 , "--", "2")
  row$bounds <- paste0("(", row$lo, ", ", row$hi, ")")
  row$latex_nm <- paste0("Dirichlet-multinomial fishery likelihood, ",
                         latex_italics("$\\log(\\theta_{fishery})$"))
  row$prior_str <- get_prior_string(row, digits)
  sr_params <- sr_params |>
    bind_rows(row)

  # Dirichlet multinomial - survey ----
  row <- dm_params |>
    dplyr::filter(grepl(".*2$", param))

  if(nrow(row) != 1){
    stop("Either the 'Dirichlet multinomial survey' parameter was not found ",
         "in the control file or there is more than one of them")
  }

  row$param <- "dm_survey"
  row$num_param <- ifelse(row$phase <= 0 , "--", "2")
  row$bounds <- paste0("(", row$lo, ", ", row$hi, ")")
  row$latex_nm <- paste0("Dirichlet-multinomial survey likelihood, ",
                         latex_italics("$\\log(\\theta_{survey})$"))
  row$prior_str <- get_prior_string(row, digits)
  sr_params <- sr_params |>
    bind_rows(row)

  # Clean up populated data frame ----
  if(ret_df){
    return(sr_params)
  }

  d <- sr_params |>
    select(latex_nm, num_param, bounds, prior_str)

  names(d) <- c("Parameter",
                "Number of\nparameters",
                "Bounds\n(low, high)",
                "Prior (Mean, SD)\nsingle value = fixed")

  d <- insert_row(d,
                  c(paste0("\\textbf{\\underline{Stock Dynamics}}"),
                    "", "", ""), 1)
  d <- insert_row(d,
                   c(paste0("\\textbf{\\underline{Data Source}}"),
                     "", "", ""), 7)
  d <- insert_row(d,
                  c(paste0("\\textbf{\\underline{Data Weighting}}"),
                    "", "", ""), 12)
  d <- insert_row(d,
                   c(paste0("\\textbf{\\emph{Acoustic Survey}}"),
                     "", "", ""), 8)

  # To add Age-1 back in, uncomment the following and mess with the row indices
  # following until correct. Also comment out the line below which filters out
  # the row containing "Non-parametric age-based selectivity: ages 2--6".
  # Use gotest() to test

  # d <- insert_row(d,
  #                  c(paste0("\\textbf{\\emph{Age-1 Survey}}"),
  #                    "", "", ""), 11)

  d <- d |>
    dplyr::filter(Parameter != "Non-parametric age-based selectivity: ages 2--6")

  d <- insert_row(d,
                  c(paste0("\\textbf{\\emph{Fishery and Survey}}"),
                    "", "", ""), 11)
  sec_inds <- c(1, 7, 13)
  # Add spaces after commas and before opening parentheses
  d <- map_df(d, ~{gsub(",", ", ", .x)})
  d <- map_df(d, ~{gsub("\\(", " \\(", .x)})

  # Insert custom header fontsize before linebreaker
  if(is.null(header_font_size)){
    header_font_size <- font_size
  }
  hdr_font_str <- create_fontsize_str(header_font_size,
                                      header_vert_spacing,
                                      header_vert_scale)
  col_names <- names(d)
  col_names <- gsub("\\n", paste0("\n", hdr_font_str$quad), col_names)
  col_names <- paste0(hdr_font_str$dbl, col_names)
  # Add \\makecell{} latex command to headers with newlines
  col_names <- linebreaker(col_names, align = "c")

  k <- kbl(d,
           format = "latex",
           booktabs = TRUE,
           align = c("l", "c", "c", "c"),
           linesep = "",
           col.names = col_names,
           escape = FALSE,
           ...) |>
    row_spec(0, bold = TRUE) |>
    kable_styling(font_size = font_size,
                  latex_options = c("repeat_header", "hold_position"),
                  position = "left")

  # Place a line under the sections as a way to separate them  vertically
  # from the sections above and below
  if(section_sep_lines){
    # Don't place a line before the first position as it will create a double
    # line at the top of the table
    sec_inds_above <- sec_inds[sec_inds != 1]
    # Place the lines above the section headers, so subtract one from those
    # because we are using `extra_latex_after`
    sec_inds_above <- sec_inds_above - 1
    k <- k |>
      row_spec(sec_inds_above,
               extra_latex_after = paste0("\\cline{",
                                          1,
                                          "-",
                                          ncol(d),
                                          "}"))
  }

  k
}
