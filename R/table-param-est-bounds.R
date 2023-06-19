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
#' @return Either a data frame or [kableExtra::kbl()] object
#' @export
table_param_est_bounds <- function(model,
                                   start_rec_dev_yr,
                                   end_rec_dev_yr,
                                   section_sep_lines = FALSE,
                                   digits = 3,
                                   font_size = 8,
                                   header_font_size = 10,
                                   header_vert_spacing = 12,
                                   header_vert_scale = 1.2,
                                   ret_df = FALSE,
                                   ...){

  lo <- 1
  hi <- 2
  init <- 3
  p_mean <- 4 # prior mean
  p_sd <- 5   # prior sd
  p_type <- 6 # prior type
  phase <- 7
  start_yr_sel <- 10
  end_yr_sel <- 11

  prior_type <- c("0" = "Uniform",
                  "-1" = "Uniform",
                  "2" = "Beta",
                  "3" = "Lognormal")

  # Fetch the line `x` from the vector `ctl` and split it up, removing spaces.
  # Also remove any leading spaces
  #
  # @param ctl The input vector of lines from the control file
  # @param x The line to extract from vector `vec`
  #
  # @return A vector of values
  fetch_and_split <- function(ctl, x){

    j <- ctl[x]
    # Remove inter-number spaces
    j <- strsplit(j," +")[[1]]
    # Remove leading spaces
    j <- j[j != ""]
    return(j)
  }

  # Looks at the prior type p_type and phase, and if uniform will return
  #  "Uniform"
  # If not uniform, it will parse the `vals` and build a string defining
  #  the prior info.
  # If Fixed, it will return the initial value
  # If Lognormal, it will parse the `vals` and build a string defining the
  #  prior info, with the exp function applied
  #
  # @param vals The parameter initial values and settings from the control file
  # @param digits The number of decimal points to return
  #
  # @return A string with the prior type and initial values in parentheses
  fetch_prior_info <- function(vals, digits = 2){
    if(vals[p_type] < 0 & vals[phase] > 0){
      # Uniform prior on estimated parameter
      return("Uniform")
    }
    if(vals[p_type] < 0 & vals[phase] < 0){
      # Fixed parameter
      return(vals[init])
    }
    paste0(prior_type[vals[p_type]], " (",
           f(as.numeric(vals[p_mean]), digits), ", ",
           f(as.numeric(vals[p_sd]), digits), ")")
  }

  ctl <- model$ctl
  # Remove preceding and trailing whitespace on all elements,
  #  but not 'between' whitespace.
  ctl <- trimws(ctl)
  # Remove all lines that start with a comment
  ctl <- ctl[-grep("^#.*", ctl)]

  # R0
  pat <- "_SR_LN\\(R0\\)$"
  ind <- grep(pat, ctl)
  if(!length(ind)){stop("`ctl` did not have a line containing ", pat)}
  r0 <- fetch_and_split(ctl, ind)
  r0_vals <- c(paste0("Log (",
                      latex_subscr(latex_italics("R"),
                                   "0"),
                      ")"),
               1,
               paste0("(", r0[lo], ", ", r0[hi], ")"),
               prior_type[r0[p_type]])

  # Steepness
  pat <- "_SR_BH_steep$"
  ind <- grep(pat, ctl)
  if(!length(ind)){stop("`ctl` did not have a line containing ", pat)}
  h <- fetch_and_split(ctl, ind)
  h_vals <- c(paste0("Steepness (",
                     latex_italics("h"),
                     ")"),
              1,
              paste0("(", h[lo], ", ", h[hi], ")"),
              fetch_prior_info(h, digits))

  # Recruitment variability (sigma_r)
  pat <- "_SR_sigmaR$"
  ind <- grep(pat, ctl)
  if(!length(ind)){stop("`ctl` did not have a line containing ", pat)}
  sig_r <- fetch_and_split(ctl, ind)
  sig_r_vals <- c(paste0("Recruitment variability (",
                         latex_italics("$\\sigma_r$"),
                         ")"),
                  if(sig_r[p_type] < 0 & sig_r[phase] > 0)
                    1
                  else
                    "--",
                  if(sig_r[p_type] < 0 & sig_r[phase] > 0)
                    paste0(" (", sig_r[lo], ", ", sig_r[hi], ")")
                  else
                    "--",
                  sig_r[3])

  # Recruitment devs
  # The number of them comes from the arguments to this function (for now)
  pat_lb <- "#min rec_dev$"
  pat_ub <- "#max rec_dev$"
  ind_lb <- grep(pat_lb, ctl)
  ind_ub <- grep(pat_ub, ctl)
  if(!length(ind_lb)){stop("`ctl` did not have a line containing ", pat_lb)}
  if(!length(ind_ub)){stop("`ctl` did not have a line containing ", pat_ub)}
  rec_dev_lb <- fetch_and_split(ctl, ind_lb)[1]
  rec_dev_ub <- fetch_and_split(ctl, ind_ub)[1]
  rec_dev_vals <- c(paste0("Log recruitment deviations: ",
                           start_rec_dev_yr,
                           "--",
                           end_rec_dev_yr),
                    end_rec_dev_yr - start_rec_dev_yr + 1,
                    paste0("(",
                           rec_dev_lb,
                           ", ",
                           rec_dev_ub,
                           ")"),
                    paste0("Lognormal (0, ",
                           latex_italics("$\\sigma_r$"),
                           ")"))

  # Natural mortality
  pat <- "#_NatM_p_1_Fem_GP_1$"
  ind <- grep(pat, ctl)
  if(!length(ind)){stop("`ctl` did not have a line containing ", pat)}
  m <- fetch_and_split(ctl, ind)
  m.vals <- c(paste0("Natural mortality (",
                     latex_italics("M"),
                     ")"),
              if(prior_type[m[p_type]] == "Fixed")
                "--"
              else
                1,
              paste0("(", m[lo], ", ", m[hi], ")"),
              fetch_prior_info(m, digits))

  # Survey additional value for SE
  pat <- "#_Q_extraSD_Acoustic_Survey\\(2\\)$"
  ind <- grep(pat, ctl)
  if(!length(ind)){stop("`ctl` did not have a line containing ", pat)}
  se <- fetch_and_split(ctl, ind)
  se_vals <- c("Additional variance for survey log(SE)",
               1,
               paste0("(", se[lo], ", ", se[hi], ")"),
               "Uniform")

  # Survey selectivity parameters
  pat <- "AgeSel_.*_Survey.*[0-9]\\)$"
  vals <- grep(pat, ctl, value = TRUE)
  vals_split <- strsplit(vals, "\\s+")
  vals_df <- data.frame(do.call("rbind", vals_split))
  # Which parameters have phase greater than zero, subtract 1 because the ages
  # start at zero so the row numbers are off by one
  s_ages_estimated <- which(vals_df[, 7] > 0) - 1
  age_sel_vals <- c(paste0("Non-parametric age-based selectivity: ages ",
                           min(s_ages_estimated),
                           "--",
                           max(s_ages_estimated)),
                    length(s_ages_estimated),
                    paste0(" (",
                           min(vals_df[vals_df[, 7] > 0, lo]),
                           ", ",
                           min(vals_df[vals_df[, 7] > 0, hi]), ")"),
                    "Uniform")

  # Age-1 survey additional value for SE
  pat <- "#_Q_extraSD_Age1_Survey\\(3\\)$"
  ind <- grep(pat, ctl)
  if(!length(ind)){stop("`ctl` did not have a line containing ", pat)}
  se_age1 <- fetch_and_split(ctl, ind)
  se_age1_vals <- c("Additional variance for age-1 index log(SE)",
                    1,
                    paste0("(", se_age1[lo], ", ", se_age1[hi], ")"),
                    "Uniform")


  # Survey selectivity parameters
  pat <- "AgeSel_.*_Fishery.*[0-9]\\)$"
  vals <- grep(pat, ctl, value = TRUE)
  vals_split <- strsplit(vals, "\\s+")
  vals_df <- data.frame(do.call("rbind", vals_split))
  # Which parameters have phase greater than zero, subtract 1 because the ages
  # start at zero so the row numbers are off by one
  f_ages_estimated <- which(vals_df[, 7] > 0) - 1
  f_age_sel_vals <- c(paste0("Non-parametric age-based selectivity: ages ",
                             min(f_ages_estimated),
                             "--",
                             max(f_ages_estimated)),
                      length(f_ages_estimated),
                      paste0("(", min(vals_df[vals_df[, 7] > 0, lo]), ", ",
                             max(vals_df[vals_df[, 7] > 0, hi]), ")"),
                      "Uniform")
  n_yrs_sel_vals <-
    diff(as.numeric(vals_df[vals_df[, 7] > 0,
                            start_yr_sel:end_yr_sel][1, 1:2])) + 1

  # Selectivity deviations for fishery
  sel_devs <- model$parameter_priors$`Fishery recruitment deviations`

  sel_dev_bounds <- paste0("(",
                           first(sel_devs$min),
                           ", ",
                           first(sel_devs$max),
                           ")")

  f_age_sel_dev_vals <-
    c(paste0("Selectivity deviations (",
             min(vals_df[vals_df[, start_yr_sel] != 0, start_yr_sel]),
             "--",
             max(vals_df[vals_df[, end_yr_sel] != 0, end_yr_sel]),
             ", ages ",
             min(f_ages_estimated),
             "--",
             max(f_ages_estimated),
             ")"),
      length(f_ages_estimated) * n_yrs_sel_vals,
      sel_dev_bounds,
      paste0("Normal (0, ",
             model$mcmcparams$param_recdevs_se,
             ")"))

  # Dirichlet-Multinomial likelihood parameters
  dm_inds <- grep("DM", ctl)
  if(!length(dm_inds)){
    stop("`DM` not found n the control file, cannot extract ",
         "Dirichlet-multinomial parameter settings",
         call. = FALSE)
  }
  if(length(dm_inds) != 2){
    stop("`DM` does not match exactly two items in the control file. See ",
         "the code in table_param_est_bounds()",
         call. = FALSE)
  }
  dm <- map(dm_inds, ~{fetch_and_split(ctl, .x)})

  dmf <- dm[[1]]
  dms <- dm[[2]]
  dmf_vals <- c(paste0("Dirichlet-multinomial fishery likelihood, ",
                       latex_italics("$\\log(\\theta_{fish})$")),
                2,
                paste0("(", dmf[lo], ", ", dmf[hi], ")"),
                paste0("Normal (",
                       dmf[4], ", ",
                       dmf[5],
                       ")"))

  dms_vals <- c(paste0("Dirichlet-multinomial survey likelihood, ",
                       latex_italics("$\\log(\\theta_{survey})$")),
                2,
                paste0("(", dms[lo], ", ", dms[hi], ")"),
                paste0("Normal (",
                       dms[4], ", ",
                       dms[5],
                       ")"))

  d <- rbind(r0_vals,
             h_vals,
             sig_r_vals,
             rec_dev_vals,
             m.vals,
             se_vals,
             age_sel_vals,
             se_age1_vals,
             f_age_sel_vals,
             f_age_sel_dev_vals,
             dmf_vals,
             dms_vals) |>
    as.data.frame() |>
    as_tibble()

  if(ret_df){
    return(d)
  }

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
                    "", "", ""), 13)
  d <- insert_row(d,
                   c(paste0("\\textbf{\\emph{Acoustic Survey}}"),
                     "", "", ""), 8)
  d <- insert_row(d,
                   c(paste0("\\textbf{\\emph{Age-1 Survey}}"),
                     "", "", ""), 11)
  d <- insert_row(d,
                   c(paste0("\\textbf{\\emph{Fishery Survey}}"),
                     "", "", ""), 13)
  sec_inds <- c(1, 7, 16)
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
