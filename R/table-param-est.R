#' Returns an [xtable::xtable()] in the proper format for the MCMC posterior
#' parameter estimates for all the models in the list, one for each column
#'
#' @param models A list of models which contain the MCMC output
#' @param model_nms A vector of names of the same length as the number of
#' models in the models list
#' @param end_yr The last year to include
#' @param section_italics Logical. If `TRUE`, make the section header lines
#' italicized
#' @param section_bold Logical. If `TRUE`, make the section header lines
#' boldface
#' @param section_underline Logical. If `TRUE`, make the section header lines
#' underlined
#' @param section_line_above Logical. If `TRUE`, place a horizontal line above
#' section header lines
#' @param section_line_below Logical. If `TRUE`, place a horizontal line below
#' section header lines
#' @param digits Number of decimal points for the estimates
#' @param rec_yrs A vector of integers supplying the years for which you
#' want estimates of recruitment
#' @param right_cols_cm The number of centimeters wide to make all of the
#' rightmost columns (all the value columns)
#' @param font_size The table data and header font size in points
#' @param header_font_size The font size for the headers only. If `NULL`,
#' the headers will have the same font size as the table cell data
#' @param vert_spacing The vertical spacing between newlines for this font.
#' If `NULL` this will be calculated as `header_font_size * header_vert_scale`
#' @param header_vert_scale Scale factor to create the vertical spacing value.
#' See `header_vert_spacing`
#' @param ... Arguments passed to [knitr::kable()]
#'
#' @return An [knitr::kable()] object
#' @export
table_param_est <- function(models,
                            model_nms,
                            end_yr,
                            section_italics = TRUE,
                            section_bold = TRUE,
                            section_underline = TRUE,
                            section_line_above = FALSE,
                            section_line_below = TRUE,
                            digits = 3,
                            rec_yrs = NA,
                            right_cols_cm = 1.8,
                            font_size = 10,
                            header_font_size = 10,
                            header_vert_spacing = 12,
                            header_vert_scale = 1.2,
                            ...){

  if(is.na(rec_yrs[1])){
    stop("No cohort recruitment years supplied: `rec_yrs`",
         call. = FALSE)
  }

  # MCMC parameter estimates
  d <- map2(models, model_nms, function(mdl, mdl_nm){

    q_acoustic <- NA
    q_age1 <- NA
    recs <- NA

    if(mdl$extra_mcmc_exists){
      recs <- map_dbl(rec_yrs, \(rec_yr){
        mdl$extra_mcmc$recr_cohorts |>
          filter(yr == rec_yr) |>
          pull(`50%`)
      }) |>
        set_names(rec_yrs)
      recs <- recs / 1e3
      recs <- f(recs, 0)

      q_med <- mdl$extra_mcmc$q_med
      # Filter for last year in the series, since q's are not time varying
      q_acoustic <- q_med |>
        filter(fleet == 2)
      if(nrow(q_acoustic)){
        q_acoustic <- q_acoustic |>
          pull(value)
        q_acoustic <- q_acoustic[length(q_acoustic)]
      }else{
        q_acoustic <- NA
      }
      q_age1 <- q_med |>
        filter(fleet == 3)
      if(nrow(q_age1)){
        q_age1 <- q_age1 |>
          pull(value)
        q_age1 <- q_age1[length(q_age1)]
      }else{
        q_age1 <- NA
      }
    }

    like <- mdl$likelihoods_used |>
      as_tibble(rownames = "type") |>
      mutate(type = tolower(type)) |>
      select(-lambdas)

    like_fleet <- mdl$likelihoods_by_fleet |>
      as_tibble() |>
      mutate(Label = tolower(Label)) |>
      rename_all(~{tolower(.x)})
    age_like <- like_fleet |>
      filter(label == "age_like")
    survey_age_like <- age_like |>
      pull(acoustic_survey)
    fishery_age_like <- age_like |>
      pull(fishery)

    df <- enframe(
      c(nat_m = f(mdl$mcmccalcs$m[2], digits),
        ro = f(mdl$mcmccalcs$ro[2]),
        h =  f(mdl$mcmccalcs$steep[2], digits),
        survey_sd = f(mdl$mcmccalcs$survey_sd[2], digits),
        catchability = ifelse(is.na(q_acoustic), NA, f(q_acoustic, digits)),
        survey_age1_sd = f(mdl$mcmccalcs$age1_index_sd[2], digits),
        catchability_age1 = ifelse(all(is.na(q_age1)), NA, f(q_age1, digits)),
        dm_fishery = f(mdl$mcmccalcs$dm_fishery[2], digits),
        dm_survey = f(mdl$mcmccalcs$dm_survey[2], digits),
        recs_2010 = recs[1],
        recs_2014 = recs[2],
        recs_2016 = recs[3],
        recs_2020 = recs[4],
        bo = mdl$mcmccalcs$refpts$unfish_fem_bio[2],
        ssb_2009 =
          paste0(f(mdl$mcmccalcs$dmed["2009"] * 100, 1),
                 "\\%"),
        ssb_curr =
          ifelse(mdl$endyr <= end_yr - 2,
                 "--",
                 paste0(f(mdl$mcmccalcs$dmed[as.character(mdl$endyr + 1)] * 100, 1),
                        "\\%")),
        spr_last =
          ifelse(mdl$endyr <= end_yr - 2,
                 "--",
                 paste0(f(mdl$mcmccalcs$pmed[as.character(mdl$endyr)] * 100, 1),
                        "\\%")),
        ssb_curr_fem = mdl$mcmccalcs$refpts$f_spawn_bio_bf40[2],
        spr_msy = "40.0\\%",
        exp_frac = mdl$mcmccalcs$refpts$exp_frac_spr[2],
        yield_f40 = mdl$mcmccalcs$refpts$yield_b40[2],
        total_like = f(pull(filter(like, type == "total")), 2),
        survey_like = f(pull(filter(like, type == "survey")), 2),
        survey_age_like = f(survey_age_like, 2),
        fishery_age_like = f(fishery_age_like, 2),
        recr_like = f(pull(filter(like, type == "recruitment")), 2),
        priors_like = f(pull(filter(like, type == "parm_priors")), 2),
        parmdev_like = f(pull(filter(like, type == "parm_devs")), 2)),
      value = mdl_nm)
  })

  # Remove parameter name column from all but first model then bind them all together,
  # make a variable that records if there are any age-1 index parameter values,
  # replace NAs with double-dashes, and add a blank row at the top for aesthetic purposes
  d[-1] <- map(d[-1], \(mdl_d){
    mdl_d |>
      select(-name)
  })
  d <- d |>
    # All this name repair stuff just silences the New names.... messages
    # caused during the binding of columns
    bind_cols(.name_repair = ~vec_as_names(..., quiet = TRUE)) |>
    set_names(c("parameter", model_nms))

   param_descs <- enframe(c(
      "Parameters",
      paste0("Natural mortality (",
             latex_italics("M"),
             ")"),
      paste0("Unfished recruitment (",
             latex_subscr(latex_italics("R"),
                          "0"),
             ", millions)"),
      paste0("Steepness (",
             latex_italics("h"),
             ")"),
      "Additional biomass index SD",
      paste0("Catchability: biomass index (",
             latex_italics("$q_b$"),
             ")"),
      "Additional age-1 index SD",
      paste0("Catchability: age-1 index (",
             latex_italics("$q_1$"),
             ")"),
      "Dirichlet-multinomial fishery (log~$\\theta_{\\text{fish}}$)",
      "Dirichlet-multinomial survey (log~$\\theta_{\\text{surv}}$)",
      "Derived Quantities",
      paste(rec_yrs, "recruitment (millions)"),
      paste0("Unfished female spawning biomass (",
             latex_subscr(latex_italics("B"), "0"), ", kt)"),
      "2009 relative spawning biomass",
      paste0(end_yr, " relative spawning biomass"),
      paste0(end_yr - 1, " rel. fishing intensity: (1-SPR)/(1-",
             latex_subscr("SPR", "40\\%"), ")"),
      "Reference Points based on $\\bm{\\Fforty}$",
      "Female spawning biomass at $\\FSPRfortynoit$ ($\\BSPRfortynoit$, kt)",
      "SPR at $\\FSPRfortynoit$ (kt)",
      "Exploitation fraction corresponding to SPR",
      "Yield at $\\BSPRfortynoit$ (kt)",
      "Negative log likelihoods",
      "Total",
      "Survey index",
      "Survey age compositions",
      "Fishery age compositions",
      "Recruitment",
      "Parameter priors",
      "parameter deviations"),
    name = NULL)

   sec_inds <- c(1, 11, 20, 25)

   # Insert empty rows at the row indices where the section headers are
   row_vec <- rep("", length(models) + 1)
   walk(sec_inds, ~{
     d <<- insert_row(d, row_vec, .x)
   })

   d <- d |>
     mutate(parameter = param_descs$value)

   if(section_underline){
     # Make section headers bold
     d[sec_inds, "parameter"] <- map(d[sec_inds, "parameter"], ~{
       latex_under(.x)
     })
   }
   if(section_italics){
     # Make section headers italics
     d[sec_inds, "parameter"] <- map(d[sec_inds, "parameter"], ~{
       latex_italics(.x)
     })
   }
   if(section_bold){
     # Make section headers bold
     d[sec_inds, "parameter"] <- map(d[sec_inds, "parameter"], ~{
       latex_bold(.x)
     })
   }

   # Replace NA and "NA" with "--"
   d <- map_df(d, ~{
     gsub(" *NA *", "--", .x)
   })
   d[is.na(d)] <- "--"

  col_names <- c("Parameter, Quantity, or Reference point",
                 gsub(" +", "\n", model_nms))

  # Introduce newline at a slash seperator
  col_names <- gsub("\\/", "\\\\/\n", col_names)

  # Insert custom header fontsize before linebreaker
  if(is.null(header_font_size)){
    header_font_size <- font_size
  }
  hdr_font_str <- create_fontsize_str(header_font_size,
                                      header_vert_spacing,
                                      header_vert_scale)

  col_names <- gsub("\\n", paste0("\n", hdr_font_str$quad), col_names)
  col_names <- paste0(hdr_font_str$dbl, col_names)
  # Add \\makecell{} latex command to headers with newlines
  col_names <- linebreaker(col_names, align = "c")

  k <- kbl(d,
           format = "latex",
           booktabs = TRUE,
           align = c("l",
                     rep(paste0("R{",
                                right_cols_cm,
                                "cm}"),
                         length(models))),
           linesep = "",
           col.names = col_names,
           escape = FALSE,
           ...) |>
    row_spec(0, bold = TRUE)

  if(section_line_above){
    # Do not show a line above if a section starts on the first line
    # as it creates a double line at the top of the table
    sec_inds_above <- sec_inds
    sec_inds_above <- sec_inds_above[sec_inds_above != 1]
    k <- k |>
      row_spec(sec_inds_above - 1,
               extra_latex_after = paste0("\\cline{",
                                          1,
                                          "-",
                                          length(col_names),
                                          "}"))
  }
  if(section_line_below){
    k <- k |>
      row_spec(sec_inds,
               extra_latex_after = paste0("\\cline{",
                                          1,
                                          "-",
                                          length(col_names),
                                          "}"))
  }

  k |>
    kable_styling(font_size = font_size,
                  latex_options = c("repeat_header"))
}
