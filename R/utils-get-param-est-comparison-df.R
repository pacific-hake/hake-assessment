#' Create the data frame that holds parameter estimates for several models
#' for comparison purposes in a table
#'
#' @param models A list of models which contain the MCMC output
#' @param model_nms A vector of names of the same length as the number of
#' models in the models list
#' @param end_yr The last year to include
#' @param digits The number of decimal points to include in the table
#' @param inc_loglike Logical. If `TRUE`, include the negative log-likelihood
#' values
#' @param ... Absorbs arguments meant for other functions
#'
#' @return A data frame containing 1 column of parameter estimates for each
#' model in `models` list
#' @export
get_param_est_comparison_df <- function(models,
                                        model_nms,
                                        end_yr = models[[1]]$endyr,
                                        digits = 3,
                                        inc_loglike = TRUE,
                                        ...){

  # MCMC parameter estimates
  d <- map2(models, model_nms, function(mdl, mdl_nm){

    q_acoustic <- NA
    q_age1 <- NA
    recs <- NA

    if(mdl$extra_mcmc_exists){
      recs <- map_dbl(large_cohorts, \(rec_yr){
        mdl$extra_mcmc$recr_cohorts |>
          dplyr::filter(yr == rec_yr) |>
          pull(`50%`)
      }) |>
        set_names(paste0("recs_", large_cohorts))
      recs <- f(recs / 1e3)

      q_med <- mdl$extra_mcmc$q_med
      # Filter for last year in the series, since q's are not time varying
      q_acoustic <- q_med |>
        dplyr::filter(fleet == 2)
      if(nrow(q_acoustic)){
        q_acoustic <- q_acoustic |>
          pull(value)
        q_acoustic <- q_acoustic[length(q_acoustic)]
      }else{
        q_acoustic <- NA
      }
      q_age1 <- q_med |>
        dplyr::filter(fleet == 3)
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
      dplyr::filter(label == "age_like")
    survey_age_like <- age_like |>
      pull(acoustic_survey)
    fishery_age_like <- age_like |>
      pull(fishery)

    df <- enframe(
      c(nat_m = f(mdl$mcmccalcs$m[2], digits),
        ro = f(mdl$mcmccalcs$ro[2]),
        h = ifelse(is.null(mdl$mcmccalcs$steep),
                   NA,
                   f(mdl$mcmccalcs$steep[2], digits)),
        survey_sd = f(mdl$mcmccalcs$survey_sd[2], digits),
        catchability = ifelse(is.na(q_acoustic), NA, f(q_acoustic, digits)),
        survey_age1_sd = ifelse(is.null(mdl$mcmccalcs$age1_index_sd[1]),
                                NA,
                                f(mdl$mcmccalcs$age1_index_sd[2], digits)),
        catchability_age1 = ifelse(all(is.na(q_age1)), NA, f(q_age1, digits)),
        dm_fishery = ifelse(is.null(mdl$mcmccalcs$dm_fishery[1]),
                            NA,
                            f(mdl$mcmccalcs$dm_fishery[2], digits)),
        dm_survey = ifelse(is.null(mdl$mcmccalcs$dm_survey[1]),
                           NA,
                           f(mdl$mcmccalcs$dm_survey[2], digits)),
        # `recs` is a named vector of all recruitment cohorts
        recs,
        bo = mdl$mcmccalcs$refpts$unfish_fem_bio[2],
        #ssb_2009 =
        #  paste0(f(mdl$mcmccalcs$dmed["2009"] * 100, 1),
        #         "\\%"),
        ssb_last =
          ifelse(mdl$Retro_year < end_yr - 1,
                 paste0(f(mdl$mcmccalcs$dmed[as.character(mdl$endyr + 1)] * 100,
                          1),
                        "\\%"),
                 paste0(f(mdl$mcmccalcs$dmed[as.character(end_yr)] * 100,
                          1),
                        "\\%")),
        ssb_curr =
          ifelse(mdl$Retro_year < end_yr,
                 "--",
                 paste0(f(mdl$mcmccalcs$dmed[as.character(mdl$endyr + 1)] * 100,
                          1),
                        "\\%")),
        spr_last =
          ifelse(mdl$Retro_year < end_yr,
                 "--",
                 paste0(f(mdl$mcmccalcs$pmed[as.character(mdl$endyr)] * 100,
                          1),
                        "\\%")),
        ssb_curr_fem = mdl$mcmccalcs$refpts$f_spawn_bio_bf40[2],
        spr_msy = "40.0\\%",
        exp_frac = mdl$mcmccalcs$refpts$exp_frac_spr[2],
        yield_f40 = mdl$mcmccalcs$refpts$yield_bf40[2]),
      value = mdl_nm)
    if(inc_loglike){
      df_nll <- enframe(
        c(total_like = f(pull(dplyr::filter(like, type == "total")), 2),
          survey_like = f(pull(dplyr::filter(like, type == "survey")), 2),
          survey_age_like = f(survey_age_like, 2),
          fishery_age_like = f(fishery_age_like, 2),
          recr_like = f(pull(dplyr::filter(like, type == "recruitment")), 2),
          priors_like = f(pull(dplyr::filter(like, type == "parm_priors")), 2),
          parmdev_like = f(pull(dplyr::filter(like, type == "parm_devs")), 2)),
        value = mdl_nm)
      df <- bind_rows(df, df_nll)
    }
    df
  })

  # Remove parameter name column from all but first model then bind them
  # all together, make a variable that records if there are any age-1
  # index parameter values, replace NAs with double-dashes, and add a blank
  # row at the top for aesthetic purposes
  d[-1] <- map(d[-1], \(mdl_d){
    mdl_d |>
      select(-name)
  })
  d <- d |>
    bind_cols_quiet() |>
    set_names(c("parameter", model_nms))

  param_descs <- enframe(c(
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
    paste(large_cohorts, "recruitment (millions)"),
    paste0("Unfished female spawning biomass (",
           latex_subscr(latex_italics("B"), "0"), ", kt)"),
    #"2009 relative spawning biomass",
    paste0(end_yr, " relative spawning biomass"),
    paste0(end_yr + 1, " relative spawning biomass"),
    paste0(end_yr, " rel. fishing intensity: ", rel_fishing_intensity_for_latex_table),
    paste0("Female spawning biomass at ", fspr_40_for_latex_table, "(", bspr_40_for_latex_table, ", kt)"),
    paste0("SPR at ", fspr_40_for_latex_table, " (kt)"),
    "Exploitation fraction corresponding to SPR",
    paste0("Yield at ",  bspr_40_for_latex_table, " (kt)")),
    name = NULL)

  if(inc_loglike){
    param_descs_nll <- enframe(
      c("Total",
        "Survey index",
        "Survey age compositions",
        "Fishery age compositions",
        "Recruitment",
        "Parameter priors",
        "Parameter deviations"),
      name = NULL)
    param_descs <- bind_rows(param_descs, param_descs_nll)
  }

  d |>
    mutate(parameter = param_descs$value)
}
