#' Create the data frame that holds parameter estimates for several models
#' for comparison purposes in a table
#'
#' @param models A list of models which contain the MCMC output
#' @param model_nms A vector of names of the same length as the number of
#' models in the models list
#' @param end_yr The last year to include
#' @param digits The number of decimal points to include in the table
#' @param ... Absorbs arguments meant for other functions
#'
#' @return A data frame containing 1 column of parameter estimates for each
#' model in `models` list
#' @export
get_param_est_comparison_df <- function(models,
                                        model_nms,
                                        end_yr = models[[1]]$endyr,
                                        digits = 3,
                                        ...){

  # MCMC parameter estimates
  d <- map2(models, model_nms, function(mdl, mdl_nm){

    q_acoustic <- NA
    q_age1 <- NA
    recs <- NA

    if(mdl$extra_mcmc_exists){
      recs <- map_dbl(hake::large_cohorts, \(rec_yr){
        mdl$extra_mcmc$recr_cohorts |>
          filter(yr == rec_yr) |>
          pull(`50%`)
      }) |>
        set_names(paste0("recs_", hake::large_cohorts))
      recs <- f(recs / 1e3)

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
        # `recs` is a named vector of all recruitment cohorts
        recs,
        bo = mdl$mcmccalcs$refpts$unfish_fem_bio[2],
        ssb_2009 =
          paste0(f(mdl$mcmccalcs$dmed["2009"] * 100, 1),
                 "\\%"),
        ssb_curr =
          ifelse(mdl$endyr <= end_yr - 2,
                 "--",
                 paste0(f(mdl$mcmccalcs$dmed[as.character(mdl$endyr + 1)] * 100,
                          1),
                        "\\%")),
        spr_last =
          ifelse(mdl$endyr <= end_yr - 2,
                 "--",
                 paste0(f(mdl$mcmccalcs$pmed[as.character(mdl$endyr)] * 100,
                          1),
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
    paste(hake::large_cohorts, "recruitment (millions)"),
    paste0("Unfished female spawning biomass (",
           latex_subscr(latex_italics("B"), "0"), ", kt)"),
    "2009 relative spawning biomass",
    paste0(end_yr, " relative spawning biomass"),
    paste0(end_yr - 1, " rel. fishing intensity: (1-SPR)/(1-",
           latex_subscr("SPR", "40\\%"), ")"),
    "Female spawning biomass at $\\FSPRfortynoit$ ($\\BSPRfortynoit$, kt)",
    "SPR at $\\FSPRfortynoit$ (kt)",
    "Exploitation fraction corresponding to SPR",
    "Yield at $\\BSPRfortynoit$ (kt)",
    "Total",
    "Survey index",
    "Survey age compositions",
    "Fishery age compositions",
    "Recruitment",
    "Parameter priors",
    "Parameter deviations"),
    name = NULL)

  d |>
    mutate(parameter = param_descs$value)
}