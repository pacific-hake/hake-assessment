#' Load some values from the MCMC output and store in a list
#'
#' @details
#' Uses the [probs] vector which is included in the package data for
#' this package
#'
#' @param model A model list as created by [create_rds_file()]
#' @param assess_yr The current assessment year
#'
#' @return A list of values extracted from the MCMC data frame of a model
#' @export
load_mcmc_vals <- function(model,
                           assess_yr){

  out <- list()
  out$probs_curr_b40 <-
    f(mean(model$mcmc[[paste0("Bratio_",
                              assess_yr)]] > 0.40) * 100,
      1)
  out$probs_curr_b25 <-
    f(mean(model$mcmc[[paste0("Bratio_",
                              assess_yr)]] > 0.25) * 100,
      1)
  out$probs_curr_b10 <-
    f(mean(model$mcmc[[paste0("Bratio_",
                              assess_yr)]] > 0.10) * 100,
      0)
  out$probs_curr_below_b40 <-
    f(mean(model$mcmc[[paste0("Bratio_",
                              assess_yr)]] < 0.40) * 100,
      1)
  out$probs_curr_below_b25 <-
    f(mean(model$mcmc[[paste0("Bratio_",
                              assess_yr)]] < 0.25) * 100,
      1)

  out$ct_limit_quantiles <-
    f(as.numeric(quantile(model$mcmc[[paste0("ForeCatch_",
                                             assess_yr)]],
                          probs = probs)))
  names(out$ct_limit_quantiles) <- c("lower", "median", "upper")

  out$num_mcmc_samples <- dim(model$mcmc)[1]

  out$probs_curr_rel_fish_intens_above_1 <-
    f(sum(model$mcmc[[paste0("SPRratio_", assess_yr - 1)]] > 1) /
        nrow(model$mcmc) * 100,
      1)

  out$prob_percent_2014_rec_gt_2010_rec <-
    f(mean(model$mcmc$Recr_2014 > model$mcmc$Recr_2010) * 100, 0)
  out$prob_percent_2016_rec_gt_2010_rec <-
    f(mean(model$mcmc$Recr_2016 > model$mcmc$Recr_2010) * 100, 1)
  out$prob_percent_2020_rec_gt_2010_rec <-
    f(mean(model$mcmc$Recr_2020 > model$mcmc$Recr_2010) * 100, 0)
  out$prob_percent_2021_rec_gt_2010_rec <-
    f(mean(model$mcmc$Recr_2021 > model$mcmc$Recr_2010) * 100, 0)
  out$prob_percent_2022_rec_gt_2010_rec <-
    f(mean(model$mcmc$Recr_2022 > model$mcmc$Recr_2010) * 100, 0)
  out$prob_percent_2023_rec_gt_2010_rec <-
    f(mean(model$mcmc$Recr_2023 > model$mcmc$Recr_2010) * 100, 0)
  out$prob_percent_2024_rec_gt_2010_rec <-
    f(mean(model$mcmc$Recr_2024 > model$mcmc$Recr_2010) * 100, 0)

  out$prob_percent_2014_rec_gt_2016_rec <-
    f(mean(model$mcmc$Recr_2014 > model$mcmc$Recr_2016) * 100, 0)
  out$prob_percent_2016_rec_gt_2010_rec <-
    f(mean(model$mcmc$Recr_2016 > model$mcmc$Recr_2010) * 100, 1)
  out$prob_percent_2010_rec_gt_1980_rec <-
    f(mean(model$mcmc$Recr_2010 > model$mcmc$Recr_1980) * 100, 0)

  out$sigma_r_alt_allyr <- model$mcmc |>
    select(matches(regex_eml_recdevs)) |>
    apply(1, FUN = sd) |>
    median() |>
    f(2)

  out$sigma_r_this_year_main <-  model$mcmc |>
    select(matches(regex_main_recdevs)) |>
    apply(1, FUN = sd) |>
    median() |>
    f(2)

  # MCMC quantiles for the fishery DM parameter
  col_effn <- get_col_name_from_key_title(
    model$mcmc,
    pat = "Dirichlet-multinomial fishery")
  if(length(col_effn)){
    col_effn_sym <- sym(col_effn)
    effn <- model$mcmc |>
      pull(!!col_effn_sym)
    out$log_theta_fishery_median <- median(effn)
    out$log_theta_fishery_lower <- quantile(effn, probs = probs[1])
    out$log_theta_fishery_upper <- quantile(effn, probs = probs[3])
    out$dm_weight_fishery_median <- median(exp(effn) / (1 + exp(effn)))
    out$dm_weight_fishery_lower <- exp(out$log_theta_fishery_lower) /
      (1 + exp(out$log_theta_fishery_lower))
    out$dm_weight_fishery_upper <- exp(out$log_theta_fishery_upper) /
      (1 + exp(out$log_theta_fishery_upper))
  }else{
    out$log_theta_fishery_median <- NA
    out$log_theta_fishery_lower <- NA
    out$log_theta_fishery_upper <- NA
    out$dm_weight_fishery_median <- NA
    out$dm_weight_fishery_lower <- NA
    out$dm_weight_fishery_upper <- NA
  }

  # MCMC quantiles for the survey DM parameter
  col_effn <- get_col_name_from_key_title(
    model$mcmc,
    pat = "Dirichlet-multinomial survey")
  if(length(col_effn)){
    col_effn_sym <- sym(col_effn)
    effn <- model$mcmc |>
      pull(!!col_effn_sym)
    out$log_theta_survey_median <- median(effn)
    out$log_theta_survey_lower <- quantile(effn, probs = probs[1])
    out$log_theta_survey_upper <- quantile(effn, probs = probs[3])
    out$dm_weight_survey_median <- median(exp(effn) / (1 + exp(effn)))
    out$dm_weight_survey_lower <- exp(out$log_theta_survey_lower) /
      (1 + exp(out$log_theta_survey_lower))
    out$dm_weight_survey_upper <- exp(out$log_theta_survey_upper) /
      (1 + exp(out$log_theta_survey_upper))
  }else{
    out$log_theta_survey_median <- NA
    out$log_theta_survey_lower <- NA
    out$log_theta_survey_upper <- NA
    out$dm_weight_survey_median <- NA
    out$dm_weight_survey_lower <- NA
    out$dm_weight_survey_upper <- NA
  }

  # MCMC parameter estimates for model ----------------------------------------
  # ... natural mortality -----------------------------------------------------
  nat_mort <- get_col_name_from_key_title(
    model$mcmc,
    pat = "Natural mortality")

  if(length(nat_mort)){
    nat_mort_sym <- sym(nat_mort)
    nat_mort<- model$mcmc |>
      pull(!!nat_mort_sym)
    out$nat_m <- quantile(nat_mort, probs = probs)
  }else{
    out$nat_m <- NA
  }

  # ... steepness -------------------------------------------------------------
  val <- get_col_name_from_key_title(
    model$mcmc,
    pat = "Steepness")

  if(length(val)){
    val_sym <- sym(val)
    steepness <- model$mcmc |>
      pull(!!val)

    out$steep <- quantile(steepness, probs = probs)
  }else{
    out$steep <- NA
  }

  # ... Bratio and SPR---------------------------------------------------------
  bratio_label <- paste0("Bratio_", assess_yr)
  spr_label <- paste0("SPRratio_", assess_yr - 1)
  val <- model$mcmc |>
    pull(bratio_label)

  if(length(val)){
    out$bratio_curr <- quantile(val, probs = probs)
    out$bratio_age1 <- quantile(val, probs = probs)
  }else{
    out$bratio_curr <- NA
    out$bratio_age1 <- NA
  }

  out$joint_percent_prob_above_below <-
    f(sum(model$mcmc[[bratio_label]] < 0.4 &
            model$mcmc[[spr_label]] > 1) /
        nrow(model$mcmc) * 100,
      1)

  # Probabilities for historical performance analyses -------------------------
  # Needs assess_history_probs_df updated with assess_yr-1 assessment values
    out$historical_probs_df <-
    combine_historical_probs(model = model,
                             end = assess_yr - 1)

  # Random values relying directly on MCMC posteriors in the document text
  # These will be `NULL` for retrospectives going back too far, in that case,
  # make it `NULL`
  out$rec_cor_2014_2016 <- NULL
  if(!is.null(model$mcmc$Main_RecrDev_2014) &&
     !is.null(model$mcmc$Main_RecrDev_2016)){
    out$rec_cor_2014_2016 <- cor(model$mcmc$Main_RecrDev_2014,
                                 model$mcmc$Main_RecrDev_2016)
  }

  out
}
