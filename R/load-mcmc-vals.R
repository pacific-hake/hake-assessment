#' Load some values from the MCMC output and store in a list
#'
#' @param model A model list as created by [create_rds_file()]
#' @param assess_yr The current assessment year
#' @param probs A vector of 3 values, the lower CI, median, and upper CI
#'
#' @return A list of values extracted from the MCMC data frame of a model
#' @export
load_mcmc_vals <- function(model,
                           assess_yr,
                           probs = c(0.025, 0.5, 0.975)){

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
  out$prob_percent_2014_rec_gt_2016_rec <-
    f(mean(model$mcmc$Recr_2014 > model$mcmc$Recr_2016) * 100, 0)
  out$prob_percent_2016_rec_gt_2010_rec <-
    f(mean(model$mcmc$Recr_2016 > model$mcmc$Recr_2010) * 100, 1)
  out$prob_percent_2010_rec_gt_1980_rec <-
    f(mean(model$mcmc$Recr_2010 > model$mcmc$Recr_1980) * 100, 0)

  out$sigma_r_alt_allyr <- calc_sd_of_devs(model$mcmc,
                                           pattern = "^[EML].+_RecrDev")
  out$sigma_r_this_year_main <- calc_sd_of_devs(model$mcmc)

  # MCMC quantiles for the fishery DM parameter
  pat <- "^.*\\(DM_theta\\)_(Age_P)?1$"
  col_effn <- grep(pat, colnames(model$mcmc))
  if(!length(col_effn)){
    stop("Fishery DM parameter not found in `model$mcmc` using regular ",
         "expression ", pat,
         call. = FALSE)
  }
  if(length(col_effn) > 1){
    stop("More than one fishery DM parameter found in `model$mcmc` using ",
         "regular expression ", pat,
         call. = FALSE)
  }
  effn <- model$mcmc |>
    pull(col_effn)
  out$log_theta_fishery_median <- median(effn)
  out$log_theta_fishery_lower <- quantile(effn, probs = probs[1])
  out$log_theta_fishery_upper <- quantile(effn, probs = probs[3])
  out$dm_weight_fishery_median <- median(exp(effn) / (1 + exp(effn)))
  out$dm_weight_fishery_lower <- exp(out$log_theta_fishery_lower) /
                                       (1 + exp(out$log_theta_fishery_lower))
  out$dm_weight_fishery_upper <- exp(out$log_theta_fishery_upper) /
                                       (1 + exp(out$log_theta_fishery_upper))

  # MCMC quantiles for the survey DM parameter
  pat <- "^.*\\(DM_theta\\)_(Age_P)?2$"
  col_effn <- grep(pat, colnames(model$mcmc))
  if(!length(col_effn)){
    stop("Survey DM parameter not found in `model$mcmc` using regular ",
         "expression ", pat,
         call. = FALSE)
  }
  if(length(col_effn) > 1){
    stop("More than one survey DM parameter found in `model$mcmc` using ",
         "regular expression ", pat,
         call. = FALSE)
  }
  effn <- model$mcmc |>
    pull(col_effn)
  out$log_theta_survey_median <- median(effn)
  out$log_theta_survey_lower <- quantile(effn, probs = probs[1])
  out$log_theta_survey_upper <- quantile(effn, probs = probs[3])
  out$dm_weight_survey_median <- median(exp(effn) / (1 + exp(effn)))
  out$dm_weight_survey_lower <- exp(out$log_theta_survey_lower) /
                                      (1 + exp(out$log_theta_survey_lower))
  out$dm_weight_survey_upper <- exp(out$log_theta_survey_upper) /
                                      (1 + exp(out$log_theta_survey_upper))

  # MCMC parameter estimates for model ----------------------------------------
  # ... natural mortality -----------------------------------------------------
  out$nat_m <- quantile(model$mcmc$NatM_uniform_Fem_GP_1, probs = probs)
  # ... steepness -------------------------------------------------------------
  out$steep <- quantile(model$mcmc$SR_BH_steep, probs = probs)
  out$bratio_curr <- quantile(model$mcmc[[paste0("Bratio_", assess_yr)]],
                              probs = probs)
  out$bratio_age1 <- quantile(model$mcmc[[paste0("Bratio_", assess_yr)]],
                              probs = probs)

  out$joint_percent_prob_above_below <-
    f(sum(model$mcmc[[paste0("Bratio_", assess_yr)]] < 0.4 &
            model$mcmc[[paste0("SPRratio_", assess_yr - 1)]] > 1) /
        nrow(model$mcmc) * 100,
      1)

  # Probabilities for historical performance analyses -------------------------
  assess_history_probs_df <-
    read_csv(here::here("inst",
                        "extdata",
                        "data",
                        "assessment-history-probs.csv"),
             col_types = cols(),
             comment = "#",
             show_col_types = FALSE)

  out$historical_probs_df <-
    combine_historical_probs(model = model,
                             hist_probs = assess_history_probs_df,
                             end = assess_yr - 1) |>
    as_tibble()


  out
}