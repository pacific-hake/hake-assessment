#' Return a list of mcmc calculations, e.g. quantiles for various values
#'
#' @details
#' The values calculated in this function are used in the tables and plots in
#' the hake document. If a new value is needed, add that calculation here
#' and re-run the [create_rds_file()] function to re-build the RDS file
#' for the model.
#'
#' @param mcmc The output of the [r4ss::SSgetMCMC()] function as a data.frame
#' @param biomass_scale A scale factor to divide biomass values by
#' @param recr_scale A scale factor to divide recruitment values by
#' @param ... Absorbs arguments meant for other functions
#'
#' @return A named list of MCMC outputs
#' @export
calc_mcmc <- function(mcmc,
                      biomass_scale = 1e6,
                      recr_scale = 1e6,
                      ...){

  out <- list()
  mcmc <- mcmc |>
    as_tibble()

  # Natural mortality ----
  m_col <- "NatM_uniform_Fem_GP_1"
  if(!m_col %in% names(mcmc)){
    m_col <- "NatM_p_1_Fem_GP_1"
    if(!m_col %in% names(mcmc)){
      m_col <- NULL
    }
  }
  if(is.null(m_col)){
    out$m <- NA
  }else{
    out$m <- mcmc |>
      select({{m_col}}) |>
      unlist() |>
      quantile(probs)
  }

  # R0 ----
  # Find the posterior column name by matching the description which should
  # never change and then using that index to get the key posterior regular
  # expression to use to get the correct column from the posteriors data frame
  col_nm <- get_col_name_from_key_title(mcmc, "R\\[0\\]")
  col_nm_sym <- sym(col_nm)
  out$ro <- mcmc |>
    transmute(ro = !!col_nm_sym) |>
    mutate(ro = exp(ro) / 1e3) |>
    unlist() |>
    quantile(probs)

  # Steepness ----
  col_nm <- get_col_name_from_key_title(mcmc, "Steepness")
  out$steep <- NULL
  if(!is.null(col_nm)){
    col_nm_sym <- sym(col_nm)
    out$steep <- mcmc |>
      select(!!col_nm_sym) |>
      unlist() |>
      quantile(probs)
  }
  # Acoustic survey SD ----
  col_nm <- get_col_name_from_key_title(mcmc, "Survey extra SD")
  out$survey_sd <- NULL
  if(!is.null(col_nm)){
    col_nm_sym <- sym(col_nm)
    out$survey_sd <- mcmc |>
      select(!!col_nm_sym) |>
      unlist() |>
      quantile(probs)
  }
  # Age 1 index ----
  col_nm <- get_col_name_from_key_title(mcmc, "Age 1 extra SD")
  out$age1_index_sd <- NULL
  if(!is.null(col_nm)){
    col_nm_sym <- sym(col_nm)
    out$age1_index_sd <- mcmc |>
      select(!!col_nm_sym) |>
      unlist() |>
      quantile(probs)
  }
  # DM fishery parameter ----
  col_nm <- get_col_name_from_key_title(mcmc, "Dirichlet-multinomial fishery")
  out$dm_fishery <- NULL
  if(!is.null(col_nm)){
    col_nm_sym <- sym(col_nm)
    out$dm_fishery <- mcmc |>
      select(!!col_nm_sym) |>
      unlist() |>
      quantile(probs)
  }
  # DM survey parameter ----
  col_nm <- get_col_name_from_key_title(mcmc, "Dirichlet-multinomial survey")
  out$dm_survey <- NULL
  if(!is.null(col_nm)){
    col_nm_sym <- sym(col_nm)
    out$dm_survey <- mcmc |>
      select(!!col_nm_sym) |>
      unlist() |>
      quantile(probs)
  }
  # Spawning biomass ----
  ssb <- get_post_cols(mcmc, "SSB", biomass_scale)

  svirg <- get_post_cols(mcmc, "SSB_Virgin", biomass_scale, exact = TRUE)
  out$svirg <- svirg |> pull(1) |> quantile(probs, na.rm = TRUE)
  sinit <- get_post_cols(mcmc, "SSB_Initial", biomass_scale, exact = TRUE)
  out$sinit <- sinit |> pull(1) |> quantile(probs, na.rm = TRUE)

  out$slower <- apply(ssb, 2, quantile, prob = probs[1], na.rm = TRUE)
  out$smed <- apply(ssb, 2, quantile, prob = probs[2], na.rm = TRUE)
  out$supper <- apply(ssb, 2, quantile, prob = probs[3], na.rm = TRUE)

  # Depletion ----
  depl <- ssb |>
    bind_cols(sinit) |>
    mutate(across(-SSB_Initial, ~{.x / SSB_Initial})) |>
    select(-SSB_Initial)

  out$dlower <- apply(depl, 2, quantile, prob = probs[1], na.rm = TRUE)
  out$dmed <- apply(depl, 2, quantile, prob = probs[2], na.rm = TRUE)
  out$dupper <- apply(depl, 2, quantile, prob = probs[3], na.rm = TRUE)

  # Dynamic B0 biomass ----
  ssb_dyn <- get_post_cols(mcmc, "Dyn_Bzero", biomass_scale)

  out$dyn_slower <- apply(ssb_dyn, 2, quantile, prob = probs[1], na.rm = TRUE)
  out$dyn_smed <- apply(ssb_dyn, 2, quantile, prob = probs[2], na.rm = TRUE)
  out$dyn_supper <- apply(ssb_dyn, 2, quantile, prob = probs[3], na.rm = TRUE)
  rel_dyn_df <- map2_df(ssb, ssb_dyn, ~{
    .x / .y
  })

  out$rel_dyn_slower <- apply(rel_dyn_df, 2, quantile, prob = probs[1], na.rm = TRUE)
  out$rel_dyn_smed <- apply(rel_dyn_df, 2, quantile, prob = probs[2], na.rm = TRUE)
  out$rel_dyn_supper <- apply(rel_dyn_df, 2, quantile, prob = probs[3], na.rm = TRUE)

  # Recruitment ----
  recr <- get_post_cols(mcmc, "Recr", recr_scale)
  rvirg <- get_post_cols(mcmc, "Recr_Virgin", biomass_scale, TRUE)
  out$rvirg <- quantile(rvirg, probs,na.rm = TRUE)
  rinit <- get_post_cols(mcmc, "Recr_Initial", biomass_scale, TRUE)
  out$rinit <- quantile(rinit, probs,na.rm = TRUE)

  out$rlower <- apply(recr, 2, quantile,prob = probs[1], na.rm = TRUE)
  out$rmed <- apply(recr, 2, quantile, prob = probs[2], na.rm = TRUE)
  out$rupper <- apply(recr, 2, quantile,prob = probs[3], na.rm = TRUE)
  out$rmean <- apply(recr, 2, mean, na.rm = TRUE)

  # Relative recruitment ----
  # Divide all posterior value by the corresponding posterior value in
  # 2010
  rel_recr <- recr |> mutate(across(everything(), ~{.x / `2010`}))
  out$r_rel_lower <- apply(rel_recr, 2, quantile,prob = probs[1], na.rm = TRUE)
  out$r_rel_med <- apply(rel_recr, 2, quantile, prob = probs[2], na.rm = TRUE)
  out$r_rel_upper <- apply(rel_recr, 2, quantile,prob = probs[3], na.rm = TRUE)
  out$r_rel_mean <- apply(rel_recr, 2, mean, na.rm = TRUE)

  out$r_rel_init <- rinit |> bind_cols(recr |> select(`2010`)) |>
    mutate(Recr_Initial = Recr_Initial / `2010`) |>
    select(-`2010`) |>
    unlist() |>
    quantile(probs)

  # Recruitment deviations ----
  pat_early <- regex_recdev_early
  pat_ts <- regex_recdev_all

  # `recdev_early` are age columns
  recdev_early <- mcmc |>
    select(matches(pat_early)) %>%
    setNames(gsub(pat_early, "\\1", names(.)))
  # `recdev_ts` are year columns
  recdev_ts <-  mcmc |>
    select(matches(pat_ts)) %>%
    setNames(gsub(pat_ts, "\\2", names(.)))

  yr_init <- min(as.numeric(names(recdev_ts)))
  num_early <- ncol(recdev_early)
  names(recdev_early) <- seq(yr_init - num_early, yr_init - 1, 1)
  dev <- bind_cols(recdev_early, recdev_ts)

  out$devlower <- apply(dev, 2, quantile, prob = probs[1], na.rm = TRUE)
  out$devmed <- apply(dev, 2, quantile, prob = probs[2], na.rm = TRUE)
  out$devupper <- apply(dev, 2, quantile, prob = probs[3], na.rm = TRUE)

  # Spawning potential ratio ----
  spr <- get_post_cols(mcmc, "SPRratio")
  out$plower <- apply(spr, 2, quantile, prob = probs[1], na.rm = TRUE)
  out$pmed <- apply(spr, 2, quantile, prob = probs[2], na.rm = TRUE)
  out$pupper <- apply(spr, 2, quantile, prob = probs[3], na.rm = TRUE)

  # Fishing mortality ----
  f_mort <- get_post_cols(mcmc, "F")
  out$flower <- apply(f_mort, 2, quantile, prob = probs[1], na.rm = TRUE)
  out$fmed   <- apply(f_mort, 2, quantile, prob = probs[2], na.rm = TRUE)
  out$fupper <- apply(f_mort, 2, quantile, prob = probs[3], na.rm = TRUE)

  # The following are used for a single table, the reference points table.
  # They are formatted nicely here to avoid running that code during the
  # document build
  out$refpts <- list()
  out$refpts$unfish_fem_bio <- calc_refpt_quants(mcmc, "SSB_Virgin", 1e3, 0)
  out$refpts$unfish_recr <- calc_refpt_quants(mcmc, "Recr_Virgin", 1e3, 0)
  out$refpts$f_spawn_bio_bf40 <- calc_refpt_quants(mcmc, "SSB_SPR", 1e3, 0)
  out$refpts$spr_msy_proxy <- c(latex_bold("--"), "40\\%", latex_bold("--"))
  out$refpts$exp_frac_spr <- calc_refpt_quants(mcmc, "annF_SPR", 1, 1, TRUE)
  out$refpts$yield_bf40 <- calc_refpt_quants(mcmc, "Dead_Catch_SPR", 1e3, 0)
  out$refpts$fem_spawn_bio_b40 <- calc_refpt_quants(mcmc, "SSB_Btgt", 1e3, 0)
  out$refpts$spr_b40 <- calc_refpt_quants(mcmc, "SPR_Btgt", 1, 1, TRUE)
  out$refpts$exp_frac_b40 <- calc_refpt_quants(mcmc, "annF_Btgt", 1, 1, TRUE)
  out$refpts$yield_b40 <- calc_refpt_quants(mcmc, "Dead_Catch_Btgt", 1e3, 0)
  out$refpts$fem_spawn_bio_bmsy <- calc_refpt_quants(mcmc, "SSB_MSY", 1e3, 0)
  out$refpts$spr_msy <- calc_refpt_quants(mcmc, "SPR_MSY", 1, 1, TRUE)
  out$refpts$exp_frac_spr_msy <- calc_refpt_quants(mcmc, "annF_MSY", 1, 1, TRUE)
  out$refpts$msy <- calc_refpt_quants(mcmc, "Dead_Catch_MSY", 1e3, 0)

  out
}
