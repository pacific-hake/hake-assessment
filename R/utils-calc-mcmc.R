#' Return a list of mcmc calculations, e.g. quantiles for various values
#'
#' @details
#' The values calculated in this function are used in the tables and plots in
#' the hake document. If a new value is needed, add that calculation here
#' and re-run the [create_rds_file()] function to re-build the RDS file
#' for the model.
#'
#' @param mcmc The output of the [r4ss::SSgetMCMC()] function as a data.frame
#' @param probs A vector of 3 values, the lower CI, median, and upper CI
#' @param biomass_scale A scale factor to divide biomass values by
#' @param recr_scale A scale factor to divide recruitment values by
#' @param ... Absorb arguments meant for other functions
#'
#' @return A named list of MCMC outputs
#' @export
calc_mcmc <- function(mcmc,
                      probs = c(0.025, 0.5, 0.975),
                      biomass_scale = 1e6,
                      recr_scale = 1e6,
                      ...){

  # Extract time series columns for a given parameter and change the column
  # names to years. Divide all values by the biomass_scale
  # @param pat Pattern to match
  cols_par <- function(pat, scale = 1){
    mcmc |>
      select(matches(paste0("^", pat, "_[0-9]{4}$"))) %>%
      setNames(gsub(paste0("^", pat, "_([0-9]{4})$"), "\\1", names(.))) %>%
      mutate_all(~ . / scale)
  }

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
  if("SR_LN(R0)" %in% names(mcmc)){
    out$ro <- mcmc |>
      transmute(ro = `SR_LN(R0)`) |>
      mutate(ro = exp(ro) / 1e3) |>
      unlist() |>
      quantile(probs)
  }else{
    out$ro <- NA
  }

  # Steepness ----
  if("SR_BH_steep" %in% names(mcmc)){
    out$steep <- mcmc |>
      select("SR_BH_steep") |>
      unlist() |>
      quantile(probs)
  }else{
    out$steep <- NA
  }

  # Acoustic survey SD ----
  if("Q_extraSD_Acoustic_Survey(2)" %in% names(mcmc)){
    out$survey_sd <- mcmc |>
      select("Q_extraSD_Acoustic_Survey(2)") |>
      unlist() |>
      quantile(probs)
  }else{
    out$survey_sd <- NA
  }

  # Age 1 index ----
  if("Q_extraSD_Age1_Survey(3)" %in% names(mcmc)){
    out$age1_index_sd <- mcmc |>
      select("Q_extraSD_Age1_Survey(3)") |>
      unlist() |>
      quantile(probs)
  }else{
    out$age1_index_sd <- NA
  }
  # DM fishery parameter ----
  dm_col <- "ln(DM_theta)_Age_P1"
  if(!dm_col %in% names(mcmc)){
    dm_col <- "ln(DM_theta)_1"
    if(!dm_col %in% names(mcmc)){
      dm_col <- NULL
    }
  }
  if(is.null(dm_col)){
    out$dm_fishery <- NA
  }else{
    out$dm_fishery <- mcmc |>
      select({{dm_col}}) |>
      unlist() |>
      quantile(probs)
  }

  # DM survey parameter ----
  dm_col <- "ln(DM_theta)_Age_P2"
  if(!dm_col %in% names(mcmc)){
    dm_col <- "ln(DM_theta)_2"
    if(!dm_col %in% names(mcmc)){
      dm_col <- NULL
    }
  }
  if(is.null(dm_col)){
    out$dm_survey <- NA
  }else{
    out$dm_survey <- mcmc |>
      select({{dm_col}}) |>
      unlist() |>
      quantile(probs)
  }

  # Spawning biomass ----
  ssb <- cols_par("SSB", biomass_scale)

  # svirg <- mcmc |>
  #   select(matches("^SSB_Virgin$")) |>
  #   setNames("value") |>
  #   mutate(value = value / biomass_scale)
  sinit <- mcmc |>
    select(matches("^SSB_Initial$")) |>
    setNames("value") |>
    mutate(value = value / biomass_scale)
  # out$svirg <- svirg |>
  #   unlist() |>
  #   quantile(probs)
  out$sinit <- sinit |>
    unlist() |>
    quantile(probs)

  out$slower <- apply(ssb, 2, quantile, prob = probs[1], na.rm = TRUE)
  out$smed <- apply(ssb, 2, quantile, prob = probs[2], na.rm = TRUE)
  out$supper <- apply(ssb, 2, quantile, prob = probs[3], na.rm = TRUE)

  # Depletion ----
  depl <- apply(ssb, 2, function(x){x / sinit}) |>
    as.data.frame() |>
    as_tibble() |>
    setNames(names(ssb))
  out$dlower <- apply(depl, 2, quantile, prob = probs[1], na.rm = TRUE)
  out$dmed <- apply(depl, 2, quantile, prob = probs[2], na.rm = TRUE)
  out$dupper <- apply(depl, 2, quantile, prob = probs[3], na.rm = TRUE)

  # Recruitment ----
  recr <- cols_par("Recr", recr_scale)
  # rvirg <- mcmc |>
  #   select(matches("^Recr_Virgin$")) |>
  #   setNames("value")
  rinit <- mcmc |>
    select(matches("^Recr_Initial$")) |>
    setNames("value")
  runfished <- mcmc |>
    select(matches("^Recr_unfished$")) |>
    setNames("value")
  # out$rvirg <- rvirg |>
  #   unlist() |>
  #   quantile(probs)
  out$rinit <- rinit |>
    unlist() |>
    quantile(probs)
  out$runfished <- runfished |>
    unlist() |>
    quantile(probs)

  out$rlower <- apply(recr, 2, quantile,prob = probs[1], na.rm = TRUE)
  out$rmed <- apply(recr, 2, quantile, prob = probs[2], na.rm = TRUE)
  out$rupper <- apply(recr, 2, quantile,prob = probs[3], na.rm = TRUE)
  out$rmean <- apply(recr, 2, mean, na.rm = TRUE)

  # Recruitment deviations ----
  pat_early <- "^Early_InitAge_([0-9]{1,2})$"
  pat_ts <- "^(Early_RecrDev_|Main_RecrDev_|Late_RecrDev_)([0-9]{4})$"

  recdev_early <- mcmc |>
    select(matches(pat_early)) %>%
    setNames(gsub(pat_early, "\\1", names(.)))
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
  spr <- cols_par("SPRratio")
  out$plower <- apply(spr, 2, quantile, prob = probs[1], na.rm = TRUE)
  out$pmed <- apply(spr, 2, quantile, prob = probs[2], na.rm = TRUE)
  out$pupper <- apply(spr, 2, quantile, prob = probs[3], na.rm = TRUE)

  # Fishing mortality ----
  f_mort <- cols_par("F")
  out$flower <- apply(f_mort, 2, quantile, prob = probs[1], na.rm = TRUE)
  out$fmed   <- apply(f_mort, 2, quantile, prob = probs[2], na.rm = TRUE)
  out$fupper <- apply(f_mort, 2, quantile, prob = probs[3], na.rm = TRUE)

  # Calculations for the reference points table
  refpt_quants <- function(param, scale = 1, digits = 0, perc = FALSE){
    vec <- mcmc |>
      select(matches(paste0("^", param,"$"))) |>
      pull()
    vec <- vec / scale
    tmp <- f(ifelse(perc, 100, 1) *
        quantile(vec, probs = probs),
      dec.points = digits)
    if(perc){
      return(paste0(tmp, "\\%"))
    }
    tmp
  }

  # The following are used for a single table, the reference points table.
  # They are formatted nicely here to avoid running that code during the
  # document build
  out$refpts <- list()
  out$refpts$unfish_fem_bio <- refpt_quants("SSB_Virgin", 1e3, 0)
  out$refpts$unfish_recr <- refpt_quants("Recr_Virgin", 1e3, 0)
  out$refpts$f_spawn_bio_bf40 <- refpt_quants("SSB_SPR", 1e3, 0)
  out$refpts$spr_msy_proxy <- c(latex_bold("--"), "40\\%", latex_bold("--"))
  out$refpts$exp_frac_spr <- refpt_quants("annF_SPR", 1, 1, TRUE)
  out$refpts$yield_bf40 <- refpt_quants("Dead_Catch_SPR", 1e3, 0)
  out$refpts$fem_spawn_bio_b40 <- refpt_quants("SSB_Btgt", 1e3, 0)
  out$refpts$spr_b40 <- refpt_quants("SPR_Btgt", 1, 1, TRUE)
  out$refpts$exp_frac_b40 <- refpt_quants("annF_Btgt", 1, 1, TRUE)
  out$refpts$yield_b40 <- refpt_quants("Dead_Catch_Btgt", 1e3, 0)
  out$refpts$fem_spawn_bio_bmsy <- refpt_quants("SSB_MSY", 1e3, 0)
  out$refpts$spr_msy <- refpt_quants("SPR_MSY", 1, 1, TRUE)
  out$refpts$exp_frac_spr_msy <- refpt_quants("annF_MSY", 1, 1, TRUE)
  out$refpts$msy <- refpt_quants("Dead_Catch_MSY", 1e3, 0)

  out
}
