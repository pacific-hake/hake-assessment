#' Return a list of mcmc calculations, e.g. quantiles for various values
#'
#' @param mcmc The output of the [r4ss::SSgetMCMC()] function as a data.frame
#' @param lower Lower quantile value
#' @param upper Upper quantile value
#' @param recr_scale Scale the recruitment by this amount. The default
#' is 1e6 because recruitment will be shown in millions of tonnes
#'
#' @return A named list of MCMC outputs
#' @export
calc_mcmc <- function(mcmc,
                      probs = c(0.025, 0.5, 0.975),
                      biomass_scale = 1e3,
                      recr_scale = 1e6){

  cols_par <- function(pat){
    mcmc |>
      select(matches(paste0("^", pat, "_[0-9]{4}$"))) %>%
      setNames(gsub(paste0("^", pat, "_([0-9]{4})$"), "\\1", names(.))) %>%
      mutate_all(~ . / biomass_scale)
  }

  out <- list()
  mcmc <- mcmc |>
    as_tibble()

  # Spawning biomass ----
  ssb <- cols_par("SSB")
  svirg <- mcmc |>
    select(matches("^SSB_Virgin$")) |>
    setNames("value")
  sinit <- mcmc |>
    select(matches("^SSB_Initial$")) |>
    setNames("value")
  out$svirg <- svirg |>
    unlist() |>
    quantile(probs)
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
  recr <- cols_par("Recr")
  rvirg <- mcmc |>
    select(matches("^Recr_Virgin$")) |>
    setNames("value")
  rinit <- mcmc |>
    select(matches("^Recr_Initial$")) |>
    setNames("value")
  runfished <- mcmc |>
    select(matches("^Recr_unfished$")) |>
    setNames("value")
  out$rvirg <- rvirg |>
    unlist() |>
    quantile(probs)
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
  pat_ts <- "^(Early_RecrDev_|Main_RecrDev_)([0-9]{4})$"

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
  out$plower <- apply(spr, 2, quantile, prob = probs[1], na.rm = TRUE) *
    biomass_scale
  out$pmed <- apply(spr, 2, quantile, prob = probs[2], na.rm = TRUE) *
    biomass_scale
  out$pupper <- apply(spr, 2, quantile, prob = probs[3], na.rm = TRUE) *
    biomass_scale

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
  out$unfish_fem_bio <- refpt_quants("SSB_Virgin", biomass_scale, 0)
  out$unfish_recr <- refpt_quants("Recr_Virgin", biomass_scale, 0)
  out$f_spawn_bio_bf40 <- refpt_quants("SSB_SPR", biomass_scale, 0)
  out$spr_msy_proxy <- c(latex_bold("--"), "40\\%", latex_bold("--"))
  out$exp_frac_spr <- refpt_quants("annF_SPR", 1, 1, TRUE)
  out$yield_bf40 <- refpt_quants("Dead_Catch_SPR", biomass_scale, 0)
  out$fem_spawn_bio_b40 <- refpt_quants("SSB_Btgt", biomass_scale, 0)
  out$spr_b40 <- refpt_quants("SPR_Btgt", 1, 1, TRUE)
  out$exp_frac_b40 <- refpt_quants("annF_Btgt", 1, 1, TRUE)
  out$yield_b40 <- refpt_quants("Dead_Catch_Btgt", 1000, 0)
  out$fem_spawn_bio_bmsy <- refpt_quants("SSB_MSY", 1000, 0)
  out$spr_msy <- refpt_quants("SPR_MSY", 1, 1, TRUE)
  out$exp_frac_sprmsy <- refpt_quants("annF_MSY", 1, 1, TRUE)
  out$msy <- refpt_quants("Dead_Catch_MSY", 1000, 0)

  out
}
