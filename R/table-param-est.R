#' Returns an [xtable::xtable()] in the proper format for the MCMC posterior
#' parameter estimates for all the models in the list, one for each column
#'
#' @param models A list of models which contain the MCMC output
#' @param model_nms A vector of names of the same length as the number of
#' models in the models list
#' @param end_yr The last year to include
#' @param digits Number of decimal points for the estimates
#' @param xcaption Caption to appear in the calling document
#' @param xlabel The label used to reference the table in latex
#' @param font_size Size of the font for the table
#' @param space_size Size of the vertical spaces for the table
#' @param rec_yrs A vector of integers supplying the years for which you
#' want estimates of recruitment
#' @param show_like If `TRUE`, include the negative log-likelihoods (set to
#' `FALSE` for presentations)
#'
#' @return as [xtable::xtable()]
#' @export
#'
table_param_est <- function(models,
                            model_nms,
                            end_yr,
                            digits = 3,
                            xcaption = "default",
                            xlabel = "default",
                            font_size = 9,
                            space_size = 10,
                            rec_yrs = NA,
                            show_like = TRUE){

  if(is.na(rec_yrs[1])){
    stop("No cohort recruitment years supplied: `rec_yrs`",
         call. = FALSE)
  }

  # MCMC parameter estimates
  tab <- map2(models, model_nms, function(mdl, mdl_nm){
    recs <- map_dbl(rec_yrs, function(rec_yr){
      median(mdl$mcmc[[paste0("Recr_", rec_yr)]]) / 1e3
      })
    names(recs) <- paste0("recr_", rec_yrs)
    recs <- f(recs, 0)

    # Catchabilities
    if(mdl$extra_mcmc_exists){
      q_med <- mdl$extra.mcmc$q.med
      # Filter for last year in the series, since q's are not time varying
      q_acoustic <- q_med |>
        filter(Fleet == 2)
      if(nrow(q_acoustic)){
        q_acoustic <- q_acoustic %>%
          `[`("value") |>
          tail(1) |>
          unlist()
      }else{
        q_acoustic <- NA
      }
      q_age1 <- q_med |>
        filter(Fleet == 3)
      if(nrow(q_age1)){
        q_age1 <- q_age1 %>%
          `[`("value") |>
          tail(1) |>
          unlist()
      }else{
        q_age1 <- NA
      }
    }else{
      q_acoustic <- NA
      q_age1 <- NA
    }

    df <- enframe(
      c(nat_m =
          ifelse("NatM_uniform_Fem_GP_1" %in% names(mdl$mcmc),
                 f(median(mdl$mcmc$`NatM_uniform_Fem_GP_1`), digits),
                 f(median(mdl$mcmc$`NatM_p_1_Fem_GP_1`), digits)),
        ro = f(exp(median(mdl$mcmc$`SR_LN(R0)`)) / 1e3),
        h = ifelse(is.null(mdl$mcmc$`SR_BH_steep`),
                   NA,
                   f(median(mdl$mcmc$`SR_BH_steep`),
                     digits)),

        survey_sd = f(median(mdl$mcmc$`Q_extraSD_Acoustic_Survey(2)`), digits),
        catchability = ifelse(is.na(q_acoustic), NA, f(q_acoustic, digits)),

        survey_age1_sd = ifelse(is.null(mdl$mcmc$`Q_extraSD_Age1_Survey(3)`),
                                NA,
                                f(median(mdl$mcmc$`Q_extraSD_Age1_Survey(3)`), digits)),
        catchability_age1 = ifelse(all(is.na(q_age1)),
                                   NA,
                                   f(q_age1, digits)),

        dm_fishery = ifelse(is.null(mdl$mcmc$`ln(DM_theta)_Age_P1`),
                            ifelse(is.null(mdl$mcmc$`ln(DM_theta)_1`),
                                   NA,
                                   f(median(mdl$mcmc$`ln(DM_theta)_1`), digits)),
                            f(median(mdl$mcmc$`ln(DM_theta)_Age_P1`), digits)),
        dm_survey = ifelse(is.null(mdl$mcmc$`ln(DM_theta)_Age_P2`),
                           ifelse(is.null(mdl$mcmc$`ln(DM_theta)_2`),
                                  NA,
                                  f(median(mdl$mcmc$`ln(DM_theta)_2`), digits)),
                           f(median(mdl$mcmc$`ln(DM_theta)_Age_P2`), digits)),
        recs,
        bo = f(median(mdl$mcmc$`SSB_Initial`) / 1e3),
        ssb_2009 = paste0(f(median(mdl$mcmc$`SSB_2009` /
                                     mdl$mcmc$SSB_Initial) * 100, 1),
                          "\\%"),
        ssb_curr = ifelse(mdl$endyr <= end_yr - 2,
                          "--",
                          paste0(f(median(mdl$mcmc[[paste0("SSB_", end_yr)]] /
                                            mdl$mcmc$SSB_Initial) * 100, 1), "\\%")),
        spr_last = ifelse(mdl$endyr <= end_yr - 2,
                          "--",
                          paste0(f(median(mdl$mcmc[[paste0("SPRratio_", end_yr - 1)]]) * 100, 1), "\\%")),
        ssb_curr_fem = f(median(mdl$mcmc$`SSB_SPR`) / 1e3, 0),
        spr_msy = "40.0\\%",
        exp_frac = mdl$mcmccalcs$exp.frac.spr[2],
        yield_f40 = f(median(mdl$mcmc$`Dead_Catch_SPR`) / 1e3, 0),
        total_like = f(filter(mdl$likelihoods_used,
                              rownames(mdl$likelihoods_used) == "TOTAL")$values, 2),
        survey_like = f(filter(mdl$likelihoods_used,
                               rownames(mdl$likelihoods_used) == "Survey")$values, 2),
        fishery_age_like = f(filter(mdl$likelihoods_by_fleet, Label == "Age_like")$Fishery, 2),
        survey_age_like = f(filter(mdl$likelihoods_by_fleet, Label == "Age_like")$Acoustic_Survey, 2),
        recr_like = f(filter(mdl$likelihoods_used,
                             rownames(mdl$likelihoods_used) == "Recruitment")$values, 2),
        priors_like = f(filter(mdl$likelihoods_used,
                               rownames(mdl$likelihoods_used) == "Parm_priors")$values, 2),
        parmdev_like = f(filter(mdl$likelihoods_used,
                                rownames(mdl$likelihoods_used) == "Parm_devs")$values, 2)),
      value = mdl_nm)
  })

  # Remove parameter name column from all but first model then bind them all together,
  # make a variable that records if there are any age-1 index parameter values,
  # replace NAs with double-dashes, and add a blank row at the top for aesthetic purposes
  tab[-1] <- map(tab[-1], function(mdl_tab){
    mdl_tab[, 2]
  })
  tab <- tab |>
    bind_cols()
  age_1 <- !(tab |>
               filter(name == "survey_age1_sd") |>
               select(-name) |>
               is.na() |>
               all())
  tab[is.na(tab)] <- "--"
  tab <- rbind(rep("", length(models) + 1), tab)

  tab_labels <- enframe(
    c("", # For blank line at top
      paste0("Natural mortality (", latex.italics("M"), ")"),
      paste0("Unfished recruitment (", latex.subscr(latex.italics("R"), "0"), ", millions)"),
      paste0("Steepness (", latex.italics("h"), ")"),
      "Additional biomass index SD",
      paste0("Catchability: biomass index (", latex.italics("$q_b$"), ")"),
      "Additional age-1 index SD",
      paste0("Catchability: age-1 index (", latex.italics("$q_1$"), ")"),
      "Dirichlet-multinomial fishery (log~$\\theta_{\\text{fish}}$)",
      "Dirichlet-multinomial survey (log~$\\theta_{\\text{surv}}$)",
      paste(rec_yrs, "recruitment (millions)"),
      paste0("Unfished female spawning biomass (",
             latex.subscr(latex.italics("B"), "0"), ", thousand t)"),
      "2009 relative spawning biomass",
      paste0(end_yr, " relative spawning biomass"),
      paste0(end_yr - 1, " rel. fishing intensity: (1-SPR)/(1-",
             latex.subscr("SPR", "40\\%"), ")"),
      paste0("Female spawning biomass at ",
             latex.subscr(latex.italics("F"), "SPR=40\\%"),
             " (",
             latex.subscr(latex.italics("B"), "SPR=40\\%"), ", thousand t)"),
      paste0("SPR at ", latex.subscr(latex.italics("F"), "SPR=40\\%")), "Exploitation fraction corresponding to SPR",
      paste0("Yield at ", latex.subscr(latex.italics("B"), "SPR=40\\%"), " (thousand t)")),
    name = NULL)
  if(!age_1){
    tab_labels <- tab_labels |>
      filter(value != "Additional age-1 index SD")
    tab <- tab |>
      filter(name != "survey_age1_sd")
  }

  if(show_like){
    tab_labels <- rbind(tab_labels,
                        "Total",
                        "Survey",
                        "Survey age compositions",
                        "Fishery age compositions",
                        "Recruitment",
                        "Parameter priors",
                        "Parameter deviations")
  }else{
    tab <- head(tab, -7)
  }

  # Add the labels and remove the 'name' column which is just the short form of the name.
  # If debugging, leave 'name' in here and compare side-by-side with the labels created
  tab <- bind_cols(tab_labels, tab) |>
    select(-name)

  # Split up the headers (model names) by words and let them stack on
  # top of each other to reduce width of table
  model_nms_str <- map_chr(model_nms, function(model_nm){
    latex.mlc(gsub(" ", "\\\\\\\\", model_nm), make.bold = FALSE)
    })
  colnames(tab) <- c("", model_nms_str)

  # Add sub-headers for different parameter types
  addtorow <- list()
  addtorow$pos <- list()
  addtorow$pos[[1]] <- 1
  addtorow$pos[[2]] <- ifelse(age_1,
                              10,
                              9)
  addtorow$pos[[3]] <- ifelse(age_1,
                              14 + length(rec_yrs),
                              13 + length(rec_yrs))
  addtorow$command <-
    c(paste0(latex.bold(latex.under("Parameters")),
             latex.nline),
      paste0(latex.nline,
             latex.bold(latex.under("Derived Quantities")),
             latex.nline),
      paste0(latex.nline,
             latex.bold(latex.under("Reference Points based on $\\Fforty$")),
             latex.nline))
  if(show_like){
    addtorow$pos[[4]] <- ifelse(age_1, 21, 20)
    addtorow$command <- c(addtorow$command,
                          paste0(latex.nline,
                                 latex.bold(latex.under("Negative log likelihoods")),
                                 latex.nline))
  }

  # Make the size string for font and space size
  size.string <- latex.size.str(font_size, space_size)
  print(xtable(tab,
               caption = xcaption,
               label = xlabel,
               align = get.align(ncol(tab),
                                 just = "c")),
        caption.placement = "top",
        include.rownames = FALSE,
        sanitize.text.function = function(x){x},
        size = size.string,
        add.to.row = addtorow,
        tabular.environment = "tabular",
        table.placement = "H")
}
