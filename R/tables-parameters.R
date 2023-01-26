#' Returns an [xtable::xtable()] in the proper format for the MCMC posterior parameter estimates for
#' all the models in the list, one for each column
#'
#' @param models A list of models which contain the MCMC output
#' @param model.names A vector of names of the same length as the number of models in the models list
#' @param end_yr The last year to include
#' @param digits Number of decimal points for the estimates
#' @param xcaption Caption to appear in the calling document
#' @param xlabel The label used to reference the table in latex
#' @param font.size Size of the font for the table
#' @param space.size Size of the vertical spaces for the table
#' @param getrecs A vector of integers supplying the years for which you want estimates of recruitment
#' @param mle_table If TRUE, prepend the MLE values from the first model in the list as a column in the table
#' @param show_like If `TRUE`, include the negative log-likelihoods (set to `FALSE` for presentations)
#'
#' @return as [xtable::xtable()]
#' @export
#'
param_est_table <- function(models,
                            model.names,
                            end_yr,
                            digits = 3,
                            xcaption = "default",
                            xlabel   = "default",
                            font.size = 9,
                            space.size = 10,
                            getrecs = NA,
                            mle_table = FALSE,
                            show_like = TRUE){

  if(is.na(getrecs[1])){
    stop("No cohort recruitment years supplied: `getrecs`", call. = FALSE)
  }

  # MCMC parameter estimates
  tab <- map2(models, model.names, ~{
    recs <- map_dbl(getrecs, ~{median(.y$mcmc[[paste0("Recr_", .x)]]) / 1e3}, .y = .x)
    names(recs) <- paste0("recr_", getrecs)
    recs <- f(recs, 0)

    # Catchabilities
    if(.x$endyr == 2020){
      q_acoustic <- median(.x$extra.mcmc$Q_vector)
      q_age1 <- NA
    }else if(.x$extra_mcmc_exists){
      q_med <- .x$extra.mcmc$q.med
      # Filter for last year in the series, since q's are not time varying
      q_acoustic <- q_med %>% filter(Fleet == 2)
      if(nrow(q_acoustic)){
        q_acoustic <- q_acoustic %>% `[`("value") %>% tail(1) %>% unlist
      }else{
        q_acoustic <- NA
      }
      q_age1 <- q_med %>% filter(Fleet == 3)
      if(nrow(q_age1)){
        q_age1 <- q_age1 %>% `[`("value") %>% tail(1) %>% unlist
      }else{
        q_age1 <- NA
      }
    }else{
      q_acoustic <- NA
      q_age1 <- NA
    }

    df <- enframe(
      c(nat_m =
          ifelse("NatM_uniform_Fem_GP_1" %in% names(.x$mcmc),
                 f(median(.x$mcmc$`NatM_uniform_Fem_GP_1`), digits),
                 f(median(.x$mcmc$`NatM_p_1_Fem_GP_1`), digits)),
        ro = f((median(.x$mcmc$`SR_LN(R0)`) %>% exp) / 1e3, 0),
        h = ifelse(is.null(.x$mcmc$`SR_BH_steep`), NA, f(median(.x$mcmc$`SR_BH_steep`), digits)),

        survey_sd = f(median(.x$mcmc$`Q_extraSD_Acoustic_Survey(2)`), digits),
        catchability = ifelse(is.na(q_acoustic), NA, f(q_acoustic, digits)),

        survey_age1_sd = ifelse(is.null(.x$mcmc$`Q_extraSD_Age1_Survey(3)`),
                                NA,
                                f(median(.x$mcmc$`Q_extraSD_Age1_Survey(3)`), digits)),
        catchability_age1 = ifelse(all(is.na(q_age1)),
                                   NA,
                                   f(q_age1, digits)),

        dm_fishery = ifelse(is.null(.x$mcmc$`ln(DM_theta)_Age_P1`),
                            ifelse(is.null(.x$mcmc$`ln(DM_theta)_1`),
                                   NA,
                                   f(median(.x$mcmc$`ln(DM_theta)_1`), digits)),
                            f(median(.x$mcmc$`ln(DM_theta)_Age_P1`), digits)),
        dm_survey = ifelse(is.null(.x$mcmc$`ln(DM_theta)_Age_P2`),
                            ifelse(is.null(.x$mcmc$`ln(DM_theta)_2`),
                                   NA,
                                   f(median(.x$mcmc$`ln(DM_theta)_2`), digits)),
                            f(median(.x$mcmc$`ln(DM_theta)_Age_P2`), digits)),
        recs,
        bo = f(median(.x$mcmc$`SSB_Initial`) / 1e3, 0),
        ssb_2009 = paste0(f(median(.x$mcmc$`SSB_2009` / .x$mcmc$SSB_Initial) * 100, 1), "\\%"),
        ssb_curr = ifelse(.x$endyr <= end_yr - 2,
                          "--",
                          paste0(f(median(.x$mcmc[[paste0("SSB_", end_yr)]] / .x$mcmc$SSB_Initial) * 100, 1), "\\%")),
        spr_last = ifelse(.x$endyr <= end_yr - 2,
                          "--",
                          paste0(f(median(.x$mcmc[[paste0("SPRratio_", end_yr - 1)]]) * 100, 1), "\\%")),
        ssb_curr_fem = f(median(.x$mcmc$`SSB_SPR`) / 1e3, 0),
        spr_msy = "40.0\\%",
        exp_frac = .x$mcmccalcs$exp.frac.spr[2],
        yield_f40 = f(median(.x$mcmc$`Dead_Catch_SPR`) / 1e3, 0),
        total_like = f(filter(.x$likelihoods_used, rownames(.x$likelihoods_used) == "TOTAL")$values, 2),
        survey_like = f(filter(.x$likelihoods_used, rownames(.x$likelihoods_used) == "Survey")$values, 2),
        fishery_age_like = f(filter(.x$likelihoods_by_fleet, Label == "Age_like")$Fishery, 2),
        survey_age_like = f(filter(.x$likelihoods_by_fleet, Label == "Age_like")$Acoustic_Survey, 2),
        recr_like = f(filter(.x$likelihoods_used, rownames(.x$likelihoods_used) == "Recruitment")$values, 2),
        priors_like = f(filter(.x$likelihoods_used, rownames(.x$likelihoods_used) == "Parm_priors")$values, 2),
        parmdev_like = f(filter(.x$likelihoods_used, rownames(.x$likelihoods_used) == "Parm_devs")$values, 2)),
      value = .y)
  })

  # MLE parameter estimates
  if(mle_table){
    if(show_like){
      warning("Cannot show the likelihoods when `mle_table = TRUE`")
      show_like <- FALSE
    }
    model.names <- c(paste0(model.names[[1]], " MLE"), map_chr(model.names, ~{paste0(.x, " MCMC")}))
    md <- models[[1]]
    mdp <- md$parameters
    mddq <- md$derived_quants
    recs <- map_chr(getrecs, ~{(mddq %>% filter(Label == paste0("Recr_", .x)) %>% pull(Value) / 1e3) %>% f(0)})
    names(recs) <- paste0("recr_", getrecs)
    mle_tab <- enframe(
      c(nat_m = mdp %>% filter(Label == "NatM_uniform_Fem_GP_1") %>% pull(Value) %>% f(digits),
        ro = ((mdp %>% filter(Label == "SR_LN(R0)") %>% pull(Value) %>% log) * 1e3) %>% f(0),
        h = mdp %>% filter(Label == "SR_BH_steep") %>% pull(Value) %>% f(digits),
        survey_sd = mdp %>% filter(Label == "Q_extraSD_Acoustic_Survey(2)") %>% pull(Value) %>% f(digits),
        dm_fishery = ifelse(length(mdp %>% filter(Label == "ln(DM_theta)_1") %>% pull(Value)),
                            mdp %>% filter(Label == "ln(DM_theta)_1") %>% pull(Value) %>% f(digits), NA),
        dm_survey = ifelse(length(mdp %>% filter(Label == "ln(DM_theta)_2") %>% pull(Value)),
                           mdp %>% filter(Label == "ln(DM_theta)_2") %>% pull(Value)%>% f(digits), NA),
        survey_age1_sd = ifelse(length(mdp %>% filter(Label == "Q_extraSD_Age1_Survey(3)") %>% pull(Value)),
                                mdp %>% filter(Label == "Q_extraSD_Age1_Survey(3)") %>% pull(Value) %>% f(digits), NA),
        catchability = f(md$cpue$Calc_Q[1], digits),
        recs,
        bo = (mddq %>% filter(Label == "SSB_Initial") %>% pull(Value) / 1e3) %>% f(0),
        ssb_2009 = ((mddq %>% filter(Label == "SSB_2009") %>% pull(Value)) /
                      (mddq %>% filter(Label == "SSB_Initial") %>% pull(Value)) * 100) %>% f(1) %>% paste0("\\%"),
        ssb_curr = ((mddq %>% filter(Label == paste0("SSB_", end_yr)) %>% pull(Value)) /
                      (mddq %>% filter(Label == "SSB_Initial") %>% pull(Value)) * 100) %>% f(1) %>% paste0("\\%"),
        spr_last = ((mddq %>% filter(Label == paste0("SPRratio_", end_yr - 1)) %>% pull(Value)) * 100) %>% f(1) %>% paste0("\\%"),
        ssb_curr_fem = (mddq %>% filter(Label == "SSB_SPR") %>% pull(Value) / 1e3) %>% f(0),
        spr_msy = "40.0\\%",
        exp_frac = ((mddq %>% filter(Label == "annF_SPR") %>% pull(Value)) * 100) %>% f(1) %>% paste0("\\%"),
        yield_f40 = ((mddq %>% filter(Label == "Dead_Catch_SPR") %>% pull(Value)) / 1e3) %>% f(0),
        total_like = f(filter(md$likelihoods_used, rownames(md$likelihoods_used) == "TOTAL")$values, 2),
        survey_like = f(filter(md$likelihoods_used, rownames(md$likelihoods_used) == "Survey")$values, 2),
        fishery_age_like = f(filter(md$likelihoods_by_fleet, Label == "Age_like")$Fishery, 2),
        survey_age_like = f(filter(md$likelihoods_by_fleet, Label == "Age_like")$Acoustic_Survey, 2),
        recr_like = f(filter(md$likelihoods_used, rownames(md$likelihoods_used) == "Recruitment")$values, 2),
        priors_like = f(filter(md$likelihoods_used, rownames(md$likelihoods_used) == "Parm_priors")$values, 2),
        parmdev_like = f(filter(md$likelihoods_used, rownames(md$likelihoods_used) == "Parm_devs")$values, 2)),
    value = paste0(model.names[[1]]))
    tab <- c(list(mle_tab), tab)
  }

  # Remove parameter name column from all but first model then bind them all together,
  # make a variable that records if there are any age-1 index parameter values,
  # replace NAs with double-dashes, and add a blank row at the top for aesthetic purposes
  tab[-1] <- map(tab[-1], ~{
    .x[, 2]
  })
  tab <- tab %>%
    bind_cols
  age_1 <- !(tab %>% filter(name == "survey_age1_sd") %>% select(-name) %>% is.na %>% all)
  tab[is.na(tab)] <- "--"
  tab <- rbind(rep("", length(models)), tab)

  tab_labels <- enframe(
    c("", # For blank line at top
      paste0("Natural mortality (", latex.italics("M"), ")"),
      paste0("Unfished recruitment (", latex.subscr(latex.italics("R"), "0"), ", millions)"),
      paste0("Steepness (", latex.italics("h"), ")"),
      "Additional biomass index SD",
      paste0("Catchability: biomass index (", latex.italics("$q_b$"), ")"),
      "Additional age-1 index SD",
      paste0("Catchability: age-1 index (", latex.italics("$q_1$"), ")"),
      #paste0("Catchability: age-1 index (", latex.italics("q_1"), ")"),
      "Dirichlet-Multinomial fishery (log~$\\theta_{\\text{fish}}$)",
      "Dirichlet-Multinomial survey (log~$\\theta_{\\text{surv}}$)",
      paste(getrecs, "recruitment (millions)"),
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
    tab_labels <- tab_labels %>% filter(value != "Additional age-1 index SD")
    tab <- tab %>% filter(name != "survey_age1_sd")
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
  tab <- bind_cols(tab_labels, tab) %>%
    select(-name)

  # Split up the headers (model names) by words and let them stack on
  # top of each other to reduce width of table
  model.names.str <- map_chr(model.names, ~{latex.mlc(gsub(" ", "\\\\\\\\", .x), make.bold = FALSE)})
  colnames(tab) <- c("", model.names.str)

  # Add sub-headers for different parameter types
  addtorow <- list()
  addtorow$pos <- list()
  addtorow$pos[[1]] <- 1
  addtorow$pos[[2]] <- ifelse(age_1, 10, 9)
  addtorow$pos[[3]] <- ifelse(age_1, 17, 16)
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
  size.string <- latex.size.str(font.size, space.size)
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

make.long.parameter.estimates.table <- function(model,
                                                posterior.regex,
                                                digits = 4,
                                                xcaption = "default",
                                                xlabel   = "default",
                                                font.size = 9,
                                                space.size = 10){
  ## Returns an xtable in the proper format for the posterior medians of the
  ##  parameter estimates
  ##
  ## model - an mcmc run, output of the r4ss package's function SSgetMCMC()
  ## posterior.regex - a vector of the posterior names to search for
  ##  (partial names will be matched)
  ## digits - number of decimal points for the estimates
  ## xcaption - caption to appear in the calling document
  ## xlabel - the label used to reference the table in latex
  ## font.size - size of the font for the table
  ## space.size - size of the vertical spaces for the table

  mc <- model$mcmc
  mc.names <- names(mc)

  ## Start with the key posteriors using the regex
  mcmc.grep <- unique(grep(paste(posterior.regex, collapse = "|"), mc.names))
  mcmc.names <- mc.names[mcmc.grep]
  mcmc.par <- mc[,mcmc.grep]
  mcmc.meds <- as.data.frame(apply(mcmc.par, 2, median))
  df <- cbind(mcmc.names, mcmc.meds)
  names(df) <- c("param", "p.med")
  rownames(df) <- NULL

  calc.meds <- function(df, x){
    ## x is a data frame of posteriors for some parameters
    ## This function will take the medians of these,
    ##  and bind them with the data frame df, and return the result
    ## Assumes df has column names param and p.med
    d <- as.data.frame(apply(x, 2, median))
    d <- cbind(rownames(d), d)
    rownames(d) <- NULL
    names(d) <- c("param", "p.med")
    df <- rbind(df, d)
    return(df)
  }

  ## Add Dirichlet-Multinomial parameter
  ## currently only 1 value so calc.meds doesn't work due to
  ## getting a vector instead of a data.frame with names in header
  # dm <- data.frame(param = "ln(EffN_mult)_1",
  #                  p.med = median(mc$`ln(EffN_mult)_1`))
  # df <- rbind(df, dm)

  ## Add all Early_InitAge parameters
  ei <- mc[,grep("Early_InitAge_[0-9]+", mc.names)]
  df <- calc.meds(df, ei)

  ## Add all Recruitment deviation parameters
  rec <- mc[,union(grep(".*_RecrDev_[0-9]+", mc.names),
                  grep("ForeRecr_[0-9]+", mc.names))]
  df <- calc.meds(df, rec)

  ## Add all AgeSel
  a.sel <- mc[,grep("AgeSel_.*", mc.names)]
  df <- calc.meds(df, a.sel)

  ## Format the values
  df[,2] <- f(df[,2], digits)

  ## Make the underscores in the names have a preceding \ so latex will like it
  param.names <- df[,1]
  df[,1] <- gsub("\\_", "\\\\_", param.names)

  ## Latex column names
  names(df) <- c(latex.bold("Parameter"), latex.bold("Posterior median"))

  addtorow          <- list()
  addtorow$pos      <- list()
  addtorow$pos[[1]] <- c(0)
  addtorow$command  <- c(paste0(latex.hline,
                                "\n",
                                "\\endhead \n",
                                latex.hline,
                                "\n",
                                "{\\footnotesize Continued on next page} \n",
                                "\\endfoot \n",
                                "\\endlastfoot \n"))
  ## Make the size string for font and space size
  size.string <- latex.size.str(font.size, space.size)
  print(xtable(df,
               caption = xcaption,
               label = xlabel,
               align = get.align(ncol(df)),
               digits = digits),
        caption.placement = "top",
        table.placement = "H",
        tabular.environment = "longtable",
        floating = FALSE,
        include.rownames = FALSE,
        sanitize.text.function = function(x){x},
        size = size.string,
        add.to.row = addtorow,
        hline.after = c(-1))
}
