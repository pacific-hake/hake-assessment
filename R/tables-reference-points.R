make.reference.points.table <- function(model,
                                        xcaption = "default",
                                        xlabel   = "default",
                                        font.size = 9,
                                        space.size = 10,
                                        placement = "tbp",
                                        tabular.envt = "longtable"){
  ## Returns an xtable in the proper format for the executive summary
  ##  reference points. The values are calculated previously in the calc_mcmc
  ##  function in load-models.r.
  ##
  ## model - an mcmc run, output of the r4ss package's function SSgetMCMC()
  ## probs - values to use for the quantile funcstion
  ## xcaption - caption to appear in the calling document
  ## xlabel - the label used to reference the table in latex
  ## font.size - size of the font for the table
  ## space.size - size of the vertical spaces for the table
  ## placement - where to put table
  ## tabular.envt - "longtable" or "table"

  m <- model$mcmccalcs
  tab <- rbind(m$unfish_fem_bio,
               m$unfish_recr,
               m$f_spawn_bio_bf40,
               m$spr_msy_proxy,
               m$exp_frac_spr,
               m$yield_bf40,
               m$fem_spawn_bio_b40,
               m$spr_b40,
               m$exp_frac_b40,
               m$yield_b40,
               m$fem_spawn_bio_bmsy,
               m$spr_msy,
               m$exp_frac_sprmsy,
               m$msy)
  descr <- c("Unfished female spawning biomass ($B_0$, thousand t)",
             "Unfished recruitment ($R_0$, millions)",
             "Female spawning biomass at $\\Fforty$ ($B_{\\text{SPR}=40\\%}$, thousand t)",
             "SPR at $\\Fforty$",
             "Exploitation fraction corresponding to $\\Fforty$",
             "Yield associated with $\\Fforty$ (thousand t)",
             "Female spawning biomass ($B_{40\\%}$, thousand t)",
             "SPR at $B_{40\\%}$",
             "Exploitation fraction resulting in $B_{40\\%}$",
             "Yield at $B_{40\\%}$ (thousand t)",
             "Female spawning biomass ($B_{\\text{MSY}}$, thousand t)",
             "SPR at MSY",
             "Exploitation fraction corresponding to SPR at MSY",
             "MSY (thousand t)")
  tab <- cbind(descr, tab)
  colnames(tab) <- c(latex_bold("Quantity"),
                     latex_mlc(c(latex_supscr("2.5", "th"),
                                 "percentile")),
                     latex_bold("Median"),
                     latex_mlc(c(latex_supscr("97.5", "th"),
                                 "percentile")))
  addtorow <- list()
  addtorow$pos <- list()
  addtorow$pos[[1]] <- -1
  addtorow$pos[[2]] <- 2
  addtorow$pos[[3]] <- 6
  addtorow$pos[[4]] <- 10

  header_code <- paste0(latex_hline,
                        paste(colnames(tab), collapse = latex_amp()),
                        latex_nline,
                        latex_hline)

  header_code <- paste0(header_code,
                        latex_continue(ncol(tab), header_code))
  addtorow$command <-
    c(header_code,
      paste0(latex_nline,
             latex_bold(latex_under(paste0("Reference points (equilibrium) ",
                                           "based on $\\Fforty$"))),
             latex_nline),
      paste0(latex_nline,
             latex_bold(latex_under(paste0("Reference points (equilibrium) ",
                                           "based on $B_{40\\%}$ (40\\% of ",
                                           "$B_0$)"))),
             latex_nline),
      paste0(latex_nline,
             latex_bold(latex_under(paste0("Reference points (equilibrium) ",
                                           "based on estimated MSY"))),
             latex_nline))

  size.string <- latex_size_str(font.size, space.size)
  print(xtable(tab,
               caption = xcaption,
               label = xlabel,
               align = get.align(ncol(tab),
                                 just="c")),
        caption.placement = "top",
        include.rownames = FALSE,
        include.colnames = FALSE,
        sanitize.text.function = function(x){x},
        size = size.string,
        add.to.row = addtorow,
        tabular.environment = tabular.envt,
        table.placement = placement,
        hline.after = NULL)
}

# older version from commit 8908923 that has no longtable option but works for Exec Summary (and
#  added in $B_{\\text{SPR}=40\\%}$):
make.reference.points.table.old <- function(model,
                                        xcaption = "default",
                                        xlabel   = "default",
                                        font.size = 9,
                                        space.size = 10,
                                        placement = "H"){
  ## Returns an xtable in the proper format for the executive summary
  ##  reference points. The values are calculated previously in the calc_mcmc
  ##  function in load-models.r.
  ##
  ## model - an mcmc run, output of the r4ss package's function SSgetMCMC()
  ## probs - values to use for the quantile funcstion
  ## xcaption - caption to appear in the calling document
  ## xlabel - the label used to reference the table in latex
  ## font.size - size of the font for the table
  ## space.size - size of the vertical spaces for the table

  m <- model$mcmccalcs
  tab <- rbind(m$unfish_fem_bio,
               m$unfish_recr,
               m$f_spawn_bio_bf40,
               m$spr_msy_proxy,
               m$exp_frac_spr,
               m$yield_bf40,
               m$fem_spawn_bio_b40,
               m$spr_b40,
               m$exp_frac_b40,
               m$yield_b40,
               m$fem_spawn_bio_bmsy,
               m$spr_msy,
               m$exp_frac_sprmsy,
               m$msy)
  descr <- c("Unfished female spawning biomass ($B_0$, thousand t)",
             "Unfished recruitment ($R_0$, millions)",
             "Female spawning biomass at $\\Fforty$ ($B_{\\text{SPR}=40\\%}$, thousand t)",
             "SPR at $\\Fforty$",
             "Exploitation fraction corresponding to $\\Fforty$",
             "Yield associated with $\\Fforty$ (thousand t)",
             "Female spawning biomass ($B_{40\\%}$, thousand t)",
             "SPR at $B_{40\\%}$",
             "Exploitation fraction resulting in $B_{40\\%}$",
             "Yield at $B_{40\\%}$ (thousand t)",
             "Female spawning biomass ($B_{\\text{MSY}}$, thousand t)",
             "SPR at MSY",
             "Exploitation fraction corresponding to SPR at MSY",
             "MSY (thousand t)")
  tab <- cbind(descr, tab)
  colnames(tab) <- c(latex_bold("Quantity"),
                     latex_mlc(c(latex_supscr("2.5", "th"),
                                 "percentile")),
                     latex_bold("Median"),
                     latex_mlc(c(latex_supscr("97.5", "th"),
                                 "percentile")))
  addtorow <- list()
  addtorow$pos <- list()
  addtorow$pos[[1]] <- 2
  addtorow$pos[[2]] <- 6
  addtorow$pos[[3]] <- 10
  addtorow$command <-
    c(paste0(latex_nline,
             latex_bold(latex_under(paste0("Reference points (equilibrium) ",
                                           "based on $\\Fforty$"))),
      latex_nline),
      paste0(latex_nline,
             latex_bold(latex_under(paste0("Reference points (equilibrium) ",
                                           "based on $B_{40\\%}$ (40\\% of ",
                                           "$B_0$)"))),
      latex_nline),
      paste0(latex_nline,
             latex_bold(latex_under(paste0("Reference points (equilibrium) ",
                                           "based on estimated MSY"))),
      latex_nline))

  size.string <- latex_size_str(font.size, space.size)
  print(xtable(tab,
               caption = xcaption,
               label = xlabel,
               align = get.align(ncol(tab),
                                 just="c")),
        caption.placement = "top",
        include.rownames = FALSE,
        sanitize.text.function = function(x){x},
        size = size.string,
        add.to.row = addtorow,
        table.placement = placement)
}
