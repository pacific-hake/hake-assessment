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
