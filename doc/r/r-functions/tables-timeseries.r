make.ci.posterior.table <- function(model,                ## model is an mcmc run and is the output of the r4ss package's function SSgetMCMC
                                    start.yr,             ## start.yr is the first year to show in the table
                                    end.yr,               ## end.yr is the last year to show in the table
                                    weight.factor = 1000, ## divide catches by this factor
                                    xcaption = "default", ## Caption to use
                                    xlabel   = "default", ## Latex label to use
                                    font.size = 9,        ## Size of the font for the table
                                    space.size = 10,      ## Size of the spaces for the table
                                    digits = 1            ## Number of decimal points on % columns
                                    ){
  ## Returns an xtable in the proper format for the main tables section for credibility intervals for biomass,
  ##  relative biomass, recruitment, fishing intensity, and exploitation fraction

  yrs <- start.yr:end.yr

  ## Filter the values by years
  df <- lapply(model$mcmccalcs, function(x) x[names(x) %in% yrs])
  tot.bm.025 <- model$extra.mcmc$timeseries$Bio_all.0.025[model$extra.mcmc$timeseries$Yr %in% yrs]
  tot.bm.975 <- model$extra.mcmc$timeseries$Bio_all.0.975[model$extra.mcmc$timeseries$Yr %in% yrs]

  tab.filt <- cbind(yrs,
                    paste0(f(df$slower * weight.factor), "-", f(df$supper * weight.factor)),
                    paste0(f(df$dlower * 100, digits), "-", f(df$dupper * 100, digits), "\\%"),
                    paste0(f(tot.bm.025 / weight.factor), "-", f(tot.bm.975 / weight.factor)),
                    paste0(f(df$rlower * weight.factor), "-", f(df$rupper * weight.factor)),
                    paste0(f(df$plower * 100, digits), "-", f(df$pupper * 100, digits), "\\%"),
                    paste0(f(df$flower * 100, digits), "-", f(df$fupper * 100, digits), "\\%"))

  ## Make current year have dashes for exploitation rate and fishing intensity
  tab.filt[nrow(tab.filt), ncol(tab.filt)] <- "\\textbf{--}"
  tab.filt[nrow(tab.filt), ncol(tab.filt) - 1] <- "\\textbf{--}"

  ## Add latex headers
  colnames(tab.filt) <- c("\\specialcell{\\textbf{Year}}",
                          "\\specialcell{\\textbf{Female}\\\\\\textbf{spawning}\\\\\\textbf{biomass}\\\\\\textbf{(thousand t)}}",
                          "\\specialcell{\\textbf{Relative}\\\\\\textbf{spawning}\\\\\\textbf{biomass}}",
                          "\\specialcell{\\textbf{Total}\\\\\\textbf{biomass}\\\\\\textbf{(thousand t)}}",
                          "\\specialcell{\\textbf{Age-0}\\\\\\textbf{recruits}\\\\\\textbf{(millions)}}",
                          "\\specialcell{\\textbf{(1-SPR)}\\\\\\textbf{/}\\\\\\textbf{(1-SPR\\subscr{40\\%})}}",
                          "\\specialcell{\\textbf{Exploitation}\\\\\\textbf{fraction}}")

  ## Make the size string for font and space size
  size.string <- paste0("\\fontsize{",font.size,"}{",space.size,"}\\selectfont")
  return(print(xtable(tab.filt,
                      caption = xcaption,
                      label = xlabel,
                      align = get.align(ncol(tab.filt)),
                      digits = digits),
               caption.placement = "top",
               table.placement = "H",
               include.rownames = FALSE,
               sanitize.text.function = function(x){x},
               size = size.string))

}

make.median.posterior.table <- function(model,                ## model is an mcmc run and is the output of the r4ss package's function SSgetMCMC
                                        start.yr,             ## start.yr is the first year to show in the table
                                        end.yr,               ## end.yr is the last year to show in the table
                                        weight.factor = 1000, ## divide catches by this factor
                                        xcaption = "default", ## Caption to use
                                        xlabel   = "default", ## Latex label to use
                                        font.size = 9,        ## Size of the font for the table
                                        space.size = 10,      ## Size of the spaces for the table
                                        digits = 1            ## Number of decimal points on % columns
                                        ){
  ## Returns an xtable in the proper format for the main tables section for biomass,
  ##  relative biomass, recruitment, fishing intensity, and exploitation fraction

  yrs <- start.yr:end.yr

  ## Filter the values by years
  df <- lapply(model$mcmccalcs, function(x) x[names(x) %in% yrs])
  tot.bm <- model$extra.mcmc$timeseries$Bio_all[model$extra.mcmc$timeseries$Yr %in% yrs]

  tab.filt <- cbind(yrs,
                    f(df$smed * weight.factor),
                    paste0(f(df$dmed * 100, digits), "\\%"),
                    f(tot.bm / weight.factor),
                    f(df$rmed * weight.factor),
                    paste0(f(df$pmed * 100, digits), "\\%"),
                    paste0(f(df$fmed * 100, digits), "\\%"))

  ## Make current year have dashes for exploitation rate and fishing intensity
  tab.filt[nrow(tab.filt), ncol(tab.filt)] <- "\\textbf{--}"
  tab.filt[nrow(tab.filt), ncol(tab.filt) - 1] <- "\\textbf{--}"

  ## Add latex headers
  colnames(tab.filt) <- c("\\specialcell{\\textbf{Year}}",
                          "\\specialcell{\\textbf{Female}\\\\\\textbf{spawning}\\\\\\textbf{biomass}\\\\\\textbf{(thousand t)}}",
                          "\\specialcell{\\textbf{Relative}\\\\\\textbf{spawning}\\\\\\textbf{biomass}}",
                          "\\specialcell{\\textbf{Total}\\\\\\textbf{biomass}\\\\\\textbf{(thousand t)}}",
                          "\\specialcell{\\textbf{Age-0}\\\\\\textbf{recruits}\\\\\\textbf{(millions)}}",
                          "\\specialcell{\\textbf{Relative}\\\\\\textbf{fishing}\\\\\\textbf{intensity}}",
                          "\\specialcell{\\textbf{Exploitation}\\\\\\textbf{fraction}}")

  ## Make the size string for font and space size
  size.string <- paste0("\\fontsize{",font.size,"}{",space.size,"}\\selectfont")
  return(print(xtable(tab.filt,
                      caption = xcaption,
                      label = xlabel,
                      align = get.align(ncol(tab.filt)),
                      digits = digits),
               caption.placement = "top",
               table.placement = "H",
               include.rownames = FALSE,
               sanitize.text.function = function(x){x},
               size = size.string))

}

make.biomass.table <- function(model,                ## model is an mcmc run and is the output of the r4ss package's function SSgetMCMC
                               start.yr,             ## start.yr is the first year to show in the table
                               end.yr,               ## end.yr is the last year to show in the table
                               weight.factor = 1000, ## divide catches by this factor
                               xcaption = "default", ## Caption to use
                               xlabel   = "default", ## Latex label to use
                               font.size = 9,        ## Size of the font for the table
                               space.size = 10,      ## Size of the spaces for the table
                               digits = 1,           ## Number of decimal points
                               placement = "H"       ## where to place the table
                               ){
  ## Returns an xtable in the proper format for the executive summary biomass values for the base case mcmc
  ## Biomass quantiles
  slower <- model$mcmccalcs$slower * weight.factor
  smed <- model$mcmccalcs$smed * weight.factor
  supper <- model$mcmccalcs$supper * weight.factor
  ## Depletion quantiles
  dlower <- model$mcmccalcs$dlower * 100
  dmed <- model$mcmccalcs$dmed * 100
  dupper <- model$mcmccalcs$dupper * 100

  ## Join the values and apply the formatiing
  tab <- t(rbind(f(slower, digits),
                 f(smed, digits),
                 f(supper, digits),
                 paste0(f(dlower, digits), "\\%"),
                 paste0(f(dmed, digits), "\\%"),
                 paste0(f(dupper, digits), "\\%")))

  ## Filter for correct years to show and make thousand-seperated numbers (year assumed to be column 1)
  tab.filt <- tab[match(start.yr:end.yr, rownames(tab)),]

  ## Add year as a column
  tab.filt <- cbind.data.frame(rownames(tab.filt), tab.filt)
  names(tab.filt)[1] <- "year"

  colnames(tab.filt) <- c("",
                          "\\specialcell{\\textbf{2.5\\supscr{th}}\\\\\\textbf{percentile}}",
                          "\\specialcell{\\textbf{Median}}",
                          "\\specialcell{\\textbf{97.5\\supscr{th}}\\\\\\textbf{percentile}}",
                          "\\specialcell{\\textbf{2.5\\supscr{th}}\\\\\\textbf{percentile}}",
                          "\\specialcell{\\textbf{Median}}",
                          "\\specialcell{\\textbf{97.5\\supscr{th}}\\\\\\textbf{percentile}}")

  addtorow <- list()
  addtorow$pos <- list()
  addtorow$pos[[1]] <- -1
  addtorow$pos[[2]] <- nrow(tab.filt)
  addtorow$command <- c(paste0("\\toprule \\multirow{3}{*}{\\textbf{Year}} & ",
                               "\\multicolumn{3}{c}{\\specialcell{\\textbf{Spawning Biomass}\\\\\\textbf{(thousand t)}}} & ",
                               "\\multicolumn{3}{c}{\\specialcell{\\textbf{Relative spawning biomass}\\\\\\textbf{(B\\subscr{t}/B\\subscr{0})}}} \\\\ ",
                               "\\cmidrule(r){2-4} ",
                               "\\cmidrule(r){5-7} "),
                        "\\bottomrule")
  ## Make the size string for font and space size
  size.string <- paste0("\\fontsize{",font.size,"}{",space.size,"}\\selectfont")
  return(print(xtable(tab.filt,
                      caption = xcaption,
                      label = xlabel,
                      align = get.align(ncol(tab.filt)),
                      digits = digits),
               caption.placement = "top",
               add.to.row = addtorow,
               table.placement = placement,
               include.rownames = FALSE,
               sanitize.text.function = function(x){x},
               size = size.string,
               hline.after = c(0),  ## Single line after the column headers
               booktabs = TRUE))
}

make.recruitment.table <- function(model,                ## model is an mcmc run and is the output of the r4ss package's function SSgetMCMC
                                   start.yr,             ## start.yr is the first year to show in the table
                                   end.yr,               ## end.yr is the last year to show in the table
                                   weight.factor = 1000, ## multiply recruitments by this factor
                                   xcaption = "default", ## Caption to use
                                   xlabel   = "default", ## Latex label to use
                                   font.size = 9,        ## Size of the font for the table
                                   space.size = 10,      ## Size of the spaces for the table
                                   digits = 1,           ## Number of decimal points in recruitment
                                   digits.dev = 3,       ## Number of decimal points in recruitment deviations
                                   placement = "H"       ## where to place the table
                                   ){
  ## Returns an xtable in the proper format for the executive summary recruitment and deviations values for the base case mcmc
  yrs <- start.yr:end.yr
  ## Recruitment quantiles
  rlower <- model$mcmccalcs$rlower * weight.factor
  rmed <- model$mcmccalcs$rmed * weight.factor
  rupper <- model$mcmccalcs$rupper * weight.factor
  ## Only include start year to end year
  rlower <- rlower[names(rlower) %in% yrs]
  rmed <- rmed[names(rmed) %in% yrs]
  rupper <- rupper[names(rupper) %in% yrs]

  ## Deviations quantiles
  devlower <- model$mcmccalcs$devlower
  devmed <- model$mcmccalcs$devmed
  devupper <- model$mcmccalcs$devupper

  ## Remove recruitment deviations prior to the start year
  devlower <- devlower[names(devlower) %in% yrs]
  devmed <- devmed[names(devmed) %in% yrs]
  devupper <- devupper[names(devupper) %in% yrs]

  ## Join the values and apply the formatiing
  tab <- t(rbind(f(rlower,digits),f(rmed,digits),f(rupper,digits),
                 f(devlower,digits.dev),f(devmed,digits.dev),f(devupper,digits.dev)))

  ## Filter for correct years to show and make thousand-seperated numbers (year assumed to be column 1)
  tab.filt <- tab[match(start.yr:end.yr, rownames(tab)),]

  ## Add year as a column
  tab.filt <- cbind.data.frame(rownames(tab.filt), tab.filt)
  names(tab.filt)[1] <- "year"

  colnames(tab.filt) <- c("",
                          "\\specialcell{\\textbf{2.5\\supscr{th}}\\\\\\textbf{percentile}}",
                          "\\specialcell{\\textbf{Median}}",
                          "\\specialcell{\\textbf{97.5\\supscr{th}}\\\\\\textbf{percentile}}",
                          "\\specialcell{\\textbf{2.5\\supscr{th}}\\\\\\textbf{percentile}}",
                          "\\specialcell{\\textbf{Median}}",
                          "\\specialcell{\\textbf{97.5\\supscr{th}}\\\\\\textbf{percentile}}")

  addtorow <- list()
  addtorow$pos <- list()
  addtorow$pos[[1]] <- -1
  addtorow$pos[[2]] <- nrow(tab.filt)
  addtorow$command <- c(paste0("\\toprule \\multirow{3}{*}{\\textbf{Year}} & ",
                               "\\multicolumn{3}{c}{\\specialcell{\\textbf{Absolute recruitment}\\\\\\textbf{(millions)}}} & ",
                               "\\multicolumn{3}{c}{\\textbf{Recruitment deviations}} \\\\ ",
                               "\\cmidrule(r){2-4} ",
                               "\\cmidrule(r){5-7} "),
                        "\\bottomrule")
  ## Make the size string for font and space size
  size.string <- paste0("\\fontsize{",font.size,"}{",space.size,"}\\selectfont")
  return(print(xtable(tab.filt,
                      caption = xcaption,
                      label = xlabel,
                      align = get.align(ncol(tab.filt)),
                      digits = digits),
               caption.placement = "top",
               add.to.row = addtorow,
               table.placement = placement,
               include.rownames = FALSE,
               sanitize.text.function = function(x){x},
               size = size.string,
               hline.after = c(0),  ## Single line after the column headers
               booktabs = TRUE))
}

make.fishing.intensity.table <- function(model,                ## model is an mcmc run and is the output of the r4ss package's function SSgetMCMC
                                         start.yr,             ## start.yr is the first year to show in the table
                                         end.yr,               ## end.yr is the last year to show in the table
                                         xcaption = "default", ## Caption to use
                                         xlabel   = "default", ## Latex label to use
                                         font.size = 9,        ## Size of the font for the table
                                         space.size = 10,      ## Size of the spaces for the table
                                         digits = 1,           ## Number of decimal points
                                         placement = "H"       ## where to place the table
                                         ){
  ## Returns an xtable in the proper format for the executive summary fishing intensity and
  ## exploitation fraction values for the base case mcmc
  ## Fishing intensity quantiles
  plower <- model$mcmccalcs$plower
  pmed <- model$mcmccalcs$pmed
  pupper <- model$mcmccalcs$pupper
  ## Exploitation fraction quantiles
  flower <- model$mcmccalcs$flower
  fmed <- model$mcmccalcs$fmed
  fupper <- model$mcmccalcs$fupper

  yrs <- start.yr:end.yr

  ## remove prepended strings from year labels
  names(plower) <- gsub("SPRratio_","",names(plower))
  names(pmed) <- gsub("SPRratio_","",names(pmed))
  names(pupper) <- gsub("SPRratio_","",names(pupper))

  names(flower) <- gsub("F_","",names(flower))
  names(fmed) <- gsub("F_","",names(fmed))
  names(fupper) <- gsub("F_","",names(fupper))

  ## Remove any projection years from SPR tables
  plower <- plower[(names(plower) %in% yrs)]
  pmed <- pmed[(names(pmed) %in% yrs)]
  pupper <- pupper[(names(pupper) %in% yrs)]

  ## Remove any projection years from F tables
  flower <- flower[(names(flower) %in% yrs)]
  fmed <- fmed[(names(fmed) %in% yrs)]
  fupper <- fupper[(names(fupper) %in% yrs)]

  ## Join the values and apply the formatiing
  tab <- t(rbind(f(plower,digits),f(pmed,digits),f(pupper,digits),
                 f(flower,digits),f(fmed,digits),f(fupper,digits)))

  ## Filter for correct years to show
  tab.filt <- tab[match(start.yr:end.yr, rownames(tab)),]

  ## Add year as a column
  tab.filt <- cbind.data.frame(rownames(tab.filt), tab.filt)
  names(tab.filt)[1] <- "year"

  colnames(tab.filt) <- c("",
                          "\\specialcell{\\textbf{2.5\\supscr{th}}\\\\\\textbf{percentile}}",
                          "\\specialcell{\\textbf{Median}}",
                          "\\specialcell{\\textbf{97.5\\supscr{th}}\\\\\\textbf{percentile}}",
                          "\\specialcell{\\textbf{2.5\\supscr{th}}\\\\\\textbf{percentile}}",
                          "\\specialcell{\\textbf{Median}}",
                          "\\specialcell{\\textbf{97.5\\supscr{th}}\\\\\\textbf{percentile}}")

  addtorow <- list()
  addtorow$pos <- list()
  addtorow$pos[[1]] <- -1
  addtorow$pos[[2]] <- nrow(tab.filt)
  addtorow$command <- c(paste0("\\toprule \\multirow{3}{*}{\\textbf{Year}} & ",
                               "\\multicolumn{3}{c}{\\textbf{Relative fishing intensity}} & ",
                               "\\multicolumn{3}{c}{\\textbf{Exploitation fraction}} \\\\ ",
                               "\\cmidrule(r){2-4} ",
                               "\\cmidrule(r){5-7} "),
                        "\\bottomrule")
  ## Make the size string for font and space size
  size.string <- paste0("\\fontsize{",font.size,"}{",space.size,"}\\selectfont")
  return(print(xtable(tab.filt,
                      caption = xcaption,
                      label = xlabel,
                      align = get.align(ncol(tab.filt)),
                      digits = digits),
               caption.placement = "top",
               add.to.row = addtorow,
               table.placement = placement,
               include.rownames = FALSE,
               sanitize.text.function = function(x){x},
               size = size.string,
               hline.after = c(0),  ## Single line after the column headers
               booktabs = TRUE))
}
