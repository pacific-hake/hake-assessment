make.biomass.table <- function(model,                ## model is an mcmc run and is the output of the r4ss package's function SSgetMCMC
                               start.yr,             ## start.yr is the first year to show in the table
                               end.yr,               ## end.yr is the last year to show in the table
                               weight.factor = 1000, ## divide catches by this factor
                               xcaption = "default", ## Caption to use
                               xlabel   = "default", ## Latex label to use
                               font.size = 9,        ## Size of the font for the table
                               space.size = 10,      ## Size of the spaces for the table
                               digits = 1            ## Number of decimal points
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

  ## remove prepended strings from year labels
  names(slower) <- gsub("SPB_","",names(slower))
  names(smed) <- gsub("SPB_","",names(smed))
  names(supper) <- gsub("SPB_","",names(supper))

  names(dlower) <- gsub("SPB_","",names(dlower))
  names(dmed) <- gsub("SPB_","",names(dmed))
  names(dupper) <- gsub("SPB_","",names(dupper))

  ## Remove Initial biomass from biomass
  slower <- slower[-grep("Initial",names(slower))]
  smed <- smed[-grep("Initial",names(smed))]
  supper <- supper[-grep("Initial",names(supper))]

  ## Join the values and apply the formatiing
  tab <- t(rbind(fmt0(slower,digits),fmt0(smed,digits),fmt0(supper,digits),
                 paste0(fmt0(dlower,digits),"\\%"),paste0(fmt0(dmed,digits),"\\%"),paste0(fmt0(dupper,digits),"\\%")))

  ## Filter for correct years to show and make thousand-seperated numbers (year assumed to be column 1)
  tab.filt <- tab[match(start.yr:end.yr, rownames(tab)),]

  ## Add year as a column
  tab.filt <- cbind.data.frame(rownames(tab.filt), tab.filt)
  names(tab.filt)[1] <- "year"

  ## Add latex headers
  colnames(tab.filt) <- c("\\specialcell{\\textbf{Year}}",
                          "\\specialcell{\\textbf{2.5\\supscr{th}}\\\\\\textbf{percentile}}",
                          "\\specialcell{\\textbf{Median}}",
                          "\\specialcell{\\textbf{97.5\\supscr{th}}\\\\\\textbf{percentile}}",
                          "\\specialcell{\\textbf{2.5\\supscr{th}}\\\\\\textbf{percentile}}",
                          "\\specialcell{\\textbf{Median}}",
                          "\\specialcell{\\textbf{97.5\\supscr{th}}\\\\\\textbf{percentile}}")

  ## Add the extra header spanning multiple columns
  addtorow <- list()
  addtorow$pos <- list()
  addtorow$pos[[1]] <- -1
  addtorow$command <- c("& \\multicolumn{3}{|c}{\\specialcell{\\textbf{Spawning biomass}\\\\\\textbf{(thousand t)}}} & \\multicolumn{3}{|c|}{\\specialcell{\\textbf{Relative spawning biomass}\\\\\\textbf{(B\\subscr{t}/B\\subscr{0})}}} \\\\")
  ## Make the size string for font and space size
  size.string <- paste0("\\fontsize{",font.size,"}{",space.size,"}\\selectfont")
  return(print(xtable(tab.filt, caption=xcaption, label=xlabel, align=get.align(ncol(tab.filt))),
               caption.placement = "top", add.to.row=addtorow, table.placement="H", include.rownames=FALSE, sanitize.text.function=function(x){x},
               size=size.string))
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
                                   digits.dev = 3        ## Number of decimal points in recruitment deviations
                                   ){
  ## Returns an xtable in the proper format for the executive summary recruitment and deviations values for the base case mcmc
  ## Recruitment quantiles
  rlower <- model$mcmccalcs$rlower * weight.factor
  rmed <- model$mcmccalcs$rmed * weight.factor
  rupper <- model$mcmccalcs$rupper * weight.factor
  ## Deviations quantiles
  devlower <- model$mcmccalcs$devlower
  devmed <- model$mcmccalcs$devmed
  devupper <- model$mcmccalcs$devupper

  ## remove prepended strings from year labels
  names(rlower) <- gsub("Recr_","",names(rlower))
  names(rmed) <- gsub("Recr_","",names(rmed))
  names(rupper) <- gsub("Recr_","",names(rupper))

  names(devlower) <- gsub(".*_RecrDev_","",names(devlower))
  names(devmed) <- gsub(".*_RecrDev_","",names(devmed))
  names(devupper) <- gsub(".*_RecrDev_","",names(devupper))

  names(devlower) <- gsub("ForeRecr_","",names(devlower))
  names(devmed) <- gsub("ForeRecr_","",names(devmed))
  names(devupper) <- gsub("ForeRecr_","",names(devupper))

  ## Remove virgin recruitment if present
  rlower <- rlower[-grep("Virgin",names(rlower))]
  rmed <- rmed[-grep("Virgin",names(rmed))]
  rupper <- rupper[-grep("Virgin",names(rupper))]

  ## Remove recruitment deviations prior to the start year
  devlower <- devlower[-grep("Early.*",names(devlower))]
  devmed <- devmed[-grep("Early.*",names(devmed))]
  devupper <- devupper[-grep("Early.*",names(devupper))]

  ## Remove and projected years from the deviations, based on the recruitment years
  ## After this devmed and rmed will be the same length, with the same years present.
  devlower <- devlower[(names(devlower) %in% names(rlower))]
  devmed <- devmed[(names(devmed) %in% names(rmed))]
  devupper <- devupper[(names(devupper) %in% names(rupper))]

  ## Join the values and apply the formatiing
  tab <- t(rbind(fmt0(rlower,digits),fmt0(rmed,digits),fmt0(rupper,digits),
                 fmt0(devlower,digits.dev),fmt0(devmed,digits.dev),fmt0(devupper,digits.dev)))

  ## Filter for correct years to show and make thousand-seperated numbers (year assumed to be column 1)
  tab.filt <- tab[match(start.yr:end.yr, rownames(tab)),]

  ## Add year as a column
  tab.filt <- cbind.data.frame(rownames(tab.filt), tab.filt)
  names(tab.filt)[1] <- "year"

  ## Add latex headers
  colnames(tab.filt) <- c("\\specialcell{\\textbf{Year}}",
                          "\\specialcell{\\textbf{2.5\\supscr{th}}\\\\\\textbf{percentile}}",
                          "\\specialcell{\\textbf{Median}}",
                          "\\specialcell{\\textbf{97.5\\supscr{th}}\\\\\\textbf{percentile}}",
                          "\\specialcell{\\textbf{2.5\\supscr{th}}\\\\\\textbf{percentile}}",
                          "\\specialcell{\\textbf{Median}}",
                          "\\specialcell{\\textbf{97.5\\supscr{th}}\\\\\\textbf{percentile}}")

  ## Add the extra header spanning multiple columns
  addtorow <- list()
  addtorow$pos <- list()
  addtorow$pos[[1]] <- -1
  addtorow$command <- c("& \\multicolumn{3}{|c}{\\specialcell{\\textbf{Absolute recruitment}\\\\\\textbf{(millions)}}} & \\multicolumn{3}{|c|}{\\specialcell{\\textbf{Recruitment deviations}}} \\\\")
  ## Make the size string for font and space size
  size.string <- paste0("\\fontsize{",font.size,"}{",space.size,"}\\selectfont")
  return(print(xtable(tab.filt, caption=xcaption, label=xlabel, align=get.align(ncol(tab.filt))),
               caption.placement = "top", add.to.row=addtorow, table.placement="H", include.rownames=FALSE, sanitize.text.function=function(x){x},
               size=size.string))
}

make.fishing.intensity.table <- function(model,                ## model is an mcmc run and is the output of the r4ss package's function SSgetMCMC
                                         start.yr,             ## start.yr is the first year to show in the table
                                         end.yr,               ## end.yr is the last year to show in the table
                                         xcaption = "default", ## Caption to use
                                         xlabel   = "default", ## Latex label to use
                                         font.size = 9,        ## Size of the font for the table
                                         space.size = 10,      ## Size of the spaces for the table
                                         digits = 1            ## Number of decimal points
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
  tab <- t(rbind(fmt0(plower,digits),fmt0(pmed,digits),fmt0(pupper,digits),
                 fmt0(flower,digits),fmt0(fmed,digits),fmt0(fupper,digits)))

  ## Filter for correct years to show
  tab.filt <- tab[match(start.yr:end.yr, rownames(tab)),]

  ## Add year as a column
  tab.filt <- cbind.data.frame(rownames(tab.filt), tab.filt)
  names(tab.filt)[1] <- "year"

  ## Add latex headers
  colnames(tab.filt) <- c("\\specialcell{\\textbf{Year}}",
                          "\\specialcell{\\textbf{2.5\\supscr{th}}\\\\\\textbf{percentile}}",
                          "\\specialcell{\\textbf{Median}}",
                          "\\specialcell{\\textbf{97.5\\supscr{th}}\\\\\\textbf{percentile}}",
                          "\\specialcell{\\textbf{2.5\\supscr{th}}\\\\\\textbf{percentile}}",
                          "\\specialcell{\\textbf{Median}}",
                          "\\specialcell{\\textbf{97.5\\supscr{th}}\\\\\\textbf{percentile}}")

  ## Add the extra header spanning multiple columns
  addtorow <- list()
  addtorow$pos <- list()
  addtorow$pos[[1]] <- -1
  addtorow$command <- c("& \\multicolumn{3}{|c}{\\specialcell{\\textbf{Fishing intensity}}} & \\multicolumn{3}{|c|}{\\specialcell{\\textbf{Exploitation fraction}}} \\\\")
  ## Make the size string for font and space size
  size.string <- paste0("\\fontsize{",font.size,"}{",space.size,"}\\selectfont")
  return(print(xtable(tab.filt, caption=xcaption, label=xlabel, align=get.align(ncol(tab.filt)), digits=digits),
               caption.placement = "top", add.to.row=addtorow, table.placement="H", include.rownames=FALSE, sanitize.text.function=function(x){x},
               size=size.string))
}
