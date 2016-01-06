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

  ## Join the values and apply the formatiing
  tab <- t(rbind(fmt0(slower,1),fmt0(smed,1),fmt0(supper,1),
                 paste0(round(dlower,1),"\\%"),paste0(round(dmed,1),"\\%"),paste0(round(dupper,1),"\\%")))

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
  ##addtorow$pos[[2]] <- -1
  addtorow$command <- c("& \\multicolumn{3}{|c}{\\specialcell{\\textbf{Spawning biomass}\\\\\\textbf{(thousand t)}}} & \\multicolumn{3}{|c|}{\\specialcell{\\textbf{Relative spawning biomass}\\\\\\textbf{(B\\subscr{t}/B\\subscr{0})}}} \\\\")
  ## Make the size string for font and space size
  size.string <- paste0("\\fontsize{",font.size,"}{",space.size,"}\\selectfont")
  return(print(xtable(tab.filt, caption=xcaption, label=xlabel, align=get.align(ncol(tab.filt))),
               caption.placement = "top", add.to.row=addtorow, include.rownames=FALSE, sanitize.text.function=function(x){x},
               size=size.string))
}
