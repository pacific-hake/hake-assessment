make.decision.table.pres <- function(model,                  ## model is an mcmc run and is the output of the r4ss package's function SSgetMCMC
                                catch.levels,           ## list of catch levels
                                rowLabs,                #labels for each row, including blanks
                                xcaption   = "default", ## Caption to use
                                xlabel     = "default", ## Latex label to use
                                font.size  = 9,         ## Size of the font for the table
                                space.size = 10,        ## Size of the spaces for the table
                                which      = "biomass",  ## Which type to build. "biomass" or "spr".
                                placement = "H"       ## Placement of table
                                ){
  ## Returns an xtable in the proper format for the executive summary decision tables

  if(which != "biomass" & which != "spr"){
    stop("make.decisions.table: Error - type '",which,"' is not implemented. Stopping...\n\n")
  }
  if(which == "biomass"){
    forecast <- model$biomass
    table.header1 <- "\\textbf{Beginning of year}"
    table.header2 <- "\\textbf{relative spawning biomass}"
  }else{
    forecast <- model$spr
    table.header1 <- "\\textbf{Fishing}"
    table.header2 <- "\\textbf{Intensity}"
  }

  # ## tab.letters are the letters in the table, one for each forecast management action
  # ## and a blank for all but the first year in a management action
  # ##additional labels are given for some rows (below letter)
  # rows2Label <- c("d","e","f","g")
  # rowLabels <- list(c("2015","TAC"),c("FI=","100\\%"),c("default","HR"),c("C2016=","C2017"))
  # tab.letters <- NULL
  # next.ind <- 1
  # for(i in 1:length(forecast)){
  #   tab.letters[next.ind] <- paste0(letters[i],":")
  #   next.ind <- next.ind + 1
  #   for(j in 1:(nrow(forecast[[i]])-1)){
  #     if(letters[i] %in% rows2Label) {
  #       lab <- rowLabels[[which(letters[i]==rows2Label)]]
  #       tab.letters[next.ind] <- lab[j]
  #     } else {
  #       tab.letters[next.ind] <- ""
  #     }
  #     next.ind <- next.ind + 1
  #   }
  # }

  ## Merge the list elements into a data frame
  forecast.tab <- fmt0(do.call("rbind", forecast) * 100)

  ## Store years for binding later
  yrs <- rownames(forecast.tab)

  ## Append the escaped % symbols
  forecast.tab <- apply(forecast.tab, 2, paste0, "\\%")


  ## Change the quantile levels so they have correct latex escape sequence
  quant.levels <- gsub("%","\\\\%",colnames(forecast.tab))

  ## Set any catch less than 1 to be 0
  c.levels <- unlist(catch.levels)
  c.levels[c.levels < 1] <- 0
  ## Bind the catch levels and years to the correct rows
  forecast.tab <- cbind(rowLabs, yrs, fmt0(c.levels), forecast.tab)
  colnames(forecast.tab) <- c("",
                              "Year",
                              "Catch (t)",
                              quant.levels)

  ## Add the extra header spanning multiple columns
  addtorow <- list()
  addtorow$pos <- list()
  addtorow$pos[[1]] <- -1
  addtorow$pos[[2]] <- nrow(forecast.tab)

  quant.string <- ""
  quant.ampersands <- ""
  quant.cell.defs <- NULL
  for(i in 1:length(quant.levels)){
    quant.string <- paste0(quant.string, "& ", quant.levels[i], " ")
    quant.ampersands <- paste0(quant.ampersands,"& ")
    quant.cell.defs <- c(quant.cell.defs, "Y")
  }
  ## Add the vertical bar to the edge of the last quant cell
  quant.cell.defs[length(quant.cell.defs)] <- paste0(quant.cell.defs[length(quant.cell.defs)], "|")

  addtorow$command <- c(paste0("\\hline ",
                               "\\multicolumn{3}{|c|}{Within model quantile} ", quant.string, " \\\\ ",
                               "\\hline ",
                               "\\multicolumn{3}{|c|}{Management Action} &\\multicolumn{",length(quant.levels),"}{c|}{",table.header1,"}\\\\ ",
                               "\\cline{1-3} ",
                               " & Year & Catch (t) & \\multicolumn{",length(quant.levels),"}{c|}{",table.header2,"} \\\\ ",
                               "\\hline "),
                        "\\hline ")
  ## Add the right number of horizontal lines to make the table break in the correct places
  ## A line is not needed at the bottom explains the (length(forecast)-1) statement.
  for(i in 1:(length(forecast)-1)){
    addtorow$pos[[i+2]] <- i * nrow(forecast[[i]])
    addtorow$command <- c(addtorow$command, "\\hline ")
  }
  ## Make the size string for font and space size
  size.string <- paste0("\\fontsize{",font.size,"}{",space.size,"}\\selectfont")
  return(print(xtable(forecast.tab,
                      caption=xcaption,
                      label=xlabel,
                      align=c("c","|c","c","c|",quant.cell.defs)),
               caption.placement = "top",
               include.rownames = FALSE,
               include.colnames = FALSE,
               sanitize.text.function = function(x){x},
               size = size.string,
               add.to.row = addtorow,
               table.placement = placement,
               tabular.environment = "tabularx",
               width = "\\textwidth",
               hline.after = NULL))
}


make.comparison.plot.mcmc <- function(models,                   ## models is a list of model runs to be plotted against of which
                                                           ## each element is the output of the r4ss package's function SSgetMCMC
                                 subplots = 1,             ## Same as subplot argument in SSplotComparisons
                                 model.names = NULL,       ## vector of model names. Must be same length as models
                                 densitynames = NULL,      ## Same as densitynames argument in SSplotComparisons
                                 indexPlotEach = FALSE,    ## Same as indexPlotEach argument in SSplotComparisons
                                 indexUncertainty = FALSE, ## Same as indexUncertainty argument in SSplotComparisons
                                 indexfleets = NULL,       ## Passed to the SSplotComparisons function
                                 is.retro = FALSE,         ## Is this a retrospective plot?
                                 legend = TRUE,            ## Passed to the SSplotComparisons function
                                 legendloc = "topright",   ## Passed to the SSplotComparisons function
                                 end.yr = NULL,            ## End year of the plot. If is.retro is TRUE, this is ignored and endyrvec is calculated
                                 xlims="default",          ## xlim to zoom on a specific area
                                 mcmc = rep(TRUE,length(models))   ## model numbers to plot mcmc instead of MLE
                                 ){
  ## Plot the list of models against each other.
  ## if model.names is null, the directory names will be used
  oldpar <- par()
  if(is.null(model.names)){
    tmp.names <- sapply(models[1:length(models)], "[[", "path")
    model.names <- gsub(".*/","", tmp.names)
  }
  compare.summary <- SSsummarize(models)
  compare.summary$mcmc <- vector(mode="list",length=length(models))
  for(i in 1:length(models)) {
    compare.summary$mcmc[[i]] <- models[[i]]$mcmc
  }

  endyrvec <- "default"
  ## If it is a retropective plot, compute the end year vector of years so the lines end on the correct years
  if(is.retro){
    endyrvec <- compare.summary$endyrs + 1 + (0:-(length(models) - 1))
  }else{
    if(!is.null(end.yr)){
      endyrvec <- end.yr
    }
  }
  if(is.null(densitynames)){
    SSplotComparisons(compare.summary,
                      subplots = subplots,
                      legend = legend,
                      legendlabels = model.names,
                      legendloc = legendloc,
                      indexPlotEach = indexPlotEach,
                      indexUncertainty = indexUncertainty,
                      indexfleets = indexfleets,
                      endyrvec = endyrvec,
                      xlim=xlims,
                      mcmc = TRUE,
                      new=FALSE,
                      labels = c("Year", "Spawning biomass (t)", "Relative spawning biomass", "Age-0 recruits (1,000s)",
                                 "Recruitment deviations", "Index", "Log index", "SPR ratio", "Density",
                                 "Management target", "", "Spawning output",
                                 "Harvest rate"))
  }else{
    SSplotComparisons(compare.summary,
                      subplots = subplots,
                      legend = legend,
                      legendlabels = model.names,
                      legendloc = legendloc,
                      densitynames = densitynames,
                      indexPlotEach = indexPlotEach,
                      indexUncertainty = indexUncertainty,
                      indexfleets = indexfleets,
                      endyrvec = endyrvec,
                      xlim=xlims,
                      mcmc = mcmc,
                      new=FALSE,
                      labels = c("Year", "Spawning biomass (t)", "Relative spawning biomass", "Age-0 recruits (1,000s)",
                                 "Recruitment deviations", "Index", "Log index", "SPR ratio", "Density",
                                 "Management target", "", "Spawning output",
                                 "Harvest rate"))
  }
  par <- oldpar
}
