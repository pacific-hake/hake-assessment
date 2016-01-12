make.decision.table <- function(model,                  ## model is an mcmc run and is the output of the r4ss package's function SSgetMCMC
                                xcaption   = "default", ## Caption to use
                                xlabel     = "default", ## Latex label to use
                                font.size  = 9,         ## Size of the font for the table
                                space.size = 10,        ## Size of the spaces for the table
                                which      = "biomass"  ## Which type to build. "biomass" or "spr".
                                ){
  ## Returns an xtable in the proper format for the executive summary decision tables

  if(which != "biomass" & which != "spr"){
    stop("make.decisions.table: Error - type '",which,"' is not implemented. Stopping...\n\n")
  }
  if(which == "biomass"){
    forecast <- model$forecasts$biomass
    table.header <- "Beginning of year relative spawning biomass"
  }else{
    forecast <- model$forecasts$spr
    table.header <- "Fishing Intensity"
  }

  ## tab.letters are the letters in the table, one for each forecast management action
  ## and a blank for all but the first year in a management action
  tab.letters <- NULL
  next.ind <- 1
  for(i in 1:length(forecast)){
    tab.letters[next.ind] <- letters[i]
    next.ind <- next.ind + 1
    for(j in 1:(nrow(forecast[[i]])-1)){
      tab.letters[next.ind] <- ""
      next.ind <- next.ind + 1
    }
  }

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
  forecast.tab <- cbind(tab.letters, yrs, fmt0(c.levels), forecast.tab)
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
  for(i in 1:length(quant.levels)){
    quant.string <- paste0(quant.string, "& ", quant.levels[i], " ")
  }
  addtorow$command <- c(paste0("\\toprule \n",
                               "\\multicolumn{3}{c}{Within model quantile} ",
                               quant.string, " \\\\ ",
                               "\\hline",
                               " & Year & Catch (t) & \\multicolumn{5}{c}{",table.header,"} \\\\ ",
                               "\\midrule \n"),
                        "\\bottomrule \n")
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
                      align=get.align(ncol(forecast.tab), first.left=FALSE, just="c")),
               caption.placement = "top",
               include.rownames=FALSE,
               include.colnames=FALSE,
               sanitize.text.function=function(x){x},
               size=size.string,
               add.to.row=addtorow,
               table.placement="H",
               hline.after=NULL,
               booktabs=TRUE))
}

make.risk.table <- function(model,                  ## model is an mcmc run and is the output of the r4ss package's function SSgetMCMC
                            forecast.yrs,           ## A vector of the years which ere forecast.
                            index = 1,              ## Index for which risk data to use. e.g. 1 = second forecast year compared to the first.
                                                    ##  If there were N forecast years, this can be from 1 to N-1.
                            xcaption   = "default", ## Caption to use
                            xlabel     = "default", ## Latex label to use
                            font.size  = 9,         ## Size of the font for the table
                            space.size = 10         ## Size of the spaces for the table
                            ){
  ## Returns an xtable in the proper format for the executive summary risk tables

  risk <- t(as.data.frame(model$risks[[index]]))
  rownames(risk) <- NULL
  ## Fix tiny catch of less than 1 to zero
  risk[risk[,1] < 1,] <- 0
  risk[,-1] <- paste0(fmt0(risk[,-1] * 100), "\\%")
  ##risk[,1] <- fmt0(risk[,1])
  
  addtorow <- list()
  addtorow$pos <- list()
  addtorow$pos[[1]] <- -1
  addtorow$pos[[2]] <- nrow(risk)
  addtorow$command <- c(paste0("\\toprule \n",
                               "Catch in ",forecast.yrs[index]," ",
                               "& Probability\nB\\subscr{",forecast.yrs[index+1],"}<B\\subscr{",forecast.yrs[index],"} ",
                               "& Probability\nB\\subscr{",forecast.yrs[index+1],"}<B\\subscr{40\\%} ",
                               "& Probability\nB\\subscr{",forecast.yrs[index+1],"}<B\\subscr{25\\%}",
                               "& Probability\nB\\subscr{",forecast.yrs[index+1],"}<B\\subscr{10\\%}",
                               "& Probability\nFishing intensity\nin ",forecast.yrs[index],"\n>40\\% Target",
                               "& Probability\n",forecast.yrs[index+1]," Catch Target\n < ",forecast.yrs[index]," Catch",
                               "\\midrule \n"),
                        "\\bottomrule \n")

  ## Make the size string for font and space size
  size.string <- paste0("\\fontsize{",font.size,"}{",space.size,"}\\selectfont")
  return(print(xtable(risk,
                      caption=xcaption,
                      label=xlabel,
                      align=get.align(ncol(risk), first.left=FALSE, just="c")),
               caption.placement = "top",
               include.rownames=FALSE,
               include.colnames=FALSE,
               sanitize.text.function=function(x){x},
               size=size.string,
               add.to.row=addtorow,
               table.placement="H",
               hline.after=NULL,
               booktabs=TRUE))
}
