make.decision.table <- function(model,                  ## model is an mcmc run and is the output of the r4ss package's function SSgetMCMC
                                xcaption   = "default", ## Caption to use
                                xlabel     = "default", ## Latex label to use
                                font.size  = 9,         ## Size of the font for the table
                                space.size = 10,        ## Size of the spaces for the table
                                which      = "biomass", ## Which type to build. "biomass" or "spr".
                                placement = "H"         ## Placement of table
                                ){
  ## Returns an xtable in the proper format for the executive summary decision tables

  if(which != "biomass" & which != "spr"){
    stop("make.decisions.table: Error - type '",which,"' is not implemented. Stopping...\n\n")
  }
  forecast <- model$forecasts[[length(model$forecasts)]]
  forecast.yrs <- as.numeric(names(forecast))

  if(which == "biomass"){
    num.rows <- nrow(forecast[[1]]$biomass)
    table.header <- "\\textbf{Beginning of year relative spawning biomass}"
  }else{
    num.rows <- nrow(forecast[[1]]$spr)
    table.header <- "\\textbf{Relative fishing intensity}"
  }

  ## tab.letters are the letters in the table, one for each forecast management action
  ## and a blank for all but the first year in a management action
  ## additional labels are given for some rows (below letter)
  rows2Label <- c("d","f","g","h")
  rowLabels <- list(c(assess.yr - 1,"TAC"),
                    c("FI=","100\\%"),
                    c("default","HR"),
                    c(paste0("C", assess.yr, "="), paste0("C", assess.yr + 1)))
  tab.letters <- NULL
  next.ind <- 1
  for(i in 1:length(forecast)){
    tab.letters[next.ind] <- paste0(letters[i],":")
    next.ind <- next.ind + 1
    for(j in 1:(num.rows - 1)){
      if(letters[i] %in% rows2Label) {
        lab <- rowLabels[[which(letters[i]==rows2Label)]]
        tab.letters[next.ind] <- lab[j]
      } else {
        tab.letters[next.ind] <- ""
      }
      next.ind <- next.ind + 1
    }
  }

  ## Merge the list elements into a data frame
  ## forecast.tab <- f(do.call("rbind", forecast) * 100)
  if(which == "biomass"){
    forecast.tab <- f(do.call("rbind", lapply(forecast, "[[", "biomass")) * 100)
  }else{
    forecast.tab <- f(do.call("rbind", lapply(forecast, "[[", "spr")) * 100)
  }

  ## Store years for binding later
  yrs <- rownames(forecast.tab)

  ## Append the escaped % symbols
  forecast.tab <- apply(forecast.tab, 2, paste0, "\\%")


  ## Change the quantile levels so they have correct latex escape sequence
  quant.levels <- gsub("%","\\\\%",colnames(forecast.tab))

  ## Set any catch less than 1 to be 0
  ## c.levels <- unlist(catch.levels)
  c.levels <- unlist(lapply(catch.levels, "[[", 1))
  c.levels[c.levels < 1] <- 0
  ## Bind the catch levels and years to the correct rows
  forecast.tab <- cbind(tab.letters, yrs, f(c.levels), forecast.tab)
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
                               "\\multicolumn{3}{|c|}{Management Action} ",quant.ampersands,"\\\\ ",
                               "\\cline{1-3} ",
                               " & Year & Catch (t) & \\multicolumn{",length(quant.levels),"}{c|}{\\multirow{-2}{*}{",table.header,"}} \\\\ ",
                               "\\hline "),
                        "\\hline ")
  ## Add the right number of horizontal lines to make the table break in the correct places
  ## A line is not needed at the bottom explains the (length(forecast)-1) statement.
  for(i in 1:(length(forecast)-1)){
    addtorow$pos[[i+2]] <- i * num.rows
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

make.decision.table.pres <- function(model,                  ## model is an mcmc run and is the output of the r4ss package's function SSgetMCMC
                                     model.inds,             ## indices of the forecast models you want to show
                                     xcaption   = "default", ## Caption to use
                                     xlabel     = "default", ## Latex label to use
                                     font.size  = 9,         ## Size of the font for the table
                                     space.size = 10,        ## Size of the spaces for the table
                                     which      = "biomass", ## Which type to build. "biomass" or "spr".
                                     placement  = "H"        ## Placement of table
                                     ){
  ## Returns an xtable in the proper format for the management presentation in beamer

  if(which != "biomass" & which != "spr"){
    stop("make.decisions.table: Error - type '",which,"' is not implemented. Stopping...\n\n")
  }
  ## The numbers below index several of the forecast catch levels
  forecast <- model$forecasts[[length(model$forecasts)]][model.inds]
  if(which == "biomass"){
    table.header1 <- "\\textbf{Beginning of year}"
    table.header2 <- "\\textbf{relative spawning biomass}"
    forecast.tab <- f(do.call("rbind", lapply(forecast, "[[", "biomass")) * 100)
  }else{
    table.header1 <- "\\textbf{Fishing}"
    table.header2 <- "\\textbf{Intensity}"
    forecast.tab <- f(do.call("rbind", lapply(forecast, "[[", "spr")) * 100)
  }

  ## Store years for binding later
  yrs <- rownames(forecast.tab)

  ## Append the escaped % symbols
  forecast.tab <- apply(forecast.tab, 2, paste0, "\\%")

  ## Change the quantile levels so they have correct latex escape sequence
  quant.levels <- gsub("%","\\\\%",colnames(forecast.tab))

  ## Set any catch less than 1 to be 0
  c.levels <- unlist(lapply(catch.levels[model.inds], "[[", 1))
  c.levels[c.levels < 1] <- 0
  ## Bind the catch levels and years to the correct rows
  row.labs <- rep("", length(model.inds) * 3)
  row.labs[seq(1, length(model.inds) * 3, 3)] <- paste0(letters[model.inds], ":")
  ## row.labs[c(5, 6, 8, 9, 11, 12)] <- c(assess.yr - 1, "TAC", "FI=", "100\\%", "default", "HR")

  forecast.tab <- cbind(row.labs, yrs, f(c.levels), forecast.tab)
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
    if(!is.null(forecast[[i]])){
      if(which == "biomass"){
        addtorow$pos[[i+2]] <- i * nrow(forecast[[i]]$biomass)
      }else{
        addtorow$pos[[i+2]] <- i * nrow(forecast[[i]]$spr)
      }
      addtorow$command <- c(addtorow$command, "\\hline ")
    }
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

make.risk.table <- function(model,                  ## model is an mcmc run and is the output of the r4ss package's function SSgetMCMC
                            forecast.yrs,           ## A vector of the years which ere forecast.
                            index = 1,              ## Index for which risk data to use. e.g. 1 = second forecast year compared to the first.
                                                    ##  If there were N forecast years, this can be from 1 to N-1.
                            xcaption   = "default", ## Caption to use
                            xlabel     = "default", ## Latex label to use
                            font.size  = 9,         ## Size of the font for the table
                            space.size = 10,         ## Size of the spaces for the table
                            placement = "H"       ## Placement of table
                            ){
  ## Returns an xtable in the proper format for the executive summary risk tables
  risk <- model$risks[[index]]
  ## Fix tiny catch of less than 0.49 to zero, only for first (catch) column
  risk[risk[,1] < 0.49, 1] <- 0
  ## Format all columns except catch (1) to be zero decimal points and have a percent sign
  risk[,-1] <- apply(apply(risk[,-1],2,f),2,paste0,"\\%")
  ## Format the catch column (1) to have no decimal points and the thousands separator
  risk[,1] <- f(as.numeric(risk[,1]))
  ## Add letters to the catch for reference with the decision tables
  risk[,1] <- paste0(letters[1:nrow(risk)], ": ", risk[,1])

  addtorow <- list()
  addtorow$pos <- list()
  addtorow$pos[[1]] <- -1
  addtorow$pos[[2]] <- nrow(risk)
  addtorow$command <- c(paste0("\\toprule \n",
                               "\\specialcell{Catch\\\\in ",forecast.yrs[index]," } ",
                               "& \\specialcell{Probability\\\\B\\subscr{",forecast.yrs[index+1],"}<B\\subscr{",forecast.yrs[index],"}} ",
                               "& \\specialcell{Probability\\\\B\\subscr{",forecast.yrs[index+1],"}<B\\subscr{40\\%}} ",
                               "& \\specialcell{Probability\\\\B\\subscr{",forecast.yrs[index+1],"}<B\\subscr{25\\%}} ",
                               "& \\specialcell{Probability\\\\B\\subscr{",forecast.yrs[index+1],"}<B\\subscr{10\\%}} ",
                               "& \\specialcell{Probability\\\\",forecast.yrs[index]," relative\\\\fishing\\\\intensity\\\\>100\\%} ",
                               "& \\specialcell{Probability\\\\",forecast.yrs[index+1]," default\\\\harvest policy\\\\catch\\\\<",forecast.yrs[index]," catch} \\\\ ",
                               "\\midrule \n"),
                        "\\bottomrule \n")

  ## Make the size string for font and space size
  size.string <- paste0("\\fontsize{",font.size,"}{",space.size,"}\\selectfont")
  return(print(xtable(risk,
                      caption=xcaption,
                      label=xlabel,
                      align=get.align(ncol(risk), first.left=TRUE, just="Y")),
               caption.placement = "top",
               include.rownames=FALSE,
               include.colnames=FALSE,
               sanitize.text.function=function(x){x},
               size=size.string,
               add.to.row=addtorow,
               table.placement=placement,
               hline.after=NULL,
               tabular.environment = "tabularx",
               width = "\\textwidth",
               booktabs=TRUE))
}
