plot_multiple_tv_selex_unc <- function(tv.sel.list){
  ## Takes a list of outputs from the calc.tv.selex function, and calls
  ##  make.tv.selex.uncertainty.plot for each of the items, placing
  ##  them side-by-side with single labels for Age and Selectivity by year.
  ## This allows the user to select how they want to break up the plots by year

  oldpar <- par()
  par(mfrow=c(1, length(tv.sel.list)), oma=c(1,1,0,0))
  for(i in 1:length(tv.sel.list)){
    make.tv.selex.uncertainty.plot(tv.sel.list[[i]])
  }
  mtext(side = 1, line = -1, outer = TRUE, text="Age")
  mtext(side = 2, outer = TRUE, text="Selectivity by year")
  par <- oldpar
}

