## -----------------------------------------------------------------------------
## Set up the retrospective list and number of retro years for the plot and table.
## -----------------------------------------------------------------------------
retro.yrs <- 1:2
plot.retro.yrs <- 1:2
## retro.yrs <- 1:15
## plot.retro.yrs <- 1:5
## retro.model.names <- c(base.model.name, sapply(plot.retro.yrs, function(x) paste0("-", x, if(x == 1) " year" else " years")))
## ## Need to re-assemble the list with the base as the first element
## retro.list <- list(base.model)
## for(i in plot.retro.yrs){
##   retro.list[[i + 1]] <- models[[base.model.ind]]$retros[[i]]
## }

