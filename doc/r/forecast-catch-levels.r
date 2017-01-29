## -----------------------------------------------------------------------------
## The forecasting yrs and probs can be set to whatever is required, the
##  code is set up to automatically accomodate changes
## -----------------------------------------------------------------------------
forecast.yrs <- end.yr:(end.yr + 2)
forecast.probs <- c(0.05, 0.25, 0.5, 0.75, 0.95)

## -----------------------------------------------------------------------------
## catch.levels is a list of N lists of catch levels with 3 items each:
##  1. values for catch to forecast
##  2. their pretty names
##  3. their directory names
## Each element of the list is a list of length equal to the
## number of elements in forcast.yrs.
## Constant catch values are somewhat arbitrary, but choosen to span a range
##    given recent TACs (levels: 01, 02, 03, 05)
## Contant catch values set at the most recent TAC (level 04)
## Catches specific to particular situations (SPR100, Default HR, and Stable
##    catch) are manually solved for following forecast_find_DecisionTable.R
##    located in .../doc/r

## -----------------------------------------------------------------------------
catch.levels <-
  list(list(rep(0.01, 3), "No Fishing", "01-0"),
       list(rep(180000, 3), "180,000 t", "02-180000"),
       list(rep(350000, 3), "350,000 t", "03-350000"),
       list(rep(497500, 3), "2016 TAC: 497,500 t", "04-497500"),
       list(rep(600000, 3), "600,000 t", "05-600000"),
       list(c(934000, 848000, 698000), "FI=100%", "06-spr-100"),
       list(c(969840, 843566, 679881), "Default Harvest Policy: 969,840 t", "07-default-hr"),
       list(c(866263, 866263, 683014), "Stable Catch", "08-stable-catch"))
        
## -----------------------------------------------------------------------------
## Indicies for the forecasts list, which list items above are the TAC case and
##  default policy case
## This is used in the one-page summary and a plot comparing several catch cases
## -----------------------------------------------------------------------------
catch.tac.ind <- 4
catch.default.policy.ind <- 7
catch.default.policy <- catch.levels[[catch.default.policy.ind]][[1]]

## The verify.catch.levels function is in verify.r
verify.catch.levels(catch.levels,
                    c(catch.tac.ind, catch.default.policy.ind),
                    forecast.yrs)
