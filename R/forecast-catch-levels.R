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
##  given recent TACs (levels: 01, 02, 03, 05), and, for 2018 onwards, the
##  previous year's catch
## Contant catch values set at the most recent TAC (level 04)
## 3 additional catch levels will be added for each model,
##  see the calc.catch.levels() and fetch.catch.levels() functions
##  is load-models.R for details. The NA's below will be populated in each model
##  by the routines.

## -----------------------------------------------------------------------------
catch.levels <-
  list(list(rep(0.01, 3), "No Fishing", "01-0"),
       list(rep(180000, 3), "180,000 t", "02-180000"),
       list(rep(350000, 3), "350,000 t", "03-350000"),
       list(rep(410000, 3), "2018 catch: 410,000 t", "04-410000"),
       list(rep(597500, 3), "2018 TAC: 597,500 t", "05-597500"),
       list(rep(NA, 3), "FI=100%", "06-spr-100"),
       list(rep(NA, 3), "Default Harvest Policy", "07-default-hr"),
       list(rep(NA, 3), "Stable Catch", "08-stable-catch"))

## -----------------------------------------------------------------------------
## Indices for the forecasts list, which list items above are the TAC case and
##  default policy case
## This is used in the one-page summary and a plot comparing several catch cases
## -----------------------------------------------------------------------------
catch.tac.ind <- 5
catch.default.policy.ind <- 7
