## -----------------------------------------------------------------------------
## The forecasting yrs and probs can be set to whatever is required, the
##  code is set up to automatically accomodate changes
## -----------------------------------------------------------------------------
forecast_yrs <- end.yr:(end.yr + 2)
forecast_probs <- c(0.05, 0.25, 0.5, 0.75, 0.95)

## -----------------------------------------------------------------------------
## catch_levels is a list of N lists of catch levels with 3 items each:
##  1. values for catch to forecast
##  2. their pretty names
##  3. their directory names
## Each element of the list is a list of length equal to the
## number of elements in forecast_yrs.
## Constant catch values are somewhat arbitrary, but choosen to span a range
##  given recent TACs (levels: 01, 02, 03, 05), and, for 2018 onwards, the
##  previous year's catch
## Contant catch values set at the most recent TAC (level 04)
## 3 additional catch levels will be added for each model,
##  see the calc.catch.levels() and fetch.catch.levels() functions
##  is load-models.R for details. The NA's below will be populated in each model
##  by the routines.

## -----------------------------------------------------------------------------
catch_levels <-
  list(list(rep(0.01, length(forecast_yrs)), "No Fishing", "01-0"),
       list(rep(180000, length(forecast_yrs)), "180,000 t", "02-180000"),
       list(rep(350000, length(forecast_yrs)), "350,000 t", "03-350000"),
       list(rep(410000, length(forecast_yrs)), "2018 catch: 410,000 t", "04-410000"),
       list(rep(500000, length(forecast_yrs)), "500,000 t", "05-500000"),
       list(rep(597500, length(forecast_yrs)), "2019 TAC: 597,500 t", "06-597500"),
       list(rep(NA, length(forecast_yrs)), "FI=100%", "07-spr-100"),
       list(rep(NA, length(forecast_yrs)), "Default Harvest Policy", "08-default-hr"),
       list(rep(NA, length(forecast_yrs)), "Stable Catch", "09-stable-catch"))

## -----------------------------------------------------------------------------
## Indices for the forecasts list, which list items above are the TAC case and
##  default policy case
## This is used in the one-page summary and a plot comparing several catch cases
## -----------------------------------------------------------------------------
catch.actual.ind <- 4
catch.tac.ind <- 6
catch.spr100.ind <- 7
catch.default.policy.ind <- 8
catch.stable.ind <- 9
catch.constant.rows <- 1:6
catch.constant.str <- paste(letters[catch.constant.rows], collapse = ", ")
