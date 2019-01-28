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
##    given recent TACs (levels: 01, 02, 03, 05), and, for 2018 onwards, the previous year's catch
## Contant catch values set at the most recent TAC (level 04)
## Catches specific to particular situations (SPR100, Default HR, and Stable
##    catch) are manually solved for following forecast-find-decision-table.r
##    located in .../doc/r

## -----------------------------------------------------------------------------
catch.levels <-
  list(list(rep(0.01, 3), "No Fishing", "01-0"),
       list(rep(180000, 3), "180,000 t", "02-180000"),
       list(rep(350000, 3), "350,000 t", "03-350000"),
       list(rep(416000, 3), "2018 catch: 416000 t", "04-416000"),
       list(rep(597500, 3), "2018 TAC: 597,500 t", "05-597500"))
## 3 additional catch levels will be added for each model,
##  see the calc.catch.levels() and fetch.catch.levels() functions
##  for details. Each model object will have its own copy of catch.levels.
## The custom rows in order are:
## SPR-100
## Default HR catch
## Stable catch

## -----------------------------------------------------------------------------
## Indices for the forecasts list, which list items above are the TAC case and
##  default policy case
## This is used in the one-page summary and a plot comparing several catch cases
## -----------------------------------------------------------------------------
catch.tac.ind <- 5
catch.default.policy.ind <- 7
