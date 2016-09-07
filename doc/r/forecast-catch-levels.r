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
## -----------------------------------------------------------------------------
catch.levels <- list(list(rep(0.01,  3), "No Fishing", "01-0"),
                     list(c(760000,855000,750000), "SPR 100", "02-spr-100"))

## -----------------------------------------------------------------------------
## Indicies for the forecasts list, which list items above are the TAC case and
##  default policy case?
## This is used in the one-page summary and a plot comparing several catch cases
## -----------------------------------------------------------------------------
catch.tac.ind <- 1
catch.default.policy.ind <- 2
catch.default.policy <- catch.levels[[catch.default.policy.ind]]

## The verify.catch.levels function is in verify.r
verify.catch.levels(catch.levels,
                    c(catch.tac.ind, catch.default.policy.ind),
                    forecast.yrs)

