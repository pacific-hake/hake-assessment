# -----------------------------------------------------------------------------
# The forecasting yrs and probabilities can be set to whatever is required, the
#  code is set up to automatically accommodate changes
#  Change them in all.R
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# catch_levels is a list of N 3-item lists of catch levels with values:
#  1. values for catch to forecast
#  2. their pretty names to appear in the document
#  3. their directory names
# Each element of the list is a list of length equal to the
# number of elements in forecast_yrs.
# See the calc.catch.levels() and fetch.catch.levels() functions
#  is load-models.R for details. The NA's below will be populated in each model
#  by those two functions.

# -----------------------------------------------------------------------------
catch_levels <-
  list(list(rep(0.01, length(forecast_yrs)), "No Fishing", "01-0"),
       list(rep(180000, length(forecast_yrs)), "180,000 t", "02-180000"),
       list(rep(225000, length(forecast_yrs)), "225,000 t", "03-225000"),
       list(rep(270000, length(forecast_yrs)), "270,000 t", "04-270000"),
       list(c(320000, 288000, 259200, 233280), "320,000 t 10% red.", "05-320000-10"),
       list(rep(325000, length(forecast_yrs)), "2021 catch: 325,000 t", "06-325000"),
       list(rep(350000, length(forecast_yrs)), "350,000 t", "07-350000"),
       list(c(350000, 315000, 283500, 255150), "350,000 t 10% red.", "08-350000-10"),
       list(rep(380000, length(forecast_yrs)), "380,000 t", "09-380000"),
       list(c(380000, 342000, 307800, 277020), "380,000 t 10% red.", "10-380000-10"),
       list(rep(430000, length(forecast_yrs)), "430,000 t", "11-430000"),
       list(rep(473880, length(forecast_yrs)), "2022 TAC: 545,880 t", "12-545000"),
       list(rep(NA, length(forecast_yrs)), "FI=100%", "13-spr-100"),
       list(rep(NA, length(forecast_yrs)), "Default Harvest Policy", "14-default-hr"),
       list(rep(NA, length(forecast_yrs)), "Stable Catch", "15-stable-catch"))

# -----------------------------------------------------------------------------
# Indices for the forecasts list, which list items above are the TAC case and
#  default policy case
# This is used in the one-page summary and a plot comparing several catch cases,
#  and elsewhere
# -----------------------------------------------------------------------------
catch.levels.num <- length(catch_levels)
catch.actual.ind <- 6
catch.tac.ind <- 12
catch.spr100.ind <- 13
catch.default.policy.ind <- 14
catch.stable.ind <- 15
catch.reduction.rows <- c(5, 8, 10)
catch.constant.rows <- c(1, 2, 3, 4, 6, 7, 9, 11, 12)
catch.constant.str <- paste(letters[catch.constant.rows], collapse = ", ")
catch.reduction.str <- paste(letters[catch.reduction.rows], collapse = ", ")
