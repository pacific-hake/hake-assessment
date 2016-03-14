## -----------------------------------------------------------------------------
## The forecasting yrs and probs can be set to whatever is required, the
## latex/knitr code is set up to automatically accomodate changes
## -----------------------------------------------------------------------------
forecast.yrs <- end.yr:(end.yr + 2)
forecast.probs <- c(0.05, 0.25, 0.5, 0.75, 0.95)

## -----------------------------------------------------------------------------
## catch.levels is a list of N catch levels to run forecasts for
## Each element of the list is a vector of length the same as the
## number of elements in forcast.yrs
## -----------------------------------------------------------------------------
catch.levels <- list(rep(0.01, 3),
                     rep(180000,3),
                     rep(350000,3),
                     rep(440000,3),
                     rep(500000,3),
                     c(760000,855000,750000),
                     c(804399,889918,785036),
                     c(873000,873000,773907))

## -----------------------------------------------------------------------------
## Index for the forecasts list, which one above is the TAC case?
## This is used in the one-page summary and the plot comparing several catch cases
## -----------------------------------------------------------------------------
catch.tac.ind <- 4
## The catch as calculated using the default harvest policy.
catch.default.policy.ind <- 7
catch.default.policy <- catch.levels[[catch.default.policy.ind]]

## -----------------------------------------------------------------------------
## catch.levels.names is a list of N names for the catch levels given in catch.levels
##  to be used in plots (Pretty names)
## -----------------------------------------------------------------------------
catch.levels.names <- c("No Fishing",
                        "180,000 t",
                        "350,000 t",
                        paste0(last.data.yr, " TAC: 440,000 t"),
                        "500,000 t",
                        "SPR100",
                        paste0("Default: ",fmt0(catch.default.policy[1])," t"),
                        "stableCatch")

## -----------------------------------------------------------------------------
## catch.levels.dir.names is a list of N names for the catch levels given in catch.levels,
##  to be used as the directory names (OS-naming friendly). Use prefixed numbers so that
##  the list order is the same as the directory order.
## -----------------------------------------------------------------------------
catch.levels.dir.names <- c("01-0",
                            "02-180000",
                            "03-350000",
                            "04-440000",
                            "05-500000",
                            "06-spr-100",
                            "07-default-hr",
                            "08-stable-catch")
