# This is a little hardwired, but that's okay as it does not get
#  automatically run by other code. It's run once by someone
#  to get the catch levels to put into forecast-catch-levels.r.
# Aaron updated numbers 1/23/17
# Andy updating numbers 2/6/18.
# Andy calculating numbers for 2018 alternative MCMC run. 3/1/18.

# SSdir <- "C:/Users/Aaron.Berger/Documents/GitHub/hake-assessment/models"
SSdir <- "../../models"
model <- "2018.40.30_fecundity_matrix2"

######################################
# Forecasts
# Calculate the median recommended (default) catches for 2018-2020
# Copy  model/mcmc/ files
# into new DecisionTable/defaultHR/
out <- read.table(file.path(SSdir,model,
                            "DecisionTable/defaultHR/derived_posteriors.sso"),
                  header=T)
# with no catch specified for 2018 in forecast.ss it determines the median
#  2018 catch.
median(out$ForeCatch_2018)   # 583970 3/1/18
# now fix this catch for 2018 near the end of the forecast.ss file (keeping
#  a line starting with -9999 after the new catch)
# _Yr Seas Fleet Catch(or_F)
# 2018 1 1 583970

# ss3 -mceval
#  to find median 2019 catch:
out <- read.table(file.path(SSdir,model,
                            "DecisionTable/defaultHR/derived_posteriors.sso"),
                  header=T)
median(out$ForeCatch_2019)   #517889  3/1/18

# now fix this catch for 2019 in the forecast.ss file, run mceval, and find
#  median 2020 catch

out <- read.table(file.path(SSdir,model,
                            "DecisionTable/defaultHR/derived_posteriors.sso"),
                  header=T)
head(out[,c("ForeCatch_2018","ForeCatch_2019")])
median(out$ForeCatch_2020)   # 473043   3/1/18

# run mceval after putting in 2020 median catch (though not really needed)
# double check
out <- read.table(file.path(SSdir,model,
                            "DecisionTable/defaultHR/derived_posteriors.sso"),
                  header=T)
head(out[,c("ForeCatch_2018","ForeCatch_2019","ForeCatch_2020")])
median(out$ForeCatch_2020)   #473043


# median SPR=100
# Copy  model/mcmc/ files into new DecisionTable/SPR100

# Set the catch for 2018 in forecast.ss, run mceval, iterate until the
#  median SPRratio_2018 is really close to 1
out <- read.table(file.path(SSdir,model,
                            "DecisionTable/SPR100/derived_posteriors.sso"),
                  header=T)
head(out[,c("ForeCatch_2018","ForeCatch_2019","ForeCatch_2020")])
median(out$SPRratio_2018)   #668000 = 0.999996  3/1/18.
# 3/1/18:
# catch    median(out$SPRratio_2018)
# 638000   0.982207
# 650000   0.989483
# 668000   0.999996 **closest**
# 669000   1.00058
# So doing three sig figs (as Aaron did last year), closest to 1 is
#  668000

# set the catch for 2019, run mceval, until the median SPRratio_2019 is really
#  close to 1
out <- read.table(file.path(SSdir,model,
                            "DecisionTable/SPR100/derived_posteriors.sso"),
                  header=T)
head(out[,c("ForeCatch_2018","ForeCatch_2019","ForeCatch_2020")])
median(out$SPRratio_2019)   #582000  = 0.9999895     3/1/18
# 3/1/18:
# 2019catch    median(out$SPRratio_2019)
# 550000       0.977046
# 700000       1.071985 (D'oh)
# 600000       1.01141
# 584000       1.001365
# 582000       0.9999895  **closest**
# 583000       1.00068

# set the catch for 2020, run mceval, until the median SPRratio_2020 is really
#  close to 1
out <- read.table(file.path(SSdir,model,
                            "DecisionTable/SPR100/derived_posteriors.sso"),
                  header=T)
head(out[,c("ForeCatch_2018","ForeCatch_2019","ForeCatch_2020")])
median(out$SPRratio_2020)   # 535000  = 1.000885   3/1/18
# 3/1/18:
# 2020catch    median(out$SPRratio_2020)
# 580000       1.03258
# 530000       0.996165
# 536000       1.001095
# 535000       1.000885   **closest**
# 534000       0.9990665



# constant catch - create constantCatch/ directory.
#  set the catch for 2018, run mceval, until the median foreCatch_2019 is the same
out <- read.table(file.path(SSdir,model,
                            "DecisionTable/constantCatch/derived_posteriors.sso"),
                  header=T)
head(out[,c("ForeCatch_2018","ForeCatch_2019","ForeCatch_2020")])
median(out$ForeCatch_2019)   # 531342   3/1/18
median(out$ForeCatch_2020)   # 477594   3/1/18

# 3/1/18:
# 2018catch  2019catch
# 540000     528958
# 535000     530420
# 533000     530960
# 531400     531328.5
# 531380     531333.5
# 531342     531342  **closest**

