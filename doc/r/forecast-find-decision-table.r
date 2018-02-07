# This is a little hardwired, but that's okay as it does not get
#  automatically run by other code. It's run once by someone
#  to get the catch levels to put into forecast-catch-levels.r.
# Aaron updated numbers 1/23/17
# Andy updating numbers 2/6/18.

# SSdir <- "C:/Users/Aaron.Berger/Documents/GitHub/hake-assessment/models"
SSdir <- "../../models"
baseModel <- "2018.40_base_model"

######################################
# Forecasts
# Calculate the median recommended (default) catches for 2018-2020
# Copy  baseModel/mcmc/ files
# into new DecisionTable/defaultHR/
out <- read.table(file.path(SSdir,baseModel,
                            "DecisionTable/defaultHR/derived_posteriors.sso"),
                  header=T)
# no catches input into forecast.ss file to determine median 2018 catch (?)
median(out$ForeCatch_2018)   # 725983.5  2/6/18
# now fix this catch for 2018 near the end of the forecast.ss file (keeping
#  a line starting with -9999)
# _Yr Seas Fleet Catch(or_F)
# 2018 1 1 725983.5

# ss3 -mceval
#  to find median 2019 catch:
out <- read.table(file.path(SSdir,baseModel,
                            "DecisionTable/defaultHR/derived_posteriors.sso"),
                  header=T)
median(out$ForeCatch_2019)   #600991  2/6/18

# now fix this catch for 2019 in the forecast.ss file, run mceval, and find
#  median 2020 catch

out <- read.table(file.path(SSdir,baseModel,
                            "DecisionTable/defaultHR/derived_posteriors.sso"),
                  header=T)
head(out[,c("ForeCatch_2018","ForeCatch_2019")])
median(out$ForeCatch_2020)   #532476.5   2/6/18. Second time 538263.5

# run mceval after putting in 2020 median catch (though not really needed)
# double check
out <- read.table(file.path(SSdir,baseModel,
                            "DecisionTable/defaultHR/derived_posteriors.sso"),
                  header=T)
head(out[,c("ForeCatch_2018","ForeCatch_2019","ForeCatch_2020")])
median(out$ForeCatch_2020)   #532476

# median SPR=100
# Copy  baseModel/mcmc/ files into new DecisionTable/SPR100

# Set the catch for 2018 in forecast.ss, run mceval, iterate until the
#  median SPRratio_2018 is really close to 1
out <- read.table(file.path(SSdir,baseModel,
                            "DecisionTable/SPR100/derived_posteriors.sso"),
                  header=T)
head(out[,c("ForeCatch_2018","ForeCatch_2019","ForeCatch_2020")])
median(out$SPRratio_2018)   #639000  = 1.000057  2/6/18.
# 2/6/18:
# catch    median(out$SPRratio_2018)
# 700000   1.038065
# 650000   1.007145
# 640000   1.000695
# 639000   1.000057   **closest**
# 638000   0.9994135
# So doing three sig figs (as Aaron did last year), closest to 1 is
#  639000

# set the catch for 2019, run mceval, until the median SPRratio_2019 is really
#  close to 1
out <- read.table(file.path(SSdir,baseModel,
                            "DecisionTable/SPR100/derived_posteriors.sso"),
                  header=T)
head(out[,c("ForeCatch_2018","ForeCatch_2019","ForeCatch_2020")])
median(out$SPRratio_2019)   #554000  = 0.9999655     2/6/18
# 2/6/18:
# 2019catch    median(out$SPRratio_2019)
# 639000       1.05902
# 540000       0.9897115
# 555000       1.00141
# 550000       0.997531
# 554000       0.9999655 **closest**


# set the catch for 2020, run mceval, until the median SPRratio_2020 is really
#  close to 1
out <- read.table(file.path(SSdir,baseModel,
                            "DecisionTable/SPR100/derived_posteriors.sso"),
                  header=T)
head(out[,c("ForeCatch_2018","ForeCatch_2019","ForeCatch_2020")])
median(out$SPRratio_2020)   # 509000 = 0.9999575   2/6/18
# 2/6/18:
# 2020catch    median(out$SPRratio_2020)
# 554000       1.035885
# 500000       0.9920595
# 505000       0.99752
# 506000       0.997954
# 510000       1.000805
# 509000       0.9999575 **closest**



**updated years to here**
# constant catch
#  set the catch for 2017, run mceval, until the median foreCatch_2018 is the same
out <- read.table(file.path(SSdir,baseModel,
                            "DecisionTable/constantCatch/derived_posteriors.sso"),
                  header=T)
head(out[,c("ForeCatch_2017","ForeCatch_2018","ForeCatch_2019")])
median(out$ForeCatch_2018)   #866263      1/23/27
median(out$ForeCatch_2019)   #683014      1/23/17





