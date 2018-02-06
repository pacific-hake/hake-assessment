# This is a little hardwired, but that's okay as it does not get
#  automatically run by other code. It's run once by someone
#  to get the catch levels to put into forecast-catch-levels.r.
# Aaron updated numbers 1/23/17

SSdir <- "C:/Users/Aaron.Berger/Documents/GitHub/hake-assessment/models"

baseModel <- "45_BasePreSRG_v4_mcmc_forecasts"

######################################
# Forecasts
# Figure out median recommended (default) catches for 2017-2019
out <- read.table(file.path(SSdir,baseModel,
                            "DecisionTable/defaultHR/derived_posteriors.sso"),
                  header=T)
# no catches input into forecast.ss file to determine median 2017 catch
median(out$ForeCatch_2017)   #969840 1/23/17
# now fix this catch for 2017 in the forecast.ss file, run mceval, and find
#  median 2018 catch
out <- read.table(file.path(SSdir,baseModel,
                            "DecisionTable/defaultHR/derived_posteriors.sso"),
                  header=T)
median(out$ForeCatch_2018)   #843566  1/23/17
# now fix this catch for 2018 in the forecast.ss file, run mceval, and find
#  median 2019 catch
out <- read.table(file.path(SSdir,baseModel,
                            "DecisionTable/defaultHR/derived_posteriors.sso"),
                  header=T)
head(out[,c("ForeCatch_2017","ForeCatch_2018")])
median(out$ForeCatch_2019)   #679881   1/23/17
# run mceval
# double check
out <- read.table(file.path(SSdir,baseModel,
                            "DecisionTable/defaultHR/derived_posteriors.sso"),
                  header=T)
head(out[,c("ForeCatch_2017","ForeCatch_2018","ForeCatch_2019")])


# median SPR=100
# set the catch for 2017, run mceval, until the median SPRratio_2017 is really
#  close to 1
out <- read.table(file.path(SSdir,baseModel,
                            "DecisionTable/SPR100/derived_posteriors.sso"),
                  header=T)
head(out[,c("ForeCatch_2017","ForeCatch_2018","ForeCatch_2019")])
median(out$SPRratio_2017)   #934000  =  0.9996     1/23/17
# set the catch for 2018, run mceval, until the median SPRratio_2018 is really
#  close to 1
out <- read.table(file.path(SSdir,baseModel,
                            "DecisionTable/SPR100/derived_posteriors.sso"),
                  header=T)
head(out[,c("ForeCatch_2017","ForeCatch_2018","ForeCatch_2019")])
median(out$SPRratio_2018)   #848000  = 0.9999     1/23/17
# set the catch for 2019, run mceval, until the median SPRratio_2019 is really
#  close to 1
out <- read.table(file.path(SSdir,baseModel,
                            "DecisionTable/SPR100/derived_posteriors.sso"),
                  header=T)
head(out[,c("ForeCatch_2017","ForeCatch_2018","ForeCatch_2019")])
median(out$SPRratio_2019)   #698000  = 1.0001     1/23/17


# constant catch
#  set the catch for 2017, run mceval, until the median foreCatch_2018 is the same
out <- read.table(file.path(SSdir,baseModel,
                            "DecisionTable/constantCatch/derived_posteriors.sso"),
                  header=T)
head(out[,c("ForeCatch_2017","ForeCatch_2018","ForeCatch_2019")])
median(out$ForeCatch_2018)   #866263      1/23/27
median(out$ForeCatch_2019)   #683014      1/23/17





