# This is a little hardwired, but that's okay as it does not get
#  automatically run by other code. It's run once by someone
#  to get the catch levels to put into forecast-catch-levels.r.
# Aaron updated numbers 1/23/17
# Andy updating numbers 2/6/18.
# Andy and Chris updating numbers 1/24/19.

SSdir <- file.path(here::here(), "models")
baseModel <- "2019.02.00_base_model"

######################################
# Forecasts
# Calculate the median recommended (default) catches for 2019-2021
#  to put into Default Harvest Policy in forcast-catch-levels.r
# Copy  baseModel/mcmc/ files
# into new DecisionTable/defaultHR/
out <- read.table(file.path(SSdir,baseModel,
                            "DecisionTable/defaultHR/derived_posteriors.sso"),
                  header=T)
# with no catch specified for 2019 in forecast.ss it determines the median
#  2019 catch.
median(out$ForeCatch_2019)   # 609186 1/24/19
# now fix this catch for 2019 near the end of the forecast.ss file (keeping
#  a line at end starting with -9999)
# _Yr Seas Fleet Catch(or_F)
# 2019 1 1 609186

# ss3 -mceval
#  to find median 2020 catch:
out <- read.table(file.path(SSdir,baseModel,
                            "DecisionTable/defaultHR/derived_posteriors.sso"),
                  header=T)
median(out$ForeCatch_2020)   # 514682 (actually ...1.5)  1/24/19

# now fix this catch for 2020 in the forecast.ss file, run mceval, and find
#  median 2021 catch

out <- read.table(file.path(SSdir,baseModel,
                            "DecisionTable/defaultHR/derived_posteriors.sso"),
                  header=T)
head(out[,c("ForeCatch_2019","ForeCatch_2020")])
median(out$ForeCatch_2021)   # 411668  1/24/19

# run mceval after putting in 2021 median catch (though not really needed)
# double check
out <- read.table(file.path(SSdir,baseModel,
                            "DecisionTable/defaultHR/derived_posteriors.sso"),
                  header=T)
head(out[,c("ForeCatch_2019","ForeCatch_2020","ForeCatch_2021")])
median(out$ForeCatch_2021)   #411668

# median SPR=100
# Copy  baseModel/mcmc/ files into new DecisionTable/SPR100

# Set the catch for 2019 in forecast.ss, run mceval, iterate until the
#  median SPRratio_2019 is really close to 1
out <- read.table(file.path(SSdir,baseModel,
                            "DecisionTable/SPR100/derived_posteriors.sso"),
                  header=T)

head(out[,c("ForeCatch_2019","ForeCatch_2020","ForeCatch_2021")])
median(out$SPRratio_2019)   # 543000  0.9996595   1/24/19
# 1/24/19:
# catch   median(out$SPRratio_2019)
# 700000  1.098525
# 600000  1.039015
# 550000  1.004855
# 545000  1.001155
# 544000  1.000410
# 543000  0.9996595  **closest**
# 540000  0.997409
# Doing three sig figs (as Aaron did for 2017).

# 2/6/18:
# catch    median(out$SPRratio_2018)
# 700000   1.038065
# 650000   1.007145
# 640000   1.000695
# 639000   1.000057   **closest**
# 638000   0.9994135
# So doing three sig figs (as Aaron did last year), closest to 1 is
#  639000

# set the catch for 2020, run mceval, until the median SPRratio_2020 is really
#  close to 1
out <- read.table(file.path(SSdir,baseModel,
                            "DecisionTable/SPR100/derived_posteriors.sso"),
                  header=T)
head(out[,c("ForeCatch_2019","ForeCatch_2020","ForeCatch_2021")])
median(out$SPRratio_2020)   #  478000 =  0.99991  1/24/19
# 1/24/19:
# 2020catch  median(out$SPRratio_2020)
# 450000     0.975583
# 475000     0.9968005
# 477000     0.9990865
# 478000     0.99991  **closest**
# 479000     1.000725
# 480000     1.00123
# 500000     1.01745

# 2/6/18:
# 2019catch    median(out$SPRratio_2019)
# 639000       1.05902
# 540000       0.9897115
# 555000       1.00141
# 550000       0.997531
# 554000       0.9999655 **closest**

# set the catch for 2021, run mceval, until the median SPRratio_2021 is really
#  close to 1
out <- read.table(file.path(SSdir,baseModel,
                            "DecisionTable/SPR100/derived_posteriors.sso"),
                  header=T)
head(out[,c("ForeCatch_2019","ForeCatch_2020","ForeCatch_2021")])
median(out$SPRratio_2021)   # 399000  = 1.000265  1/24/19
# 2/24/19:
# 2021catch  median(out$SPRratio_2021)
# 350000     0.949519
# 390000     0.991492
# 392000     0.9947935
# 398000     0.999231
# 399000     1.000265  **closest**
# 400000     1.001390

# 2/6/18:
# 2020catch    median(out$SPRratio_2020)
# 554000       1.035885
# 500000       0.9920595
# 505000       0.99752
# 506000       0.997954
# 510000       1.000805
# 509000       0.9999575 **closest**

# constant catch - create constantCatch/ directory.
#  set the catch for 2019, run mceval, until the median foreCatch_2020 is the same
out <- read.table(file.path(SSdir,baseModel,
                            "DecisionTable/constantCatch/derived_posteriors.sso"),
                  header=T)
head(out[,c("ForeCatch_2019","ForeCatch_2020","ForeCatch_2021")])
median(out$ForeCatch_2020)   #   534774 1/24/19
median(out$ForeCatch_2021)   #   426014  (rounding up) 1/24/19
# 1/24/19:
# 2019catch  2020catch
# 400000     572271
# 500000     545169.5
# 520000     539278
# 530000     535956
# 532000     535442
# 534500     534840
# 534750     534779.5
# 534770     534775
# 534773     534774
# 534774     534774  **closest**
# 534775     534774

# 2/6/18:
# 2018catch  2019catch
# 400000     688012.5
# 500000     661409
# 600000     634363
# 620000     628865
# 626000     627216.5
# 627000     626942
# 626960     626952.5
# 626954     626954.5  **closest** (rounding down)
# 626955     626954.5


