# Adapted code originally provided by Cole Monnahan to run for Hake 2021 assessment model procedures
# See https://github.com/pacific-hake/hake-assessment/issues/684#issuecomment-759683974
# AMB  1/11/2021
# Two code chunks included:
#  1) main chunk to run base model (including diagnostic workups)
#  2) a shorter chunk for running one-off sensitivity evaluations
#
# Remember to copy over the model files (data, control, forecast, wt-at-age, starter,
#  and ss.exe files to new folder)
# Ensure starter file lines match the following:
#     0 # MCeval burn interval
#     1 # MCeval thin interval
#
# For 2021 Pacific Hake stock assessment model runs, let's plan to shoot for about 8,000 samples,
# which is similar to last year's trial (sensitivity) runs with adnuts.
# Note that the algebra is [(core-1) * (x-250)] where core is your # of cores, x is your iteration,
# and 250 is the warmup.
# For Aaron with 11, or 12-1, cores and a warmup of 250 I will use
#  978 iterations as [11 * (978-250) = 8,008]
# Let's leave the warmup period, or "burn-in", at 250 per core (i.e., chain).

devtools::install_github('Cole-Monnahan-NOAA/adnuts', ref = 'dev')
library(adnuts)
library(snowfall)
library(rstan)
library(shinystan)
library(matrixcalc)

#####################################################################################
## 1 main chunk used to fully explore diagnostics and run main model (e.g., base model)
##
start_time <- Sys.time()
## Chains to run in parallel
reps <- parallel::detectCores() - 1
set.seed(352)
seeds <- sample(1:1e4, size = reps)
pth <- "."
exe <- "ss"
rdata_file <- "hake.Rdata"
## First optimize the model to make sure the Hessian is good (and optimize without
#bias adjustment turned on - i.e., mcmc used)
system(paste0(exe, " -nox -iprint 200 -mcmc 15"))
## Then run parallel RWM chains as a first test to ensure
## mcmc itself is working properly, or that model is converging in mcmc space
thin <- 10
## iter is per core
iter <- 100 * thin
warmup <- iter / 4
## Start chains from MLE
inits <- NULL
pilot <- sample_rwm(model = exe,
                     iter = iter,
                     thin = thin,
                     seeds = seeds,
                     init = inits,
                     parallel = TRUE,
                     chains = reps,
                     warmup = warmup,
                     path = pth,
                     cores = reps)
save.image(file = rdata_file)
## Check convergence and slow mixing parameters
mon <- monitor(pilot$samples,
               warmup = pilot$warmup,
               print = FALSE)
## max(mon[,'Rhat'])
## min(mon[,'n_eff'])
## Examine the slowest mixing parameters
slow <- names(sort(mon[,"n_eff"]))[1:8]
pairs_admb(fit = pilot, pars = slow)
pairs_admb(fit = pilot, pars = c("MGparm[1]", "SR_parm[1]", "SR_parm[2]"))
## After regularizing run NUTS chains. First reoptimize to get the
## correct mass matrix for NUTS. Note the -hbf 1 argument. This is a
## technical requirement b/c NUTS uses a different set of bounding
## functions and thus the mass matrix will be different.
system(paste0(exe, " -hbf 1 -nox -iprint 200 -mcmc 15"))
save.image(file = rdata_file)
## Use default MLE covariance (mass matrix) and short parallel NUTS chains
## started from the MLE. Recall iter is number per core running in parallel.
nuts.mle <-
  sample_nuts(model = exe,
              iter = 500,
              init = NULL,
              seeds = seeds,
              chains = reps,
              warmup = 100,
              path = pth,
              cores = reps,
              control = list(metric = "mle",
                             adapt_delta = 0.8))
save.image(file = rdata_file)
## Check for issues like slow mixing, divergences, max treedepths with
## ShinyStan and pairs_admb as above. Fix using the shiny app and rerun this part as needed.
## launch_shinyadmb(nuts.mle)

## Once acceptable, run again for inference using updated mass matrix. Increase
## adapt_delta toward 1 if you have divergences (runs will take longer).
## Note this is in unbounded parameter space
mass <- nuts.mle$covar.est
inits <- sample_inits(nuts.mle, reps)
## The following, nuts.updated, is used for inferences
nuts.updated <- sample_nuts(model = exe,
                            iter = 1000,
                            init = inits,
                            seeds = seeds,
                            chains = reps,
                            warmup = 250,
                            path = pth,
                            cores = reps,
                            mceval = TRUE,
                            control = list(metric = mass,
                                           adapt_delta = 0.9))
save.image(file = rdata_file)
end_time <- Sys.time()
cat("Elapsed time: ", end_time - start_time, "\n")
# you can launch the shiny app to investigate diagnostics:
launch_shinyadmb(nuts.updated)

#######################################################################################
# 2 shortened version for running sensitivity models (if fail, then may need to step through
# above procedures)
#
start_time <- Sys.time()
# Chains to run in parallel
reps <- parallel::detectCores() - 1
set.seed(352)
seeds <- sample(1:1e4, size = reps)
pth <- "."
exe <- "ss"
rdata_file <- "hake.Rdata"
# First re-optimize to get the correct mass matrix for NUTS (and without
# bias adjustment turned on). Note the -hbf 1 argument.
# This is a technical requirement b/c NUTS uses a different set of bounding
# functions and thus the mass matrix will be different.
system(paste0(exe, " -hbf 1 -nox -iprint 200 -mcmc 15"))
save.image(file = rdata_file)
# Use default MLE covariance (mass matrix) and short parallel NUTS chains
# started from the MLE. Recall iter is number per core running in parallel.
nuts.mle <- sample_nuts(model = exe,
                        iter = 100,
                        init = NULL,
                        seeds = seeds,
                        chains = reps,
                        warmup = 50,
                        path = pth,
                        cores = reps,
                        control = list(metric = "mle",
                                       adapt_delta = 0.8))
save.image(file = rdata_file)
## Check for issues like slow mixing, divergences, max treedepths with
## ShinyStan and pairs_admb as above. Fix using the shiny app and rerun this part as needed.
## launch_shinyadmb(nuts.mle)

## Once acceptable, run again for inference using updated mass matrix. Increase
## adapt_delta toward 1 if you have divergences (runs will take longer).
## Note this is in unbounded parameter space
mass <- nuts.mle$covar.est
inits <- sample_inits(nuts.mle, reps)
## The following, nuts.updated, is used for inferences
nuts.updated <- sample_nuts(model = exe,
                            iter = 1000,
                            init = inits,
                            seeds = seeds,
                            chains = reps,
                            warmup = 250,
                            path = pth,
                            cores = reps,
                            mceval = TRUE,
                            control = list(metric = mass,
                                           adapt_delta = 0.9))
save.image(file = rdata_file)
end_time <- Sys.time()
cat("Elapsed time: ", end_time - start_time, "\n")
# you can launch the shiny app to investiage diagnostics:
# launch_shinyadmb(nuts.updated)
