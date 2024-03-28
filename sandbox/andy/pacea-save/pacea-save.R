# Superceded now by pacea_save().

# Saving some hake results to go into the pacea R package. Will maybe make this a
#  simple function once Chris has package built, but it only needs to be run once
#  a year. Then copy over results files into pacea/data-raw/groundfish and run
#  hake.R there.

# From hake-assessment.Rnw (loads in all models, which I don't need (just need base), but is quick now)
source(here::here("R/all.R"))
source(file.path(rootd_doc, "load-models.R"))
source(file.path(rootd_r, "custom-knitr-variables.R"))


# This runs fine, so using some to use in plot.pacea_t:
## make.mcmc.recruitment.plot(base_model,
##                            start_yr = 1966,
##                            equil.yr = unfished_eq_yr,
##                            samples = NULL,
##                            rescale = TRUE,
##                            traceplot = FALSE,
##                            end_yr = assess_yr + 2,
##                            auto.xaxis = FALSE,
##                            plot_R0 = TRUE)

# Recruitment

recr <- create_group_df_recr(list(base_model), "base model")$d

recr <- dplyr::select(recr, -c("model",
                               "rmean"))

stopifnot(colnames(recr) == c("year",
                              "rlower",
                              "rmed"   ,
                              "rupper"))

colnames(recr) <- c("year",
                    "low",
                    "median",
                    "high")

hake_recruitment_new <- recr           # Call it new for consistency in pacea code

save(hake_recruitment_new,
     file = "hake_recruitment_new.rda")

# Spawning biomass

biomass <- create_group_df_biomass(list(base_model), "base model")$d

biomass <- dplyr::select(biomass, -"model")

stopifnot(colnames(biomass) == c("year",
                                 "slower",
                                 "smed",
                                 "supper"))

colnames(biomass) <- c("year",
                       "low",
                       "median",
                       "high")

hake_biomass_new <- biomass           # Call it new for consistency in pacea code

save(hake_biomass_new,
     file = "hake_biomass_new.rda")

# Recruitment scaled by 2010 recruitment
# Need to do from the full mcmc's

dat <- as_tibble(base_model$mcmc) %>%
  select(c("Recr_Virgin",
           paste0("Recr_", start_yr):paste0("Recr_", end_yr))) / 1e6  # convert
                                        # from thousands to billions
dat <- dat / dat$"Recr_2010"

dat <- select(dat, -"Recr_Virgin")

names(dat) = gsub(pattern = "Recr_",
                  replacement = "",
                  x = names(dat))

low <- apply(dat,
             2,
             quantile,
             probs = 0.025)

median <- apply(dat,
                2,
                median)

high <- apply(dat,
              2,
              quantile,
              probs = 0.975)

hake_recruitment_over_2010_new <- tibble("year" = as.numeric(names(dat)),
                                         "low" = low,
                                         "median" = median,
                                         "high" = high)

save(hake_recruitment_over_2010_new,
     file = "hake_recruitment_over_2010_new.rda")

# Recruitment scaled by R0 recruitment
# Need to do from the full mcmc's

dat <- as_tibble(base_model$mcmc) %>%
  select(c("Recr_Virgin",
           paste0("Recr_", start_yr):paste0("Recr_", end_yr))) / 1e6  # convert
                                        # from thousands to billions
dat <- dat / dat$"Recr_Virgin"

dat <- select(dat, -"Recr_Virgin")

names(dat) = gsub(pattern = "Recr_",
                  replacement = "",
                  x = names(dat))

low <- apply(dat,
             2,
             quantile,
             probs = 0.025)

median <- apply(dat,
                2,
                median)

high <- apply(dat,
              2,
              quantile,
              probs = 0.975)

hake_recruitment_over_R0_new <- tibble("year" = as.numeric(names(dat)),
                                         "low" = low,
                                         "median" = median,
                                         "high" = high)

save(hake_recruitment_over_R0_new,
     file = "hake_recruitment_over_R0_new.rda")
