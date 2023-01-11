# This is a list of vectors of base model groups (See below). This should
# typically be only a single directory name, but is kept the same format as
# the other types for code simplicity
base_models_dirs <- "01-base"
# This is a list of vectors of bridge groups (bridge models that will be
# plotted against each other). It can be `NULL` if you want it to be ignored.
# `prepend_to_bridge` is the same length as the number of groups in
# `bridge_models_dirs` and for those groups set to `TRUE`, last year's base
# model will be prepended to the group.
# See `set_dirs()`
bridge_models_dirs <-
  list(c("01-updated-ss-exe",
         "02-add-new-catch",
         "03-add-new-weight-at-age",
         "04-add-survey-age-2-plus",
         "05-add-survey-age-1",
         "06-add-fishery-ages"))
bridge_models_desc <-
  list(c("Update Stock Synthesis version to 3.30.20",
         paste0("Add ", last_data_yr, " catch"),
         paste0("Add ", last_data_yr, " weight-at-age"),
         "Add age-2+ acoustic survey",
         "Add age-1 acoustic survey",
         paste0("Add ", last_data_yr, " fishery age comps")))
# Prepend the base model to each group? TRUE or FALSE
prepend_to_bridge <- TRUE

# This is a list of vectors of sensitivity groups (sensitivity models that
# will be plotted against each other). It can be `NULL` if you want it to be
# ignored.
# The base mode will be prepended to each group by the function.
# See `set_dirs()`
sens_models_dirs <-
  list(c("01-h-prior-mean-low",
         "02-h-fix-high",
         "03-sigma-r-fix-low",
         "04-sigma-r-fix-high",
         "05-m-02-sd",
         "06-m-03-sd",
         "07-m-hamel-prior")
       c("08-age-1-survey",
         "09-comp-weight-harmonic-mean"),
       c("10-tv-select-phi-extra-low",
         "11-tv-select-phi-low",
         "12-tv-select-phi-high"),
       c("13-max-sel-age-5",
         "14-max-sel-age-7",
         "15-max-sel-age-8",
         "16-zero-sum-constraint"))
#sens_models_desc <- NULL
sens_models_desc <-
  list(c("Steepness Mean Prior Low (0.5)",
         "Steepness Fix 1.0",
         "Sigma R 1.0",
         "Sigma R 1.6",
         "Natural Mortality (SD=0.2)",
         "Natural Mortality (SD=0.3)",
         "Natural Mortality (Hamel prior)"),
       c("Remove Age 1 Index",
         "Downweight Fishery Comps"),
       c("Phi t.v. selectivity (0.21)",
         "Phi t.v. selectivity (0.70)",
         "Phi t.v. selectivity (2.10)"),
       c("Max. age selectivity 5",
         "Max. age selectivity 7",
         "Max. age selectivity 8",
         "Recdevs sum to zero"))

sens_models_dirs <- NULL
sens_models_desc <- NULL

request_models_dirs <- NULL
request_models_desc <- NULL

test_models_dirs <- NULL
test_models_desc <- NULL

retro_models_dirs <- NULL
retro_models_desc <- NULL

drs <- set_dirs(models_dir = models_dir,
                last_yr_models_dir = last_yr_models_dir,
                base_models_dirs = base_models_dirs,
                bridge_models_dirs = bridge_models_dirs,
                sens_models_dirs = sens_models_dirs,
                request_models_dirs = request_models_dirs,
                test_models_dirs = test_models_dirs,
                retro_models_dirs = retro_models_dirs,
                prepend_to_bridge = prepend_to_bridge)

models <- model_setup(drs = drs,
                      bridge_models_desc = bridge_models_desc,
                      sens_models_desc = sens_models_desc,
                      request_models_desc = request_models_desc,
                      test_models_desc = test_models_desc,
                      retro_models_desc = retro_models_desc)
