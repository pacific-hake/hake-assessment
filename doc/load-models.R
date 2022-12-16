# This is a list of vectors of bridge groups (bridge models that will be
# plotted against each other). It can be `NULL` if to be ignored.
bridge_models_dirs <-
  list(c("01-new-ss-exe",
         "02-new-catch-age",
         "03-update-survey"),
       c("04-new-wt-at-age",
         "05-age-1-index"))

# This is a list of vectors of bridge groups (bridge models that will be
# plotted against each other). It can be `NULL` if to be ignored.
sens_models_dirs <-
  list(c("01-h-prior-mean-low",
         "02-h-fix-high",
         "03-sigma-r-fix-low",
         "04-sigma-r-fix-high",
         "05-m-02-sd",
         "06-m-03-sd"),
       c("07-age-1-survey",
         "08-comp-weight-harmonic-mean"),
       c("09-tv-select-phi-extra-low",
         "10-tv-select-phi-low",
         "11-tv-select-phi-high"),
       c("12-max-sel-age-5",
         "13-max-sel-age-7",
         "14-max-sel-age-8"))

request_models_dirs <- NULL
test_models_dirs <- NULL
retro_models_dirs <- NULL

drs <- set_dirs(bridge_models_dirs = bridge_models_dirs,
                sens_models_dirs = sens_models_dirs,
                request_models_dirs = request_models_dirs,
                test_models_dirs = test_models_dirs,
                retro_models_dirs = retro_models_dirs,
                prepend_to_bridge = c(TRUE, FALSE))
