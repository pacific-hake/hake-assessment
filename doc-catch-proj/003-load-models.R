# Base model directory name and description. these can be a lists of vectors
# like the other types if necessary
base_models_dirs <- "01-base"
base_models_desc <- "Base model"
# This is a list of vectors of bridge groups (bridge models that will be
# plotted against each other). It can be `NA` if you want it to be ignored.
# `prepend_to_bridge` is the same length as the number of groups in
# `bridge_models_dirs` and for those groups set to `TRUE`, last year's base
# model will be prepended to the group.
# See `set_dirs()`

# Uncomment these if you do not want to load bridge models
bridge_models_dirs <- NA
bridge_models_desc <- NA
prepend_to_bridge <- NA

# This is a list of vectors of sensitivity groups (sensitivity models that
# will be plotted against each other). It can be `NA` if you want it to be
# ignored.
# The base model will be prepended to each group by the function. `set_dirs()`
# This means you must reference the models by one more than it appears to be
# e.g. from this list sens_models[[1]][[2]] will be 01-h-prior-mean-low

# Uncomment these if you do not want to load sensitivity models
sens_models_dirs <- NA
sens_models_desc <- NA

# Uncomment these if you do not want to load request models
request_models_dirs <- NA
request_models_desc <- NA

test_models_dirs <- NA
test_models_desc <- NA

drs <- set_dirs(models_dir = models_dir,
                last_yr_models_dir = last_yr_models_dir,
                base_models_dirs = base_models_dirs,
                bridge_models_dirs = bridge_models_dirs,
                sens_models_dirs = sens_models_dirs,
                request_models_dirs = request_models_dirs,
                test_models_dirs = test_models_dirs,
                prepend_to_bridge = prepend_to_bridge)

if(!exists("models")){
  models <- model_setup(drs = drs,
                        base_models_desc = base_models_desc,
                        bridge_models_desc = bridge_models_desc,
                        sens_models_desc = sens_models_desc,
                        request_models_desc = request_models_desc,
                        test_models_desc = test_models_desc,
                        prepend_to_bridge = prepend_to_bridge)
}

if(!exists("last_yr_base_model")){
  last_yr_base_model <-
    readRDS(file.path(drs$last_yr_base_model_dir,
                      paste0(basename(drs$last_yr_base_model_dir), ".rds")))
}
last_yr_base_model_name <- paste0(last_assess_yr, " Base model")

base_model <- models$base_models_dirs[[1]][[1]]
base_model_name <- attr(base_model, "desc")

request_models <- models$request_models_dirs
if(is.na(request_models)[1]){
  request_models_names <- NA
}else{
  request_models_names <- map(request_models, ~{
    map_chr(.x, ~{
      attr(.x, "desc")
    })
  })
}

test_models <- models$test_models_dirs
if(is.na(test_models)[1]){
  test_models_names <- NA
}else{
  test_models_names <- map(test_models, ~{
    map_chr(.x, ~{
      attr(.x, "desc")
    })
  })
}

test_mdl_lst <- list(base_model,
                     last_yr_base_model,
                     #readRDS("/srv/hake/models/2026/01-version/05-test-models/00-areas-as-fleets/00-areas-as-fleets.rds"),
                     #readRDS("/srv/hake/models/2026/01-version/05-test-models/01-areas-as-fleets-canselmax8/01-areas-as-fleets-canselmax8.rds"),
                     readRDS("/srv/hake/models/2026/01-version/05-test-models/06-base-stochastic-forecast-recruitment/06-base-stochastic-forecast-recruitment.rds"),
                     readRDS("/srv/hake/models/2026/01-version/05-test-models/10-base-stochastic-forecast-rec-fit-survey/10-base-stochastic-forecast-rec-fit-survey.rds"),
                     readRDS("/srv/hake/models/2026/01-version/05-test-models/11-base-fit-survey/11-base-fit-survey/.rds"))

# names shown on plot legends
test_mdl_nms <- c("2026 Base model",
                  "2025 Base model",
                  "Stoch. Fore. Recr.",
                  "Stoch. Fore. Recr. & Fit Survey",
                  "Fit survey")
