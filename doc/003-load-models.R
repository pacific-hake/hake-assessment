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
bridge_models_dirs <-
  list(c("20-add-weight-at-age",
         "30-add-survey-2",
         "31-add-survey-age-comps",
         "40-add-survey-1",
         "50-add-fishery-ages"),
       c("50-add-fishery-ages",
         "60-survey-1-t-distribution",
         "71-redo-fishery-comp",
         "72-tv-fecundity"))
bridge_models_desc <-
  list(c("New SS3 and add catch/weight-at-age",
         "Add age-2+ acoustic survey index",
         "Add survey age comps",
         "Add age-1 index",
         paste0("Add ", last_data_yr, " fishery age comps")),
       c(paste0("Add ", last_data_yr, " fishery age comps"),
         "Use student-t distribution for age-1",
         "Use modeled temporal weight-at-age",
         "Use modeled spatio-temporal maturity"))
prepend_to_bridge <- c(TRUE, FALSE)
# Subtract the following number of years of the end of the models
# when plotting. Should only be 1 for the first one or two, then zeroes.
# This vector must be 1 longer than the above lists, because last year's
# base model is prepended to those lists
bridge_model_end_yr <-
  list(end_yr - c(1, rep(0, length(bridge_models_desc[[1]]))),
       end_yr - rep(0, length(bridge_models_desc[[2]])))

# This is a list of vectors of sensitivity groups (sensitivity models that
# will be plotted against each other). It can be `NA` if you want it to be
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
         "07-m-hamel-prior"),
        #  "17-m-hamel-prior-updated"),
       c("08-age-1-survey",
         "09-comp-weight-harmonic-mean"),
       c("10-tv-select-phi-extra-low",
         "11-tv-select-phi-low",
         "12-tv-select-phi-high"))
sens_models_desc <-
  list(c("Steepness Mean Prior Low (0.5)",
         "Steepness Fix 1.0",
         "Sigma R 1.0",
         "Sigma R 1.6",
         "Natural Mortality (SD=0.2)",
         "Natural Mortality (SD=0.3)",
         "Natural Mortality (Hamel/Cope prior)"),
       c("Remove Age 1 Index",
         "Downweight Fishery Comps"),
       c("Phi t.v. selectivity (0.21)",
         "Phi t.v. selectivity (0.70)",
         "Phi t.v. selectivity (2.10)"))

# bridge_models_dirs <- NA
# bridge_models_desc <- NA
# prepend_to_bridge <- NA

# sens_models_dirs <- NA
# sens_models_desc <- NA

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

bridge_models <- models$bridge_models_dirs
if(is.na(bridge_models)[1]){
  bridge_models_names <- NA
}else{
  bridge_models_names <- map(bridge_models, ~{
    map_chr(.x, ~{attr(.x, "desc")})
  })
}

sens_models <- models$sens_models_dirs
if(is.na(sens_models)[1]){
  sens_models_names <- NA
}else{
  sens_models_names <- map(sens_models, ~{
    map_chr(.x, ~{
      attr(.x, "desc")
    })
  })
}

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
