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
  list(c("00-update-ss3-exe",
         "01-fix-catches",
         "02-fix-weight-at-age",
         #"03-fix-survey-1",
         "04-fix-fishery-comps"),
       c("04-fix-fishery-comps",
         "10-add-catches",
         "20-add-weight-at-age",
         "50-add-fishery-ages",
         "60-fix-late-devs",
         "70-age-1-index"))
bridge_models_desc <-
  list(c("Update the SS3 model",
         "Add changes to catch data",
         "Add changes to weight-at-age data",
         #"Add changes to survey indices",
         "Add changes to fishery age comps"),
       c("Add changes to fishery age comps",
         paste0("Add ", last_data_yr, " catches"),
         paste0("Add ", last_data_yr, " weight-at-age"),
         paste0("Add ", last_data_yr, " fishery ages"),
         "Fix last two years rec devs",
         "Remove Age-1 index"))
prepend_to_bridge <- c(TRUE, FALSE)
# Subtract the following number of years of the end of the models
# when plotting. Should only be 1 for the first one or two, then zeroes.
# This vector must be 1 longer than the above lists, because last year's
# base model is prepended to those lists
bridge_model_end_yr <-
  list(end_yr - c(1, rep(0, length(bridge_models_desc[[1]]))),
       end_yr - rep(0, length(bridge_models_desc[[2]])))

# Uncomment these if you do not want to load bridge models
# bridge_models_dirs <- NA
# bridge_models_desc <- NA
# prepend_to_bridge <- NA

# This is a list of vectors of sensitivity groups (sensitivity models that
# will be plotted against each other). It can be `NA` if you want it to be
# ignored.
# The base model will be prepended to each group by the function. `set_dirs()`
# This means you must reference the models by one more than it appears to be
# e.g. from this list sens_models[[1]][[2]] will be 01-h-prior-mean-low
sens_models_dirs <-
  list(c("01-h-prior-mean-low",
         "02-h-fix-high",
         "03-sigma-r-fix-low",
         "04-sigma-r-fix-high",
         "05-m-02-sd",
         "06-m-03-sd",
         "07-m-hamel-prior"),
       c("08-age-1-survey",
         "09-comp-weight-harmonic-mean",
         "17-edna-index",
         "18-tv-maturity-182"),
       c("10-tv-select-phi-extra-low",
         "11-tv-select-phi-low",
         "12-tv-select-phi-high"),
       c("19-m-at-age",
         "20-m-at-age-fixed"))
sens_models_desc <-
  list(c("Steepness Mean Prior Low (0.5)",
         "Steepness Fix 1.0",
         "Sigma R 1.0",
         "Sigma R 1.6",
         "Natural Mortality (SD=0.2)",
         "Natural Mortality (SD=0.3)",
         "Natural Mortality (Hamel/Cope prior)"),
       c("Add Age-1 Index",
         "Down-weight Fishery Comps",
         "eDNA Index",
         "Earlier maturity"),
       c("Phi t.v. selectivity (0.21)",
         "Phi t.v. selectivity (0.70)",
         "Phi t.v. selectivity (2.10)"),
       c("Natural mortality at age",
         "Fixed natural mortality at age"))

# Uncomment these if you do not want to load sensitivity models
#sens_models_dirs <- NA
#sens_models_desc <- NA

request_models_dirs <-
  list(c("01-max-sel-age-8",
         "02-max-sel-age-10",
         "03-max-sel-age-12"),
       c("04-no-2023-age1",
         "05-no-2021-2023-age1",
         "06-no-2019-2021-2023-age1",
         "07-no-2015-2017-2019-2021-2023-age1"))
request_models_desc <-
  list(c("Max sel. age 8",
         "Max sel. age 10",
         "Max sel. age 12"),
       c("No 2023 age-1 ind.",
         "No 2021, 2023 age-1 ind.",
         "No 2019, 2021, 2023 age-1 ind.",
         "No 2015, 2017, 2019, 2021, 2023 age-1 ind."))

# Uncomment these if you do not want to load request models
#request_models_dirs <- NA
#request_models_desc <- NA

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
