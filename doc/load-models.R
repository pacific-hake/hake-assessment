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
  list(c("01-updated-ss-exe",
         "02-add-new-catch",
         "03-add-new-weight-at-age",
         #"04-add-survey-age-2-plus", # Uncomment in survey years
         #"05-add-survey-age-1", # Uncomment in survey years
         "06-add-fishery-ages"))
bridge_models_desc <-
  list(c("Update Stock Synthesis version to 3.30.20",
         paste0("Add ", last_data_yr, " catch"),
         paste0("Add ", last_data_yr, " weight-at-age"),
         #"Add age-2+ acoustic survey", # Uncomment in survey years
         #"Add age-1 index", # Uncomment in survey years
         paste0("Add ", last_data_yr, " fishery age comps")))
prepend_to_bridge <- TRUE
# Subtract the following number of years of the end of the models
# when plotting. Should only be 1 for the first one or two, then zeroes.
# This vector must be 1 longer than the above lists, because last year's
# base model is prepended to those lists
bridge_model_end_yr <- list(end_yr - c(1, 1, 0, 0, 0))

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
         "17-m-hamel-prior-updated"),
       c("08-age-1-survey",
         "09-comp-weight-harmonic-mean"),
       c("10-tv-select-phi-extra-low",
         "11-tv-select-phi-low",
         "12-tv-select-phi-high"),
       c("13-max-sel-age-5",
         "14-max-sel-age-7",
         "15-max-sel-age-8",
         "16-zero-sum-constraint"))
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
         "Phi t.v. selectivity (2.10)"),
       c("Max. age selectivity 5",
         "Max. age selectivity 7",
         "Max. age selectivity 8",
         "Recdevs sum to zero"))

#bridge_models_dirs <- NA
#bridge_models_desc <- NA
#prepend_to_bridge <- NA

#sens_models_dirs <- NA
#sens_models_desc <- NA

request_models_dirs <- NA
request_models_desc <- NA

test_models_dirs <- list(c("02-inflate-survey-n"))
test_models_desc <- list(c("Inflate survey sample size by 100"))

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
last_yr_base_model_name <- paste0(assess_yr-1," Base model")

base_model <- models$base_models_dirs[[1]][[1]]
base_model$ctl <- gsub("\t", " ", base_model$ctl)
base_model_name <- attr(base_model, "desc")

bridge_models <- models$bridge_models_dirs
if(is.na(bridge_models)[1]){
  bridge_models_names <- NA
}else{
  bridge_models_names <- map(bridge_models, ~{map_chr(.x, ~{attr(.x, "desc")})})
}

sens_models <- models$sens_models_dirs
if(is.na(sens_models)[1]){
  sens_models_names <- NA
}else{
  sens_models_names <- map(sens_models, ~{map_chr(.x, ~{attr(.x, "desc")})})
}

request_models <- models$request_models_dirs
if(is.na(request_models)[1]){
  request_models_names <- NA
}else{
  request_models_names <- map(request_models, ~{map_chr(.x, ~{attr(.x, "desc")})})
}

test_models <- models$test_models_dirs
if(is.na(test_models)[1]){
  test_models_names <- NA
}else{
  test_models_names <- map(test_models, ~{map_chr(.x, ~{attr(.x, "desc")})})
}

