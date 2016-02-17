################################################################################
## Base model, this year and last
################################################################################

## Set up models lists - NOTE all are *required* to build the document.
models.dir.list <- dir(models.path)

base.model.name <- "39_preSRGbase_updated"
## Last year's base model. This is used for the parameter estimates table which compares
##  last year's to this year's parameter estimates.
last.year.base.model.name <- "00_2015hake_basePreSRG"

## Indicies models as found in the directory.
base.model.ind <- grep(base.model.name, models.dir.list)
if(length(base.model.ind) == 0){
  stop("Base model '", base.model.name, "' not found. Check the name and try again.\n")
}
if(verbose){
  cat("\nDEBUG: Loading model ", base.model.name, " as base model\n\n")
}
last.year.base.model.ind <- grep(last.year.base.model.name, models.dir.list)
if(length(last.year.base.model.ind) == 0){
  stop("Last year's base model '", last.year.base.model.name, "' not found. Check the name and try again.\n")
}

################################################################################
## Bridge models
################################################################################
cat("Loading bridge models... \n\n")

bridge.model.dir.names.1 <- c(last.year.base.model.name,
                              "03_UpdatePre2015WtAge")
## Bridge model names will be used to make the bridge model plot and its caption.
bridge.model.names.1 <- c(paste0(last.assess.yr, " Base model"),
                          "Update data")
bridge.model.dir.names.2 <- c(last.year.base.model.name,
                              "40_Add2015Survey_withExtrap_update",
                              "41_Add2015Catch_FishAcomps_withExtrap_update")
## Bridge model names will be used to make the bridge model plot and its caption.
bridge.model.names.2 <- c(paste0(last.assess.yr, " Base model"),
                          paste0("Add ", survey.end.yr, " survey series"),
                          paste0("Add ", last.data.yr, " fishery data"))
bridge.model.dir.names.3 <- c("41_Add2015Catch_FishAcomps_withExtrap_update",
                              "41.01_adjustBiasRamping_update",
                              "41.02_ChangeSurveyTuning_update",
                              "41.03_ChangeAllTuning_update")
## Bridge model names will be used to make the bridge model plot and its caption.
bridge.model.names.3 <- c("Base model pretune",
                          "Adjust bias ramp",
                          "Change survey comp weights",
                          "Change all comp weights")

## Bridge model indices are used to tell knitr which elements of the models list are to
## be plotted together.
bridge.model.inds.1 <- grep(paste(bridge.model.dir.names.1, collapse = "|"), models.dir.list)
bridge.model.inds.2 <- grep(paste(bridge.model.dir.names.2, collapse = "|"), models.dir.list)
bridge.model.inds.3 <- grep(paste(bridge.model.dir.names.3, collapse = "|"), models.dir.list)
if((length(bridge.model.inds.1) != length(bridge.model.dir.names.1)) |
   (length(bridge.model.inds.2) != length(bridge.model.dir.names.2)) |
   (length(bridge.model.inds.3) != length(bridge.model.dir.names.3))){
  stop("One or more of the bridge model directory names were not found. Check the names and try again. Directory names listed in all.r are:\n",
       paste0(bridge.model.dir.names.1, "\n"),
       "\n",
       paste0(bridge.model.dir.names.2, "\n"),
       "\n",
       paste0(bridge.model.dir.names.3, "\n"))
}
if((length(bridge.model.names.1) != length(bridge.model.dir.names.1)) |
   (length(bridge.model.names.2) != length(bridge.model.dir.names.2)) |
   (length(bridge.model.names.3) != length(bridge.model.dir.names.3))){
  stop("One of the bridge.model.names vectors in all.r has a different length than its bridge.model.dir.names counterpart. Make sure these two vectors match in length and try again.\n")
}

################################################################################
## Sensitivity models
################################################################################
if(verbose){
  cat("\nDEBUG: Loading sensitivity models \n\n")
}
sens.model.dir.names.1 <- c("46_Sensbase_sigmaR_1.0_update",
                            "47_Sensbase_sigmaR_2.0_update",
                            "48_Sensbase_h_0.5prior_update",
                            "49_Sensbase_h_1.0fix_update",
                            "51_Sensbase_M_SD0.2_update",
                            "52_Sensbase_M_SD0.3_update")
## Sens model names will be used to make the sensitivity model plot and its caption.
## Make sure they are the same length as sens.model.dir.names
sens.model.names.1 <- c("Sigma R 1.0",
                        "Sigma R 2.0",
                        "Steepness prior mean 0.5",
                        "Steepness fixed mean 1.0",
                        "Natural mortality SD 0.2",
                        "Natural mortality SD 0.3")

sens.model.dir.names.2 <- c("42_Sensbase_Survey_noExtrap_update",
                            "50_Sensbase_Age1Index_update")
sens.model.names.2 <- c("No extrapolation on survey",
                        "Include age-1 index")
sens.model.dir.names.3 <- c("43_Sensbase_Selmaxage5_update",
                            "44_Sensbase_Selmaxage7_update",
                            "45_Sensbase_Selmaxage12_update")
sens.model.names.3 <- c("Max. age of selectivity 5",
                        "Max. age of selectivity 7",
                        "Max. age of selectivity 12")

## Sensitivity model indices are used to tell knitr which elements of the models list are to
## be plotted together.
sens.model.inds.1 <- grep(paste(sens.model.dir.names.1, collapse = "|"), models.dir.list)
sens.model.inds.2 <- grep(paste(sens.model.dir.names.2, collapse = "|"), models.dir.list)
sens.model.inds.3 <- grep(paste(sens.model.dir.names.3, collapse = "|"), models.dir.list)
if((length(sens.model.inds.1) != length(sens.model.dir.names.1)) |
   (length(sens.model.inds.2) != length(sens.model.dir.names.2)) |
   (length(sens.model.inds.3) != length(sens.model.dir.names.3))){
  stop("One or more of the sensitivity model directory names were not found. Check the names and try again. Directory names listed in all.r are:\n",
       paste0(sens.model.dir.names.1, "\n"),
       "\n",
       paste0(sens.model.dir.names.2, "\n"),
       "\n",
       paste0(sens.model.dir.names.3, "\n"))
}
if((length(sens.model.names.1) != length(sens.model.dir.names.1)) |
   (length(sens.model.names.2) != length(sens.model.dir.names.2)) |
   (length(sens.model.names.3) != length(sens.model.dir.names.3))){
  stop("model-setup.r: One of the sens.model.names vectors a different length than its sens.model.dir.names counterpart. Make sure these two vectors match in length and try again.\n")
}

################################################################################
## Set up lists to use for sensitivity plots and tables
################################################################################
cat("Setting up lists for sensitivities... \n\n")

## A vector of all sensitivities for the MLE parameters, derived quantiles, and reference points table
sens.model.inds.1.for.table <- sens.model.inds.1
sens.model.names.1.for.table <- c("Base model", sens.model.names.1)
sens.models.1.for.table <- list(models[[base.model.ind]])
i <- 1
for(sens.model in sens.model.inds.1.for.table){
  sens.models.1.for.table[[i + 1]] <- models[[sens.model]]
  i <- i + 1
}
sens.model.inds.2.for.table <- c(sens.model.inds.2, sens.model.inds.3)
sens.model.names.2.for.table <- c("Base model", sens.model.names.2, sens.model.names.3)
sens.models.2.for.table <- list(models[[base.model.ind]])
i <- 1
for(sens.model in sens.model.inds.2.for.table){
  sens.models.2.for.table[[i + 1]] <- models[[sens.model]]
  i <- i + 1
}
## sens.models.1.for.table now contains the base case and sensitivity group 1 models
## sens.model.names.1.for.table now contains "Base model" sensitivity group 1 models
## sens.models.2.for.table now contains the base case and sensitivity groups 2 and 3 models
## sens.model.names.2.for.table now contains "Base model" sensitivity groups 2 and 3 models
