load.models <- function(models.dir = file.path("..","..","models")){
  ## Load all models and their dat files into a list
  ## For all model directories that have an 'mcmc'
  ## sub-directory, load that as well, and attach
  ## as an object of its parents' list item called 'mcmc'.
  ## If the loading of the mcmc directory fails,
  ## its object will be set to NULL but code will continue.
  ##
  ## Returns a list of models, each of which has a dat member
  ## and an mcmc member. The dat member must be populated, but
  ## the mcmc member will be NULL if the mcmc directory had
  ## something wrong with it.
  models.names <- file.path(models.dir, dir(models.dir))
  model.list <- NULL
  for(nm in 1:length(models.names)){
    ## Load the model in the order in which is is listed
    tryCatch({
      model.list[[nm]] <- SS_output(dir=models.names[nm])
    }, warning = function(war){
      cat("There was a warning while loading the model scenario ", models.names[nm],". Warning was: ", war$message,". Continuing...\n")
    }, error = function(err){
      cat("There was an error while loading the model scenario ", models.names[nm],". Error was: ", err$message,". Stopping...\n")
      stop("Check model outputs and re-run.")
    })

    ## Load the data file for the model
    ## Get the file whose name contains "_data.ss"
    ## If there is not exactly one, stop with error.
    model.dir.listing <- dir(models.names[nm])
    fn.ind <- grep("_data.ss", model.dir.listing)
    if(!length(fn.ind)){
      stop("For model ",models.names[nm],", there is no data file. A data file is anything followed and ending in _data.ss.\n")
    }
    if(length(fn.ind) > 1){
      stop("For model ",models.names[nm],", there is more than one data file. A data file is anything followed and ending in _data.ss. Check and make sure there is only one.\n")
    }
    fn <- file.path(models.names[nm], model.dir.listing[fn.ind])
    tryCatch({
      model.list[[nm]]$dat <- SS_readdat(fn)
    }, warning = function(war){
      cat("There was a warning while loading the dat file '",fn,"' for model scenario ", models.names[nm],". Warning was: ", war$message,". Continuing...\n")
    }, error = function(err){
      cat("There was an error while loading the dat file '",fn,"' for model scenario ", models.names[nm],". Error was: ", err$message,". Stopping...\n")
      stop("Check model outputs and re-run.")
    })

    ## If it has an 'mcmc' sub-directory, load that as well
    mcmc.dir <- file.path(models.names[nm], "mcmc")
    if(dir.exists(mcmc.dir)){
      tryCatch({
        model.list[[nm]]$mcmc <- data.frame(SSgetMCMC(dir=mcmc.dir, writecsv=FALSE)$model1)
      }, warning = function(war){
        cat("There was a warning while loading the MCMC model scenario ", mcmc.dir,". Warning was: ", war$message,". Continuing...\n")
      }, error = function(err){
        cat("There was an error while loading the MCMC model scenario ", mcmc.dir,". Error was: ", err$message,". Continuing...\n")
        model.list[[nm]]$mcmc <- NULL
      })
    }else{
      model.list[[nm]]$mcmc <- NULL
    }
  }
  return(model.list)
}
