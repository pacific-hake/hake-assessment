verify.models <- function(model.dir, model.dir.names, model.names){
  ## Verifies that the directories exist and the names given to them
  ## are of the same length as the length of the directory lists.
  ##
  ## model.dir is the directory in which the model directories reside
  ## model.dir.names is a vector of model directory names to check
  ## model.names is a vector of strings which will be used in plots, etc.
  ##  they are included to make sure they match with the model directory
  ##  names.

  model.dirs <- file.path(model.dir, model.dir.names)
  model.dirs.exist <- file.exists(model.dirs)
  if(!all(model.dirs.exist)){
    cat("verify models: Error - the following model directories do not exist:\n")
    print(model.dirs[!model.dirs.exist])
    stop("\n")
  }
  if(length(model.dir.names) != length(model.names)){
    cat("verify models: Error - the model directory names vector is not the same length as the model names vector:\n")
    cat("Model directory names:\n")
    print(model.dir.names)
    cat("Model names:\n")
    print(model.names)
    stop("\n")
  }
}
