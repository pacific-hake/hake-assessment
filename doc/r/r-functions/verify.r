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

verify.catch.levels <- function(lst, inds, forecast.yrs){
  ## Ensure the catch levels list is of the correct format
  ##  also make sure all inds are valid indices of this list
  ##  also make sure the value lists are the same length as the
  ##   forecast years vector.
  curr.func.name <- get.curr.func.name()
  ## Check that it is a list, and that each item is also a list of length 3
  if(is.list(lst))
    j <- lapply(lst, function(x) ifelse(is.list(x) && length(x[[1]]) == length(forecast.yrs), TRUE, FALSE))
  if(!all(sapply(j, all)))
    stop(curr.func.name, "Error - catch.levels is not of the correct structure.\n",
         "Make sure that the length of forecast.yrs is the same as the length of all catch value\n",
         "lists in the catch.levels list.\n",
         "Fix this in forecast-catch-levels.r\n\n")
  ## Check there are no blank names or directory names
  for(i in 1:length(lst)){
    catch.level <- lst[[i]]
    for(j in 1:length(catch.level)){
      vals <- catch.level[[1]]
      name <- catch.level[[2]]
      dir.name <- catch.level[[3]]
      if(name == ""){
        stop(curr.func.name, "Error - catch.levels pretty name for item ", i," is blank. Fix it in forecast-catch-levels.r\n")
      }
      if(dir.name == ""){
        stop(curr.func.name, "Error - catch.levels directory name for item ", i," is blank. Fix it in forecast-catch-levels.r\n")
      }
    }
  }
  ## Check that the inds are valid for this list
  if(!all((inds > 0 & inds <= length(lst))))
     stop(curr.func.name, "Error - the indices you have set up for the catch.levels are outside the length of the list. Fix it in forecast-catch-levels.r\n")
}

