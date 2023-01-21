#' Make RDS files smaller by removing unnecessary objects
#'
#' @param drs Output from [set_dirs()]
make_small_rds <- function(drs){

  br <- drs$bridge_models_dirs |> unlist()
  sn <- drs$sens_models_dirs |> unlist()

  dirs <- unique(c(br, sn))
  # Remove base model from this list, we always need the full version for that
  base_ind <- grep(paste0("01-base$"), dirs)
  if(length(base_ind) > 2){
    stop("More than two base model dirs found. There should be one for `", assess_yr - 1,
         "` and one for `", assess_yr, "`",
         call. = FALSE)
  }
  if(length(base_ind)){
    dirs <- dirs[-base_ind]
  }

  # change this year to the current year once you've updated the argument
  # list for the new year
  func_year <- 2023
  if(year(Sys.time()) != func_year){
    stop("The make_small_rds() function has default arguments that need to ",
         "be updated each year")
  }

  message("Making smaller RDS files for bridge models and sensitivity models.")

  for(i in 1:length(models)){
    fn <- file.path("models", models[i], paste0(models[i], ".rds"))
    x <- readRDS(fn)
    browser()
    ex <- x$extra.mcmc
    if(!is.na(ex[1])){
      x$extra.mcmc <- NULL

      x$extra.mcmc$Q_vector <- ex$Q_vector
      x$extra.mcmc$cpue.median <- ex$cpue.median
      x$extra.mcmc$cpue.0.025 <- ex$cpue.0.025
      x$extra.mcmc$cpue.0.975 <- ex$cpue.0.975
      x$extra.mcmc$q.med <- ex$q.med
      x$extra.mcmc$index.med <- ex$index.025
      x$extra.mcmc$index.025 <- ex$index.025
      x$extra.mcmc$index.975 <- ex$index.975

      fn_split <- strsplit(fn, "/")[[1]]
      fn_split[3] <- paste0("small_", fn_split[3])
      fn <- paste(fn_split, collapse = "/")
      saveRDS(x, fn)
      message(fn)
    }
  }
}
