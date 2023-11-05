#' Append retrospective runs to the already-built RDS file and re-save it
#'
#' @param rds_fn RDS filename
#' @param retrospective_yrs a vector of years to include in the loading
#'
#' @return Nothing
append_retros <- function(rds_fn, retrospective_yrs = 1:10){

  stopifnot(file.exists(rds_fn))
  model <- readRDS(rds_fn)
  model$retros <- load_retrospectives(model$retrospectives_path,
                                      retrospective_yrs)
  saveRDS(model, rds_fn)
}
