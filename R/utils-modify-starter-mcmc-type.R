#' Change the MCMC setting in an SS starter file so the extra MCMC files are
#' not created
#'
#' @param path The path where `starter.ss` resides
#' @param value Value to change the MCMC setting to. 1 = Do not create extra
#' MCMC files, 2 or 3 - create extra MCMC files
#'
#' @return Nothing
#' @export
modify_starter_mcmc_type <- function(path, value){

  # Make a modification to the starter file
  if(!dir.exists(path)){
    stop("The directory ", path, " does not exist")
  }
  if(!file.exists(file.path(path, starter_fn))){
    stop("The file ", file.path(path, starter_fn), " does not exist")
  }
  starter_contents <- readLines(file.path(path,
                                          starter_fn))
  mcmc_output_ind <- grep("MCMC output detail|MCMC_output_detail",
                          starter_contents)
  mcmc_output_val <- starter_contents[mcmc_output_ind]
  mcmc_output_val <- gsub("^.*(#.*)",
                          "\\1",
                          mcmc_output_val)
  mcmc_output_val <- paste0(value,
                            " ",
                            mcmc_output_val,
                            " - *Modified by modify_starter_mcmc_type()*")
  starter_contents[mcmc_output_ind] <- mcmc_output_val
  writeLines(starter_contents, file.path(path, starter_fn))
  file_chmod(file.path(path, starter_fn), output_permissions)
}
