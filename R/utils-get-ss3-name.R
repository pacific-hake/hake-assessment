#' Get the name of the SS3 executable to use.
#'
#' @details
#' Simple logic to either use:
#' 1. the one provided in the `ss_exe` argument, if it is not `NULL`
#' 2. the one defined in [ss_executable] which is a package data variable
#' 3. throw an error if both the above options are `NULL`
#' Note that this function does not check to see if the executable exists
#' or is on the 'PATH' or anything else, it is just a logic gate returning a
#' string
#'
#' @param ss_exe The name of executable to use or `NULL` to use the package
#' data variable [ss_executable]
#'
#' @return The name of the SS3 executable to use
#' error
get_ss3_exe_name <- function(ss_exe = NULL){

  if(is.null(ss_exe)){
    if(is.null(ss_executable)){
      stop("`run_ct_levels_default_hr`: No SS3 executable could be found.")
    }else{
      # If no user-defined and no YAML-defined (this function is run on the
      # command line) then use the name found in the package data variable
      # ss_executable
      ss_exe <- ss_executable
    }
  }

  ss_exe
}