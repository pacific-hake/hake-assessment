#' Set and return all model directories for the project
#'
#' @details
#' Sensitivity groups will have the base model directory prepended,
#' The bridge model groups can have `last_yr_base_model_dir`
#' prepended if `prepend_to_bridge` is set to `TRUE` for the model group.
#' If directories do not exist, they will be assigned `NULL` and a warning
#' issued. If the `models_dir` does not exist, an error will be thrown.
#'
#' @param models_dir Full path in which the SS3 model directories are
#' located for the current assessment year
#' @param last_yr_models_dir Full path in which the SS3 model directories are
#' located for the the last assessment year
#' @param base_model_dir Name of the base model directory
#' @param bridge_models_dir Name of the bridging models directory
#' @param sens_models_dir Name of the sensitivity models directory
#' @param request_models_dir Name of the request models directory
#' @param test_models_dir Name of the test models directory
#' @param retro_models_dir Name of the retrospective models directory
#' @param bridge_models_dirs A vector of subdirectory names in
#' `bridge_models_dir` that each contain an individual SS3 bridge model
#' @param sens_models_dirs A vector of subdirectory names in `sens_models_dir`
#' that each contain an individual SS3 sensitivity model
#' @param request_models_dirs A vector of subdirectory names in
#' `request_models_dir` that each contain an individual SS3 base model request
#' model
#' @param test_models_dirs A vector of subdirectory names in `test_models_dir`
#'  that each contain an individual SS3 base model test model
#' @param retro_models_dirs A vector of subdirectory names in
#' `retro_models_dir` that each contain an individual SS3 base model
#' retrospective model
#' @param prepend_to_bridge A vector of logical values, the same length as the
#' number of groups of bridge models (`length(bridge_models_dirs)`) which,
#' if `TRUE` will prepend the `last_yr_base_model_dir``to the beginning of
#' the group of that element number. If `NULL`, no prepending will take place
#'
#' @return A list of twelve full paths, which will have `NULL` elements for
#' the directories which don't exist:
#' 1.  Full path of `models_dir`
#' 2.  Full path of `last_yr_base_model_dir`
#' 3.  Full path of `base_model_dir`
#' 4.  Full path of `bridge_models_dir`
#' 5.  Full path of `sens_models_dir`
#' 6.  Full path of `request_models_dir`
#' 7.  Full path of `test_models_dir`
#' 8.  Full path of `retro_models_dir`
#' 9.  A vector of the bridge model directories
#' 10. A vector of the sensitivity model directories
#' 11. A vector of the request model directories
#' 12. A vector of the test model directories
#' 13. A vector of the retrospective model directories
#' @importFrom purrr map map_lgl
#' @importFrom lubridate year month
#' @export
set_dirs <- function(
    models_dir = NULL,
    last_yr_models_dir = NULL,
    base_models_dir = "01-base-models",
    bridge_models_dir = "02-bridging-models",
    sens_models_dir = "03-sensitivity-models",
    request_models_dir = "04-request-models",
    test_models_dir = "05-test-models",
    retro_models_dir = "06-retrospective-models",
    base_models_dirs = NULL,
    bridge_models_dirs = NULL,
    sens_models_dirs = NULL,
    request_models_dirs = NULL,
    test_models_dirs = NULL,
    retro_models_dirs = NULL,
    prepend_to_bridge = NULL){

  if(is.null(models_dir)){
    stop("`models_dir` is `NULL`",
         call. = FALSE)
  }

  if(!dir.exists(models_dir)){
    stop("`models_dir` does not exist",
         call. = FALSE)
  }

  if(is.null(last_yr_models_dir)){
    stop("`last_yr_models_dir` is `NULL`",
         call. = FALSE)
  }

  if(!dir.exists(last_yr_models_dir)){
    stop("`last_yr_models_dir` does not exist",
         call. = FALSE)
  }

  all_models_dir <- list(base_models_dir,
                         bridge_models_dir,
                         sens_models_dir,
                         request_models_dir,
                         test_models_dir,
                         retro_models_dir)
  all_models_dirs <- list(base_models_dirs,
                          bridge_models_dirs,
                          sens_models_dirs,
                          request_models_dirs,
                          test_models_dirs,
                          retro_models_dirs)
  has_models_dir <- map_lgl(all_models_dir, ~{!is.null(.x)})
  has_models_subdirs <- map_lgl(all_models_dirs, ~{!is.null(.x)})

  # Make model type directories full paths
  base_models_dir <- ifelse(has_models_dir[1] && has_models_subdirs[1],
                            file.path(models_dir, base_models_dir),
                            NA_character_)
  bridge_models_dir <- ifelse(has_models_dir[2] && has_models_subdirs[2],
                              file.path(models_dir, bridge_models_dir),
                              NA_character_)
  sens_models_dir <- ifelse(has_models_dir[3] && has_models_subdirs[3],
                            file.path(models_dir, sens_models_dir),
                            NA_character_)
  request_models_dir <- ifelse(has_models_dir[4] && has_models_subdirs[4],
                               file.path(models_dir, request_models_dir),
                               NA_character_)
  test_models_dir <- ifelse(has_models_dir[5] && has_models_subdirs[5],
                            file.path(models_dir, test_models_dir),
                            NA_character_)
  retro_models_dir <- ifelse(has_models_dir[6] && has_models_subdirs[6],
                             file.path(models_dir, retro_models_dir),
                             NA_character_)

  # Check existence of all model type directories
  # These three lists must be the same length for the `walk2()` and `map2()`
  # calls that follow
dirs <- list(base_models_dir,
             bridge_models_dir,
             sens_models_dir,
             request_models_dir,
             test_models_dir,
             retro_models_dir)
dir_types <- list("Base model",
                  "Bridging models",
                  "Sensitivity monels",
                  "Request models",
                  "Test models",
                  "Retrospective models")
subdirs <- list(base_models_dirs,
                bridge_models_dirs,
                sens_models_dirs,
                request_models_dirs,
                test_models_dirs,
                retro_models_dirs)

  # Check that encapsulating directories exist
  walk2(dirs, dir_types, ~{
    if(!is.na(.x) && !dir.exists(.x)){
      stop(paste0(.y, " directory does not exist:\n"),
           .x,
           call. = FALSE)
    }
  })

  # check that all subdirectories exist, and get paths for them
  type_iter <- 1
  dirs_full <- map2(dirs, subdirs, function(dir_name, subdir_names){
    if(is.na(dir_name)){
      return(NULL)
    }
    dir_full <- NULL
    if(!is.null(subdir_names)){
      dir_full <- map(subdir_names, function(grp){
        grp_full <- file.path(dir_name, grp)
        dir_existence <- map_lgl(grp_full, ~{dir.exists(.x)})
        if(!all(dir_existence)){
          stop(paste0("Some ", dir_types[type_iter], " model directories do not exist:\n\n"),
               paste0(grp_full[!dir_existence], collapse = "\n"),
               call. = FALSE)
        }
        grp_full
      })
    }
    type_iter <- type_iter + 1
    dir_full
  })

  last_yr_base_model_dir <- file.path(last_yr_models_dir,
                                      basename(base_models_dir),
                                      base_models_dirs)

  # Prepend last year's base model to the bridge model groups as defined
  # by `prepend_to_bridge`
  if(!is.null(prepend_to_bridge[1])){
    if(length(prepend_to_bridge) != length(bridge_models_dirs)){
      stop("Length of `prepend_to_bridge` (", length(prepend_to_bridge),
           ") is not equal to length of `bridge_model_dirs` (",
           length(bridge_models_dirs), ")", call. = FALSE)
    }
    if(has_models_dir[2] && has_models_subdirs[2]){
      dirs_full[[2]] <- map2(dirs_full[[2]], prepend_to_bridge, function(br, prp){
        if(prp){
          c(last_yr_base_model_dir, br)
        }else{
          br
        }
      })
    }
  }

  # Prepend the base model to the sensitivity model groups
  if(has_models_dir[3] && has_models_subdirs[3]){
    dirs_full[[3]] <- map(dirs_full[[3]], function(sns){
      c(base_model_dir, sns)
    })
  }

  list(models_dir = models_dir,
       last_yr_base_model_dir = last_yr_base_model_dir,
       base_model_dir = base_models_dir,
       bridge_models_dir = bridge_models_dir,
       sens_models_dir = sens_models_dir,
       request_models_dir = request_models_dir,
       test_models_dir = test_models_dir,
       retro_models_dir = retro_models_dir,
       base_models_dirs = dirs_full[[1]],
       bridge_models_dirs = dirs_full[[2]],
       sens_models_dirs = dirs_full[[3]],
       request_models_dirs = dirs_full[[4]],
       test_models_dirs = dirs_full[[5]],
       retro_models_dirs = dirs_full[[6]])
}
