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
#' located. The default value is `<here::here()>/models`
#' @param last_yr_base_model_dir Full path in which the SS3 output
#' for the last assessment year's model resides. The default value is
#' `<here::here()>/models/<YEAR>/01-version/01-base-models/01-base-model`
#' where `<YEAR>` is the current year if currently in the months of May
#' to December, and the year before the current year if currently in the
#' months of January to April, and `<NAME>` is the subdirectory name for
#' the case model in that year
#' @param base_model_dir Full path of the base model directory
#' @param bridge_models_dir Name of the bridging models directory
#' @param sens_models_dir Name of the sensitivity models directory
#' @param request_models_dir Name  of the request models directory
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
    models_dir = here::here("models"),
    year_dir = ifelse(month(Sys.Date()) %in% 5:12,
                      year(Sys.Date()) + 1,
                      year(Sys.Date())),
    version_dir = "01-version",
    last_yr_base_model_dir = file.path(models_dir,
                                       year_dir - 1,
                                       "01-version",
                                       "01-base-models",
                                       "01-base"),
    base_model_dir = file.path("01-base-models",
                               "01-base"),
    bridge_models_dir = "02-bridging-models",
    sens_models_dir = "03-sensitivity-models",
    request_models_dir = "04-request-models",
    test_models_dir = "05-test-models",
    retro_models_dir = "06-retrospective-models",
    bridge_models_dirs = NULL,
    sens_models_dirs = NULL,
    request_models_dirs = NULL,
    test_models_dirs = NULL,
    retro_models_dirs = NULL,
    prepend_to_bridge = NULL){

  if(is.null(models_dir)){
    stop("`models_dir` is `NULL`", call. = FALSE)
  }

  if(is.null(year_dir)){
    stop("`year_dir` is `NULL`", call. = FALSE)
  }

  if(is.null(version_dir)){
    stop("`version_dir` is `NULL`", call. = FALSE)
  }

  models_dir <- file.path(models_dir,
                          year_dir,
                          version_dir)

  # Make model type directories full paths
  base_model_dir = file.path(models_dir, base_model_dir)
  bridge_models_dir = file.path(models_dir, bridge_models_dir)
  sens_models_dir = file.path(models_dir, sens_models_dir)
  request_models_dir = file.path(models_dir, request_models_dir)
  test_models_dir = file.path(models_dir, test_models_dir)
  retro_models_dir = file.path(models_dir, retro_models_dir)

  # Check existence of all model type directories
  lst <- list(models_dir, last_yr_base_model_dir, base_model_dir, bridge_models_dir,
              sens_models_dir, request_models_dir, test_models_dir,
              retro_models_dir)
  lst_names <- list("Models", "Last year's base model", "Base model ",
                    "Bridging models", "Sensitivity monels", "Request models", "Test models",
                    "Retrospective models")
  walk2(lst, lst_names, ~{
    stopifnot(!is.null(.x))
    if(!dir.exists(.x)){
      stop(paste0(.y, " directory does not exist:\n"),
           .x, call. = FALSE)
    }
  })

  dir_types <- list("Bridging", "Sensitivity", "Request", "Test", "Retrospective")
  dirs <- list(bridge_models_dir, sens_models_dir, request_models_dir, test_models_dir, retro_models_dir)
  subdirs <- list(bridge_models_dirs, sens_models_dirs, request_models_dirs, test_models_dirs, retro_models_dirs)

  # Get paths for all directories
  type_iter <- 1
  dirs_full <- map2(dirs, subdirs, function(dir_name, subdir_names){
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

  # Prepend last year's base model to the bridge model groups as defined
  # by `prepend_to_bridge`
  if(!is.null(prepend_to_bridge[1])){
    if(length(prepend_to_bridge) != length(bridge_models_dirs)){
      stop("Length of `prepend_to_bridge` (", length(prepend_to_bridge),
           ") is not equal to length of `bridge_model_dirs` (",
           length(bridge_models_dirs), ")", call. = FALSE)
    }
    dirs_full[[1]] <- map2(dirs_full[[1]], prepend_to_bridge, function(br, prp){
      if(prp){
        c(last_yr_base_model_dir, br)
      }else{
        br
      }
    })
  }

  # Prepend the base model to the sensitivity model groups
  dirs_full[[2]] <- map(dirs_full[[2]], function(sns){
    c(base_model_dir, sns)
  })

  list(models_dir = models_dir,
       last_yr_base_model_dir = last_yr_base_model_dir,
       base_model_dir = base_model_dir,
       bridge_models_dir = bridge_models_dir,
       sens_models_dir = sens_models_dir,
       request_models_dir = request_models_dir,
       test_models_dir = test_models_dir,
       retro_models_dir = retro_models_dir,
       bridge_models_dirs = dirs_full[[1]],
       sens_models_dirs = dirs_full[[2]],
       request_models_dirs = dirs_full[[3]],
       test_models_dirs = dirs_full[[4]],
       retro_models_dirs = dirs_full[[5]])
}
