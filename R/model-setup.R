#' Load models and set up lists and classes for the base model, bridge
#' model groups, and sensitivity model groups
#'
#' @details
#' If any of the text lists are `NULL`, default description text will be
#' assigned and a warning given.
#'
#' @param drs Output list from [set_dirs()]
#' @param bridge_models_desc A list of vectors of text strings to show in
#' the legends for bridge model plots, one name for each model, where the
#' list elements represent a group of models
#' @param sens_models_desc A list of vectors of text strings to show in
#' the legends for sensitivity model plots, one name for each model, where
#' the list elements represent a group of models
#' @param request_models_desc A list of vectors of text strings to show in
#' the legends for request model plots, one name for each model, where the
#' list elements represent a group of models
#' @param test_models_desc A list of vectors of text strings to show in the
#' legends for test model plots, one name for each model, where the list
#' elements represent a group of models
#' @param retro_models_desc A list of vectors of text strings to show in
#' the legends for retrospective model plots, one name for each model,
#' where the list elements represent a group of models
#' @param ... Arguments to pass to [create_rds_file()]
#' @param base_models_desc A list of descriptions for the base models
#' @param prepend_to_bridge A logical vector of length the same as
#' `bridge_models_desc`. If `TRUE`, the base model will be prepended to the
#' group at that index
#'
#' @return A list of items, the base_model inside a single-element
#' list, the list of bridge model groups, sensitivity model groups, request
#' model groups, test model groups, and retrospective model groups. These
#' groups are lists of models which are to be compared with each other in
#' the document. This simplifies plotting and table functions
#' @export
#' @examples
#' \dontrun{
#' library(tidyverse)
#' library(lubridate)
#' bridge_models_dirs <- list(c("01-new-ss-exe",
#'                              "02-new-catch-age",
#'                              "03-update-survey"),
#'                            c("04-age-1-index",
#'                              "05-new-wt-at-age"))
#' bridge_models_descs <- list(c("Update Stock Synthesis version to 3.30.20",
#'                               "Update all fishery catch and comps",
#'                               "Update pre-2021 survey data"),
#'                             c("Update wt-at-age data",
#'                               "Add 2021 survey data"))
#' sens_models_dirs <- list(c("01-h-prior-mean-low",
#'                            "02-h-fix-high",
#'                            "03-sigma-r-fix-low",
#'                            "04-sigma-r-fix-high",
#'                            "05-m-02-sd",
#'                            "06-m-03-sd"),
#'                          c("07-age-1-survey",
#'                            "08-comp-weight-harmonic-mean"),
#'                          c("09-tv-select-phi-extra-low",
#'                            "10-tv-select-phi-low",
#'                            "11-tv-select-phi-high"),
#'                            c("12-max-sel-age-5",
#'                            "13-max-sel-age-7",
#'                            "14-max-sel-age-8"))
#' sens_models_descs <- list(c("Steepness Mean Prior Low (0.5)",
#'                             "Steepness Fix 1.0",
#'                             "Sigma R 1.0",
#'                             "Sigma R 1.6",
#'                             "Natural Mortality (SD=0.2)",
#'                             "Natural Mortality (SD=0.3)"),
#'                           c("Remove Age 1 Index",
#'                             "Downweight Fishery Comps"),
#'                           c("Phi t.v. selectivity (0.21)",
#'                             "Phi t.v. selectivity (0.70)",
#'                             "Phi t.v. selectivity (2.10)"),
#'                           c("Max. age selectivity 5",
#'                             "Max. age selectivity 7",
#'                             "Max. age selectivity 8"))
#'
#' drs <- set_dirs(bridge_models_dirs = bridge_models_dirs,
#'                 sens_models_dirs = sens_models_dirs,
#'                 request_models_dirs = request_models_dirs,
#'                 test_models_dirs = test_models_dirs,
#'                 retro_models_dirs = retro_models_dirs,
#'                 prepend_to_bridge = c(TRUE, FALSE))
#'
#' model_setup <- function(drs,
#'                         bridge_models_desc = bridge_models_desc,
#'                         sens_models_desc = sens_models_desc,
#'                         retro_models_desc = retro_models_desc,
#'                         overwrite_rds_files = TRUE)
#' }
model_setup <- function(drs = NA,
                        base_models_desc = NA,
                        bridge_models_desc = NA,
                        sens_models_desc = NA,
                        request_models_desc = NA,
                        test_models_desc = NA,
                        retro_models_desc = NA,
                        prepend_to_bridge = NA,
                        ...){

  if(is.null(drs[1]) || is.na(drs[1])){
    stop("`drs` is `NULL` or `NA`. Set drs to the output of `set_dirs()`")
  }

  # Set `NULL` descriptions to defaults where possible (directories exist)
  if(!is.list(base_models_desc)){
    base_models_desc <- list(base_models_desc)
  }
  lst <- list(base_models_desc, bridge_models_desc,
              sens_models_desc, request_models_desc,
              test_models_desc)
  names(lst) <- c("base_models_dirs", "bridge_models_dirs",
                  "sens_models_dirs", "request_models_dirs",
                  "test_models_dirs")
  model_lst <- drs[names(lst)]

  # If the descriptions are not present, use the directory names as
  # descriptions
  iter <- 1
  model_desc_lst <- map2(lst, model_lst, function(grp_descs, grp_drs){
    nm <- names(lst)[iter]
    if(is.null(grp_descs[[1]]) || is.na(grp_descs[1])){
      message("`", nm, "` has a `NULL` or `NA` description.\nAttempting ",
              "to use directory names for plot legends.")

      if(is.null(grp_drs[1]) || is.na(grp_drs[1])){
        message("  - Directory names for `", nm, "` are also `NULL` or `NA`")
        message("  - Cannot set up default descriptions for folders that ",
                "do not exist\n")
        NULL
      }else{
        message("  - Successfully set descriptions for `", nm, "` to ",
                "directory names.\n")
        iter <<- iter + 1
        return(map(grp_drs, ~{
          basename(.x)
        }))
      }
    }else{
      iter <<- iter + 1
      if(nm == "bridge_models_dirs"){
        return(map2(grp_descs, prepend_to_bridge, ~{
          if(.y){
            c("Last assessment base model", .x)
          }else{
            .x
          }
          }))
      }else if(nm == "sens_models_dirs" || nm == "test_models_dirs"){
        return(map(grp_descs, ~{
          c(lst$base_models_dirs[[1]], .x)
          }))
      }else{
        return(grp_descs)
      }
    }
    iter <<- iter + 1
    NA
  })

  # A vector of unique model directories
  unique_models_dirs <- model_lst[!is.na(model_lst)] |>
  flatten() |>
    flatten() |>
    as.character() |>
    unique()

  # For each type (base, bridge, sens, request, test) extract unique groups,
  # load them only once if duplicates (to save time/memory) and match with
  # where they belong in the list according to model_list
  map2(model_lst, model_desc_lst, \(type, type_nm, ...){

    if(!is.null(type[1]) && !is.na(type[1])){
      # Check that the RDS files exists for these models. If they don't, stop
      walk(unique_models_dirs, \(path){
        fn <- file.path(path, paste0(basename(path), ".rds"))
        if(!file.exists(fn)){
          stop("RDS file `", fn, "` does not exist. Create it using ",
               "the `create_rds_file()` function")
        }})

      # Load the models in from the RDS files
      unique_models <- map(unique_models_dirs, \(path){
        fn <- file.path(path, paste0(basename(path), ".rds"))
        readRDS(fn)
      })
    }

    # Populate actual model output
    models <- map2(type, type_nm, function(dirs, descs){
      if(is.na(type[1])){
        return(NA)
      }
      map2(dirs, descs, function(dr, desc){
        tmp <- unique_models[[match(dr, unique_models_dirs)]]
        # Add description to the model
        # Example of how to access description for bridge model
        # group 1 model 3:
        # attr(models$bridge_models_dirs[[1]][[3]], "desc")
        attr(tmp, which = "desc") <- desc
        tmp
      })
    })

    models
  }, ...)
}
