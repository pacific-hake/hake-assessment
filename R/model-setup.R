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
#'
#' @return A list of items, the base_model inside a single-element
#' list, the list of bridge model groups, sensitivity model groups, request
#' model groups, test model groups, and retrospective model groups. These
#' groups are lists of models which are to be compared with each other in
#' the document. This simplifies plotting and table functions
#' @importFrom purrr map_chr flatten
#' @importFrom furrr future_walk
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
model_setup <- function(drs = NULL,
                        bridge_models_desc = NULL,
                        sens_models_desc = NULL,
                        request_models_desc = NULL,
                        test_models_desc = NULL,
                        retro_models_desc = NULL,
                        ...){

  if(is.null(drs[1])){
    stop("`drs` is NULL. Set drs to the output of `set_dirs()`",
         call. = FALSE)
  }

  # Set `NULL` descriptions to defaults where possible (directories exist)
  lst <- list(bridge_models_desc, sens_models_desc, request_models_desc,
              test_models_desc, retro_models_desc)
  names(lst) <- c("bridge_models_desc", "sens_models_desc",
                  "request_models_desc", "test_models_desc",
                  "retro_models_desc")

  descs <- imap(lst, ~{
    if(is.null(.x[1])){
      message("`", .y, "` is `NULL`. Attempting to use directory names ",
              "for plot legends")
      dir_type_name <- gsub("_desc", "_dirs", .y)

      if(is.null(drs[[dir_type_name]])){
        message("  - Directory names for `", .y, "` are also `NULL`")
        message("  - Cannot set up default descriptions for folders that ",
                "do not exist\n")
        NULL
      }else{
        message("  - Successfully set descriptions for `", .y, "` to ",
                "directory names.\n")
        map(drs[[dir_type_name]], ~{
          basename(.x)
        })
      }
    }else{
      .x
    }
  })

  # model_list is a list of three lists, one for the base model, one for the bridge models,
  # and one for the sensitivity models
  model_list <- list(base_model_groups = list(drs$base_model_dir),
                     bridge_model_groups = drs$bridge_models_dirs,
                     sens_model_groups = drs$sens_models_dirs,
                     request_model_groups = drs$request_models_dirs,
                     test_model_groups = drs$test_models_dirs,
                     retro_model_groups = drs$retro_models_dirs)

  model_names_list <- list(base_model_groups = "Base model",
                           bridge_model_groups = bridge_models_desc,
                           sens_model_groups = sens_models_desc,
                           request_model_groups = request_models_desc,
                           test_model_groups = test_models_desc,
                           retro_model_groups = retro_models_desc)

  j <- imap(model_list, function(.x, .y, ...){
    models <- NULL
    if(!is.null(.x)){
      unique_models_dirs <- .x %>%
        flatten() %>%
        unique() %>%
        map_chr(~{.x})

      walk(unique_models_dirs, function(x, ...){
                    create_rds_file(x, ...)},
                  ...)


      # This ensures that each unique model is loaded only once, even if it is in multiple
      # sensitivity groups
      unique_models <- map(unique_models_dirs, ~{load_rds_file(.x)}) %>%
        `names<-`(unique_models_dirs)

      models <- map2(.x, model_names_list[[.y]], ~{
        map2(.x, .y, ~{
          k <- unique_models[[match(.x, unique_models_dirs)]] %>%
            `class<-`(mdl_cls)
          # Assign description text to the model (from bridge_model_desc and sens_model_desc)
          attr(k, "model_desc") <- .y
          k
        }) %>%
          `names<-`(.y) %>%
          `class<-`(mdl_lst_cls)
      })
    }
  }, ...)
  browser()

  base_model <- j[[1]][[1]]
  bridge_grps <- j[[2]]
  if(!is.null(bridge_grps)){
    class(bridge_grps) <- mdl_grp_cls
  }
  sens_grps <- j[[3]]
  if(!is.null(sens_grps)){
    class(sens_grps) <- mdl_grp_cls
  }
  retro_grps <- j[[4]]
  if(!is.null(retro_grps)){
    class(retro_grps) <- mdl_grp_cls
  }

  list(base_model = base_model,
       bridge_grps = bridge_grps,
       sens_grps = sens_grps,
       retro_grps = retro_grps)
}
