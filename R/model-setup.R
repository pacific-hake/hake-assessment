#' Load models and set up lists and classes for the base model, bridge model groups, and
#' sensitivity model groups
#'
#' @param main_dirs Output list from [set_dirs()]
#' @param bridge_models_text A list of vectors of text strings to show in the legends for bridge
#' model plots, one name for each model, where the list elements represent a group of models
#' @param sens_models_text A list of vectors of text strings to show in the legends for sensitivity
#' model plots, one name for each model, where the list elements represent a group of models
#' @param retro_models_text A list of vectors of text strings to show in the legends for retrospective
#' model plots, one name for each model, where the list elements represent a group of models
#' @param ... Arguments to pass to [create_rds_file()]
#'
#' @return A list of three items, the base_model inside a single-element list, the list of
#' bridge model groups, and the list of sensitivity model groups. `bridge_grp` and `sens_grp`
#' are groups lists of models which are to be compared with each other in the document.
#' This simplifies plotting and table functions
#' @importFrom purrr map_chr flatten
#' @importFrom furrr future_walk
#' @export
#' @examples
#' \dontrun{
#' library(gfiscamutils)
#' bridge_models_dirs <- list(c("01-base-2015",
#'                              "02-bridge-update-data"),
#'                            c("03-bridge-update-likelihood",
#'                              "04-bridge-update-survey"))
#' bridge_models_text <- list(c("base model from 2015",
#'                              "Add new data"),
#'                            c("Use new lieklihood method",
#'                              "Add new survey index point"))
#' sens_models_dirs <- list(c("10-high-m",
#'                            "11-low-m"),
#'                          c("12-high-sigma-r"))
#' sens_models_text <- list(c("Increase prior for M",
#'                            "Decrease prior for M"),
#'                          c("Increase prior for sigma R"))
#' drs <- set_dirs(base_model_dir = "base",
#'                 bridge_models_dirs = bridge_models_dirs,
#'                 sens_model_dirs = sens_model_dirs)
#' retro_models_dirs <- list(c("01-retrospective-1-year",
#'                             "02-retrospective-2-year",
#'                             "03-retrospective-3-year"))
#' retro_models_text <- list(c("Base model - 1 year",
#'                             "Base model - 2 years",
#'                             "Base model - 3 years"))
#' model_setup <- function(drs,
#'                         bridge_models_text = bridge_models_text,
#'                         sens_models_text = sens_models_text,
#'                         retro_models_text = retro_models_text,
#'                         overwrite_rds_files = TRUE)
#' }
model_setup <- function(main_dirs = NULL,
                        bridge_models_text = NULL,
                        sens_models_text = NULL,
                        retro_models_text = NULL,
                        ...){

  if(is.null(main_dirs[1])){
    stop("main_dirs is NULL. Set main_dirs to the output of set_dirs()", call. = FALSE)
  }

  if(is.null(bridge_models_text[1])){
    warning("`bridge_models_text` is `NULL`. Using bridge model directory names for plot legends")
    bridge_models_text <- basename(main_dirs$bridge_models_dirs)
    bridge_models_text <- bridge_models_text |>
      map(~{factor(.x, levels = .x)})
  }

  if(is.null(sens_models_text[1])){
    warning("`sens_models_text` is `NULL`. Using sens model directory names for plot legends")
    sens_models_text <- basename(main_dirs$sens_models_dirs)
    sens_models_text <- sens_models_text |>
      map(~{factor(.x, levels = .x)})
  }

  if(is.null(retro_models_text[1])){
    warning("`retro_models_text` is `NULL`. Using retro model directory names for plot legends")
    retro_models_text <- basename(main_dirs$retro_models_dirs)
    retro_models_text <- retro_models_text |>
      map(~{factor(.x, levels = .x)})
  }

  # model_list is a list of three lists, one for the base model, one for the bridge models,
  # and one for the sensitivity models
  model_list <- list(base_model_groups = list(main_dirs$base_model_dir),
                     bridge_model_groups = main_dirs$bridge_models_dirs,
                     sens_model_groups = main_dirs$sens_models_dirs,
                     retro_model_groups = main_dirs$retro_models_dirs)

  model_names_list <- list(base_model_groups = ifelse(fr(), "Modèle de base", "Base model"),
                           bridge_model_groups = bridge_models_text,
                           sens_model_groups = sens_models_text,
                           retro_model_groups = retro_models_text)

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
          # Assign description text to the model (from bridge_model_text and sens_model_text)
          attr(k, "model_desc") <- .y
          k
        }) %>%
          `names<-`(.y) %>%
          `class<-`(mdl_lst_cls)
      })
    }
  }, ...)

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
