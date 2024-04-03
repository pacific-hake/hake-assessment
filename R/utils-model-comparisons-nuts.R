#' Load all models from a directory with subdirectories
#'
#' @details
#' Creates a list of lists of models, each model in the sublist has the same
#' adapt_delta value as the others in that list. The adapt_delta value is
#' extracted from the filename. The names of the list elements are the
#' adapt delta values as character strings
#'
#' @param num_samples The number of samples to use (extracted from the
#' filenames, so must be one of 500, 1000, 2500, 5000, 10000).
#'
#' @param type_path The path where the subdirectories reside
#'
#' @return A list of lists
#' @export
load_all_models_in_subdir <- \(
  num_samples = NULL,
  type_path = "/srv/hake/models/2024/02-version/05-test-models"){

  num_samples <- match.arg(as.character(num_samples),
                           choices = c(500, 1000, 2500, 5000, 10000))

  leading_num <- ifelse(num_samples == 500,
                        15,
                        ifelse(num_samples == 1000,
                               16,
                               ifelse(num_samples == 2500,
                                      17,
                                      ifelse(num_samples == 5000,
                                             18,
                                             19))))

  mdl_dir <- paste0(leading_num, "-", num_samples, "-samples-group")
  dr <- file.path(type_path,mdl_dir)
  fns_vec <- dir(dr)
  rds_fns_vec <- paste0(fns_vec, ".rds")
  drs <- dir(dr, full.names = TRUE)
  rds_fns <- file.path(drs, rds_fns_vec)

  # Read in RDS files for all models
  lst <- map(rds_fns, ~{
    readRDS(.x)
  })
  names(lst) <- fns_vec
  lst
}

#' Group the input list of models into a list of sub-lists of models which
#' are grouped by a common variable
#'
#' @param lst A list, as returned from [load_all_models_in_subdir()]
#' @param group_by A character string defining the variable to group by.
#' Must be one of "adapt_delta", "num_cores"
#'
#' @return A list of sub-lists, grouped by the unique value of the variable
#' defined by `group_by`
#' @export
group_test_models <- \(lst,
                       group_by = c("adapt_delta",
                                    "num_cores")){

  group_by <- match.arg(group_by)
  if(group_by == "adapt_delta"){
    vars <- gsub("\\d{2}-(\\d{2})-delta-\\d+-cores",
                 "\\1",
                 names(lst)) |>
      as.numeric()
  }else if(group_by == "num_cores"){
    vars <- gsub("\\d{2}-\\d{2}-delta-(\\d+)-cores",
                 "\\1",
                 names(lst)) |>
      as.numeric()
  }else{
    stop("`", group_by, "` not implemented for grouping")
  }

  nms <- split(names(lst), vars)
  lst <- split(lst, vars)

  lst
}

#' Create an n-panel plot of the biomass, relative biomass, or recruitment
#' for a set of models comparing a value
#'
#' @param lst A list, as returned from [group_test_models()]
#' @param plot_func The plotting function to use. Typically [plot_biomass()],
#' [plot_rel_biomass()], or [plot_recruitment()]
#' @param label_text The text to paste on the label that is shown on
#'  each panel
#' @param panel_axis_titles_size Font size for the axis labels
#' @param panel_axis_text_size Font size for the axis tick labels
#' @param scale Number to divide the label by (used to divide adapt
#'  delta values by 100 so they are less than 1)
#' @param digits The number of decimal places to make the label numbers
#' @param ... Arguments to pass to the plotting function defined by `plot_func`
#'
#' @return A [ggplot2::ggplot()] object
#' @export
plot_comparison <- \(
  lst = NULL,
  plot_func = NULL,
  label_text = "Variable x",
  panel_axis_titles_size = 11,
  panel_axis_text_size = 8,
  scale = 1,
  digits = 0,
  ...){

  p <- list()
  for(i in seq_along(lst)){
    p[[i]] <- plot_func(lst[[i]], names(lst[[i]]), ...) +
      ggtitle(paste0(label_text, " = ",
                     f(as.numeric(names(lst)[i]) / scale, digits))) +
      theme(axis.title.y = element_text(size = panel_axis_titles_size),
            axis.title.x = element_text(size = panel_axis_titles_size),
            axis.text.y = element_text(size = panel_axis_text_size),
            axis.text.x = element_text(size = panel_axis_text_size),
            # plot.margin: top, right,bottom, left
            plot.margin = margin(12, 12, 12, 12))
  }

  plot_grid(plotlist = p, ncol = 2, align = "hv")
}

