#' Get a data frame which is subset of a data frame used by
#' `ggplot2::ggplot()` to create the heatmap plot of weight-at-age. This
#' will contain useful things like the fill colors and text colors for
#' each tile which can be used to create another heatmap with different
#' values
#'
#' @param col_nms A vector of the name of the columns to return in the data
#' frame along with `age` and `yr`
#' @param layer_ind A number identifying which layer to look for `col_nms` in.
#' If the wrong layer is chosen, the data frame will contain only `age`
#' and `yr` because the column names were not found
#' @param ... Arguments to pass to [plot_weight_at_age_heatmap()] and
#' [heatmap_extract_wa()]
#'
#' @return A data frame containing `age`, `yr`, and the columns listed
#' in `col_nms` if they exist in layer `layer_ind`
#' @export
heatmap_get_wa_ggplot_vals <- function(wa,
                                       col_nms,
                                       layer_ind = 1,
                                       ...){

  # Extract the mapping position data used in the weight-at-age heatmap plot
  g0 <- plot_heatmap_weight_at_age(...)
  g1 <- ggplot_build(g0)
  ggplot_map_pos_data <- g1$data

  ages <- names(wa) %>%
    grep("^\\d+$", ., value = TRUE) |>
    as.numeric()

  map_dat <- ggplot_map_pos_data[[layer_ind]] |>
    as_tibble() |>
    transmute(fill_col = fill,
              alpha_col = alpha,
              age = as.numeric(x),
              yr = y) |>
    # Offset the ages (in case they start at zero)
    mutate(age = ages[age]) |>
    mutate(age = factor(as.numeric(age),
                        levels = levels(g0$data$age)))

  map_dat
}