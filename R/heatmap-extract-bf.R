#' Extract the boldface mask data frame for the heatmap plot
#' ([plot_weight_at_age_heatmap()])
#'
#' @param extrap_mask A data frame where ages are columns (and start with
#' the letter 'a'). If the values are zero, the weight-at-age was
#' extrapolated/interpolated. If there is a value, the weight-at-age is data
#' @param fleet An integer value specifying which fleet you want plotted.
#' Fleet -2 will plot fecundity information.
#' Fleet -1 will plot population weight-at-age for the middle of the year.
#' Fleet 0 will plot population weight-at-age for the beginning of the year.
#' Positive values for fleet will link to a modeled fleet.
#' @param wa A weight-at-age data frame as created by [heatmap_extract_wa()]
#' @param ... Absorb arguments meant for other functions
#'
#' @return A data frame containing years (`yr`) column and columns for age,
#' represented as numbers (no text appended to them). The number of age
#' columns is the same as the number in `model$wtatage`. Each cell is `TRUE`
#' or `FALSE` representing if text in that cell will be boldface or not.
#' The structure of the returned data frame will be identical to the
#' structure of `wa`
heatmap_extract_bf <- function(extrap_mask = NULL,
                               fleet = NULL,
                               wa = NULL,
                               ...){

  stopifnot(!is.null(extrap_mask))
  stopifnot(!is.null(fleet))
  stopifnot(is.numeric(fleet))
  stopifnot(length(fleet) == 1)
  stopifnot(!is.null(wa))
  stopifnot(is.data.frame(wa))

  names(extrap_mask) <- tolower(names(extrap_mask))
  names(extrap_mask) <- gsub("^a(\\d+)$", "\\1", names(extrap_mask))

  bf <- extrap_mask |>
    filter(fleet == !!fleet) %>%
    select(yr, matches("^\\d+", .)) |>
    filter(yr > 0)

  bf_missing_yrs <- wa$yr[!wa$yr %in% bf$yr]

  all_bf_vec <- rep(0, ncol(bf) - 1)
  df <- map_df(bf_missing_yrs, \(yr){
    vec2df(c(yr, all_bf_vec), names(bf))
  })

  bf <- bf |>
    bind_rows(df) |>
    arrange(yr) |>
    mutate(yr = as.integer(yr)) |>
    mutate_at(vars(-yr), ~{!as.logical(.x)})

  if(!identical(dim(bf), dim(wa))){
    stop("The structure (dimensions) of `bf` is not identical to the ",
         "structure of `wa`. Chek `heatmap_extract_bf()`",
         call. = FALSE)
  }

  bf
}