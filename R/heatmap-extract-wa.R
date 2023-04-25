#' Extract the weight-at-age data frame for the heatmap plot
#' ([plot_weight_at_age_heatmap()])
#'
#' @param model An list of results read in from an SS model using
#' [load_ss_files()]
#' @param fleet An integer value specifying which fleet you want plotted.
#' Fleet -2 will plot fecundity information.
#' Fleet -1 will plot population weight-at-age for the middle of the year.
#' Fleet 0 will plot population weight-at-age for the beginning of the year.
#' Positive values for fleet will link to a modeled fleet.
#' @param apply_pre_yrs A vector of the years to use for the `apply_pre_func`
#' function to fill in missing years before the weight-at-age data starts
#' @param apply_pre_func The function to use on the data filtered by the years
#' given by `apply_pre_yrs`
#' @param apply_post_yrs A vector of the years to use for the `apply_post_func`
#' function to fill in missing years after the weight-at-age data ends
#' (projection years)
#' @param apply_post_func The function to use on the data filtered by the years
#' given by `apply_post_yrs`
#' @param pre_wa_vals A vector of weight-at-age values to use instead of the
#' function `apply_pre_func()` on the data found in the years defined by
#' `apply_pre_yrs`. If this is not `NULL`, this vector will be used for all
#' years prior to the start of the weight-at-age data instead of the output
#' of `apply_pre_func()`. Note this starts at age 0, so it may be one more
#' than you think
#' @param post_wa_vals A vector of weight-at-age values to use instead of the
#' function `apply_post_func()` on the data found in the years defined by
#' `apply_post_yrs`. If this is not `NULL`, this vector will be used for all
#' projection years instead of the output of `apply_post_func()`  Note this
#' starts at age 0, so it may be one more than you think
#' @param ... Absorb arguments meant for other functions
#'
#' @return A data frame containing years (`yr`) column and columns for age,
#' represented as numbers (no text appended to them). The number of age
#' columns is the same as the number in `model$wtatage`
heatmap_extract_wa <- function(
    model = NULL,
    fleet = NULL,
    apply_pre_yrs = min(model$wtatage |>
                          filter(Yr > 0) |>
                          pull(Yr)):model$endyr,
    apply_pre_func = mean,
    apply_post_yrs = min(model$wtatage |>
                           filter(Yr > 0) |>
                           pull(Yr)):model$endyr,
    apply_post_func = mean,
    pre_wa_vals = NULL,
    post_wa_vals = NULL,
    ...){

  stopifnot(!is.null(model))
  stopifnot(!is.null(fleet))
  stopifnot(is.numeric(fleet))
  stopifnot(length(fleet) == 1)
  stopifnot(is.numeric(apply_pre_yrs))
  stopifnot(length(apply_pre_yrs) > 1)
  stopifnot(length(apply_post_yrs) > 1)
  stopifnot(is.numeric(apply_post_yrs))
  stopifnot(is.function(apply_pre_func))
  stopifnot(is.function(apply_post_func))

  # Extract valid waa for given fleet ----
  wa <- model$wtatage |>
    as_tibble() |>
    filter(Fleet == fleet) %>%
    select(Yr, matches("^\\d", .)) |>
    rename(yr = Yr) |>
    filter(yr > 0)
  nms <- names(wa)

  # Extract unique columns ----
  # If there is a plus group in the waa data, and there are ages larger than
  # that, the vector `unq_cols` will have `FALSE` for those column indices
  # and `TRUE` for the ages equal to and younger than the plus group
  unq_cols <- apply(wa, 1, duplicated) |>
    t() |>
    as.data.frame() |>
    as_tibble() |>
    slice(1) |>
    unlist() %>%
    `!`()

  # Remove duplicated columns (typically plus group) ----
  wa <- wa |>
    select(all_of(which(unq_cols))) |>
    set_names(nms[unq_cols])

  # These checks are exactly here because `wa` has been truncated to the
  # length of the plus group and `pre_wa_vals` and `post_wa_vals` must be
  # the same length
  if(!is.null(pre_wa_vals[1])){
    if(length(pre_wa_vals) != ncol(wa) - 1){
      stop("The vector `pre_wa_vals` you supplied is not the right ",
           "length to fit the weight-at-age data frame. It is ",
           length(pre_wa_vals), " long but needs to be ", ncol(wa) - 1,
           " long",
           call. = FALSE)
    }
  }

  if(!is.null(post_wa_vals[1])){
    if(length(post_wa_vals) != ncol(wa) - 1){
      stop("The vector `post_wa_vals` you supplied is not the right ",
           "length to fit the weight-at-age data frame. It is ",
           length(post_wa_vals), " long but needs to be ", ncol(wa) - 1,
           " long",
           call. = FALSE)
    }
  }

  # Add rows for years before waa data starts but still in the model ----
  first_yr <- min(wa$yr)
  start_yr <- model$startyr
  end_yr <- model$endyr
  if(first_yr > start_yr){

    # Need to fill in the missing years at the start of the model with values.
    # Copy first row, modify it with the means of all years and go to the next
    # step to make a little data frame with several of these rows
    if(!is.null(pre_wa_vals[1])){
      pre_dat <- pre_wa_vals
      names(pre_dat) <- names(wa)[-1]
    }else{
      pre_dat <- wa |>
        filter(yr %in% apply_pre_yrs) |>
        select(-yr) |>
        apply(2, apply_pre_func)
    }

    # Make new data frame containing values for the missing years before the
    # start of the weight-at-age data.
    # We go back 2 before the start year `(start_yr - 2)` to create those rows
    # on the plot that can be modified later for the `mean` at the bottom and
    # the blank row above it
    missing_yrs <- (start_yr - 2):(start_yr + (first_yr - start_yr) - 1)
    d <- map_df(missing_yrs, \(yr){
      vec2df(c(yr, pre_dat), names(wa))
    })

    wa <- d |>
      bind_rows(wa)
  }

  # Remove the projected years from the data frame ----
  proj_yrs <- wa$yr[wa$yr > end_yr]
  if(length(proj_yrs)){
    wa <- wa |>
      filter(!yr %in% proj_yrs)
  }

  # Add projection years extrapolated values or provided values ----
  # See above (pre) for explanation of this code, it works the same way
  if(!is.null(post_wa_vals[1])){
    post_dat <- post_wa_vals
    names(post_dat) <- names(wa)[-1]
  }else{
    post_dat <- wa |>
      filter(yr %in% apply_post_yrs) |>
      select(-yr) |>
      apply(2, apply_post_func)
  }

  d <- map_df(proj_yrs, \(yr){
    vec2df(c(yr, post_dat), names(wa))
  })

  wa <- wa |>
    bind_rows(d)

wa
}