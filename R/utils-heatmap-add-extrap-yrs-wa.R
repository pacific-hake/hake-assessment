#' Add extrapolated years before and after the weight-at-age time
#' series data in `model$wtatage`
#'
#' @param model An list of results read in from an SS model using
#' [load_ss_files()]
#' @param wa The weight-at-age data frame found in `model$wtatage`
#' @param pre_yrs A vector of the years to use for the `pre_func`
#' function to fill in missing years before the weight-at-age data starts
#' @param pre_func The function to use on the data filtered by the years
#' given by `pre_yrs`
#' @param post_yrs A vector of the years to use for the `post_func`
#' function to fill in missing years after the weight-at-age data ends
#' (projection years)
#' @param post_func The function to use on the data filtered by the years
#' given by `post_yrs`
#' @param pre_wa_vals A vector of weight-at-age values to use instead of the
#' function `pre_func()` on the data found in the years defined by
#' `pre_yrs`. If this is not `NULL`, this vector will be used for all
#' years prior to the start of the weight-at-age data instead of the output
#' of `pre_func()`. Note this starts at age 0, so it may be one more
#' than you think
#' @param post_wa_vals A vector of weight-at-age values to use instead of the
#' function `post_func()` on the data found in the years defined by
#' `post_yrs`. If this is not `NULL`, this vector will be used for all
#' projection years instead of the output of `post_func()`  Note this
#' starts at age 0, so it may be one more than you think
#' @param ... Absorbs arguments meant for other functions
#'
#' @return A data frame containing years (`yr`) column and columns for age,
#' represented as numbers (no text appended to them). The number of age
#' columns is the same as the number in `model$wtatage`
heatmap_add_extrap_yrs_wa <- function(
    model = NULL,
    wa = NULL,
    pre_yrs = min(model$wtatage |>
                    dplyr::filter(Yr > 0) |>
                    pull(Yr)):model$endyr,
    pre_func = mean,
    post_yrs = min(model$wtatage |>
                     dplyr::filter(Yr > 0) |>
                     pull(Yr)):model$endyr,
    post_func = mean,
    pre_wa_vals = NULL,
    post_wa_vals = NULL,
    ...){

  stopifnot(!is.null(model))
  stopifnot(!is.null(wa))
  stopifnot(is.numeric(pre_yrs))
  stopifnot(length(pre_yrs) > 1)
  stopifnot(length(post_yrs) > 1)
  stopifnot(is.numeric(post_yrs))
  stopifnot(is.function(pre_func))
  stopifnot(is.function(post_func))

  # Extract unique columns ----
  # If there is a plus group in the waa data, and there are ages larger than
  # that, the vector `unq_cols` will have `FALSE` for those column indices
  # and `TRUE` for the ages equal to and younger than the plus group.
  unq_cols <- apply(wa, 1, duplicated) |>
    t() |>
    as.data.frame() |>
    as_tibble() |>
    slice(1) |>
    unlist() %>%
    `!`()
  # Special case: For fecundity, age 0 and age 1 have columns of all zeros
  # so we need to catch that and not remove the age 1 column in that case
  wch_age1 <- which(names(wa) == "1")
  unq_cols[wch_age1] <- TRUE

  # Remove duplicated columns (typically plus group) ----
  nms <- names(wa)
  wa <- wa |>
    select(all_of(which(unq_cols))) |>
    set_names(nms[unq_cols])

  # These checks are here because `wa` has been truncated to the
  # length of the plus group and `pre_wa_vals` and `post_wa_vals` must be
  # the same length
  if(!is.null(pre_wa_vals[1])){
    if(length(pre_wa_vals) != ncol(wa) - 1){
      stop("The vector `pre_wa_vals` you supplied is not the right ",
           "length to fit the weight-at-age data frame. It is ",
           length(pre_wa_vals), " long but needs to be ", ncol(wa) - 1,
           " long")
    }
  }

  if(!is.null(post_wa_vals[1])){
    if(length(post_wa_vals) != ncol(wa) - 1){
      stop("The vector `post_wa_vals` you supplied is not the right ",
           "length to fit the weight-at-age data frame. It is ",
           length(post_wa_vals), " long but needs to be ", ncol(wa) - 1,
           " long")
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
      pre_dat <- heatmap_calc_function(
        wa = wa |>
          dplyr::filter(yr %in% pre_yrs),
        func = pre_func,
        ...)
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
      dplyr::filter(!yr %in% proj_yrs)
  }

  # Add projection years extrapolated values or provided values ----
  # See above (pre) for explanation of this code, it works the same way
  if(!is.null(post_wa_vals[1])){
    post_dat <- post_wa_vals
    names(post_dat) <- names(wa)[-1]
  }else{
    post_dat <- heatmap_calc_function(
      wa = wa |>
        dplyr::filter(yr %in% post_yrs),
      func = post_func,
      ...)
  }

  d <- map_df(proj_yrs, \(yr){
    vec2df(c(yr, post_dat), names(wa))
  })

  wa <- wa |>
    bind_rows(d)

wa
}
