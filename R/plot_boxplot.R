#' Get depth data by year and calculate boxplot stats on it.
#'
#' @param d Output from [load_spatial_catch_data().
#' @param type One of "bottom" or "gear" for depth type.
#' @param yrs A vector of years to include. If `NULL`, all years in the data
#'   will be included.
#' @param min_depth_cutoff The depth for which to remove data. In fasthoms.
#'   All data shallower than this will be removed.
#'
#' @return A tibble containing year and depth record stats.
#' @export
get_depth_by_year <- function(d,
                              type = c("bottom", "gear"),
                              yrs = NULL,
                              min_depth_cutoff = 50 / 1.8288) {
  type <- match.arg(type)
  if (type == "bottom") {
    dpth <- d %>%
      dplyr::filter(!is.na(BOTTOM_DEPTH_FATHOMS)) %>%
      dplyr::select(year, depth = BOTTOM_DEPTH_FATHOMS)
  }
  if (type == "gear") {
    dpth <- d %>%
      dplyr::filter(!is.na(FISHING_DEPTH_FATHOMS)) %>%
      dplyr::select(year, depth = FISHING_DEPTH_FATHOMS)
  }
  dpth <- dpth %>%
    dplyr::filter(depth >= min_depth_cutoff) %>%
    dplyr::mutate(depth = depth * 1.8288)
  dpth <- dpth %>%
    dplyr::group_by(year) %>%
    # do(as.data.frame(t(boxplot.stats(.$depth, coef = 0.8)$`stats`))) %>%
    dplyr::do(as.data.frame(t(as.data.frame(
      quantile(.$depth, probs = c(0.025, 0.25, 0.5, 0.75, 0.975))
    )))) %>%
    dplyr::ungroup() %>%
    dplyr::rename(
      lower95 = 2, lowerhinge = 3,
      median = 4,
      upperhinge = 5, upper95 = 6
    )
  if (!is.null(yrs)) {
    dpth <- dpth %>%
      dplyr::filter(year %in% yrs)
  }
  return(dpth)
}

get_rate_by_month <- function(d,
                              yrs = NULL) {
  dpth <- d %>%
    dplyr::filter(!is.na(crate)) %>%
    dplyr::select(year, months, depth = crate)
  dpth <- dpth %>%
    dplyr::group_by(year, months) %>%
    dplyr::do(as.data.frame(t(as.data.frame(
      round(quantile(.$depth, probs = c(0.025, 0.25, 0.5, 0.75, 0.975)), 5)
    )))) %>%
    dplyr::ungroup() %>%
    dplyr::rename(
      lower95 = 3, lowerhinge = 4,
      median = 5,
      upperhinge = 6, upper95 = 7
    )
  if (!is.null(yrs)) {
    dpth <- dpth %>%
      dplyr::filter(year %in% yrs)
  }
  return(dpth)
}
