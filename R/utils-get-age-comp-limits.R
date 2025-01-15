#' Get the start and end year of the age comp data, and maximum
#' proportion overall with its year and age
#'
#' @param model A model as returnded by [load_ss_files()]
#' @param type 1 for Fishery and 2 for Survey
#'
#' @return A vector of 5 elements as described above
#' @export
get_age_comp_limits <- function(model, type = 1){

  dat <- model$dat$agecomp %>%
    set_names(tolower(names(.))) |>
    dplyr::filter(fleet == type) |>
    select(year, matches("^a\\d+$")) %>%
    setNames(gsub("^a", "", names(.)))

  subdat <- dat |>
    select(-year)
  max_row_col <- which(subdat == max(subdat), arr.ind = TRUE)
  max_prop_yr <- dat[max_row_col[1], ]$year
  max_prop_age <- names(subdat)[max_row_col[2]]
  ret_vec <- c(as.integer(min(dat$year)),
               as.integer(max(dat$year)),
               max(subdat),
               max_prop_yr,
               max_prop_age)

  names(ret_vec) <- c("start_yr",
                      "end_yr",
                      "max_prop",
                      "max_prop_yr",
                      "max_prop_age")

  ret_vec
}
