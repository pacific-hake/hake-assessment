# TODO: ----

# TODO: Use data-table objects rather than files
# TODO: Make sure that predictions should be the average of males, females,
#       and unknown sex

# Functions ----------------

#' Estimate time-varying weight at age
#'
#' @details
#' Predict weight-at-age for fishery and survey data without spatial
#' information, where the model includes age, year, cohort, and sex. There is a
#' smoother on age, year is modeled as a random effect, cohort is also a random
#' effect, and sex is a linear predictor to estimate weight, i.e.,
#' (weight ~ 1 + s(age) + (1|fcohort) + (1|fyear) + sex).
#' @param max_age An integer specifying the maximum age of the modeled data
#'   in the Stock Synthesis model. All age data beyond this will be assigned
#'   to the maximum age. This is typically the age beyond which data are sparse
#'   and weight and length are essentially the same across ages. The default is
#'   fifteen for Pacific Hake.
#' @param first_year A four-digit integer specifying the first year of data in
#'   the assessment model. Sometimes weight-at-age data might begin before this
#'   year but the data are so sparse for other data that the model should not
#'   include this year as a separate year. For example, weight-at-age data for
#'   Pacific Hake start in the early 70s, which means that the cohorts are
#'   predicted for years prior to this, i.e., year - age, so we want to remove
#'   these early years with little data. The default for Pacific Hake is 1975.
#' @param out_location A path to a directory where you want to save the
#'   resulting rds file with the predictions from the model. The default is to
#'   save them to the hake server.
#' @return
#' A data frame in long format with time-varying weight-at-age data.
#' @author Kelli F. Johnson
estimate_tv_weight_at_age <- function(
  max_age = 15,
  first_year = 1975,
  out_location = "/srv/hake/other/tv"
) {
  weight_at_age_files <- fs::dir_ls(
    regexp = "weight-at-age.csv",
    here::here("data-tables")
  )

  # Load in data
  weight_at_age_data <- purrr::map_df(
    weight_at_age_files,
    utils::read.csv,
    .id = "path"
  ) |>
    dplyr::mutate(
      age = ifelse(Age_yrs > 15, 15, Age_yrs),
      cohort = Year - age,
      Sex = tidyr::replace_na(Sex, "U"),
      fyear = as.factor(Year),
      fcohort = as.factor(cohort)
    ) |>
    dplyr::rename(
      weight = Weight_kg,
      sex = Sex
    ) |>
    dplyr::rename_with(.fn = tolower) |>
    dplyr::filter(weight > 0)

  # do some more manipulation and save it to a csv file that stores
  # the sample-size information to be plotted later
  weight_at_age_data |>
    dplyr::group_by(year, age) |>
    dplyr::count() |>
    dplyr::arrange(age) |>
    dplyr::ungroup() |>
    dplyr::bind_rows(
      dplyr::group_by(weight_at_age_data, age) |>
      dplyr::count() |>
      dplyr::mutate(year = -1940)
    ) |>
    tidyr::pivot_wider(
      names_from = age,
      values_from = n,
      names_sort = FALSE,
      names_glue = "a{age}"
    ) |>
    dplyr::arrange(year) |>
    dplyr::rename(Yr = year) |>
    utils::write.csv(
      fs::path(here::here("data-tables"), "wtatage-all-samplesize.csv"),
      row.names = FALSE
    )

  # model
  m1 <- sdmTMB::sdmTMB(
    data = weight_at_age_data,
    formula = weight ~ 1 + s(age) + (1|fcohort) + (1|fyear) + sex,
    family = sdmTMB::lognormal(link = "log"),
    spatial = "off",
    spatiotemporal = "off",
    time = "year",
    control = sdmTMB::sdmTMBcontrol(newton_loops = 4)
  )
  # create prediction grid
  pred_grid <- expand.grid(
    year = unique(weight_at_age_data[["year"]]),
    sex = unique(weight_at_age_data[["sex"]]),
    age = 0:15
  ) |>
    dplyr::mutate(
      cohort = year - age
    ) |>
    dplyr::filter(
      cohort %in% intersect(
        cohort,
        weight_at_age_data[["cohort"]]
      )
    ) |>
    dplyr::mutate(
      fyear = as.factor(year),
      fcohort = as.factor(cohort)
    )

  # Get estimates
  preds <- predict(m1, newdata = pred_grid) |>
    dplyr::mutate(est_weight = exp(est)) |>
    dplyr::filter(year >= first_year)
  saveRDS(
    preds,
    file = fs::path(out_location, "weight-at-age-preds.rds")
  )
  # make EWAA
  # Where it is the average of all sexes.
  ewaa <- dplyr::group_by(preds, year, age) |>
    dplyr::summarise(
      n = dplyr::n(),
      pred_weight = mean(exp(est))
    ) |>
    as.data.frame()

  ewaa_long <- ewaa |>
    dplyr::select(year, age, pred_weight)

  ewaa_wide <- tidyr::pivot_wider(
    ewaa_long,
    names_from = age,
    values_from = pred_weight
  ) |>
    dplyr::relocate(`0`, .before = `1`) |>
    as.matrix()

  utils::write.csv(
    ewaa_long,
    fs::path(here::here("data-tables"), "weight-at-age-ogives.csv"),
    row.names = FALSE
  )

  lines_of_weight_at_age_per_cohort <- ggplot2::ggplot(
    preds[preds$sex == "F", ],
    ggplot2::aes(x = age, y = est_weight)
  ) +
    ggplot2::geom_line(ggplot2::aes(col = fcohort)) +
    ggplot2::theme(legend.position = "none") +
    ggplot2::labs(subtitle = "female weight-at-age per cohort")

  return(ewaa_long)
}
