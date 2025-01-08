#' Creates a table showing the forecast age comp percentages by number and
#' weight for select cohorts
#'
#' @param props_bynum A named vector of percentages by number to report. The
#' names must be the ages
#' @param props_bywt A named vector of percentages by weight to report. The
#' names must be the ages
#' @param ages_filt A vector of the ages to show in the table
#' @param font_size The font size for the text in the table
#'
#' @return A [kableExtra::kbl()] table
#' @export
table_fore_age_props <- function(props_bynum,
                                 props_bywt,
                                 ages_filt = c(2:8, 10, 14),
                                 font_size = 8){

  ages <- names(props_bynum)
  d_bynum <- props_bynum |>
    t() |>
    as_tibble() |>
    rename(perc_bynum = V1) |>
    mutate(age = ages) |>
    mutate(age = as.numeric(age)) |>
    mutate(perc_bynum = as.numeric(perc_bynum)) |>
    mutate(cohort = forecast_yrs[1] - age) |>
    select(age, perc_bynum, cohort) |>
    arrange(desc(age))

  d_bywt <- props_bywt |>
    t() |>
    as_tibble() |>
    rename(perc_bywt = V1) |>
    mutate(age = ages) |>
    mutate(age = as.numeric(age)) |>
    mutate(perc_bywt = as.numeric(perc_bywt)) |>
    mutate(cohort = forecast_yrs[1] - age) |>
    select(age, perc_bywt, cohort) |>
    arrange(desc(age))

  d <- d_bynum |>
    full_join(d_bywt, by = "age") |>
    select(-cohort.y) |>
    select(age, cohort.x, perc_bynum, perc_bywt) |>
    mutate(across(c(perc_bynum, perc_bywt), ~{.x = paste0(.x, "\\%")})) |>
    rename(Age = age,
           `By number` = perc_bynum,
           `By weight` = perc_bywt,
           Cohort = cohort.x) |>
    dplyr::filter(Age %in% ages_filt)

  k <- kbl(d,
           format = "latex",
           booktabs = TRUE,
           align = c("l", "r", "r", "r"),
           linesep = "",
           #col.names = col_names,
           escape = FALSE) |>
    row_spec(0, bold = TRUE) |>
    kable_styling(font_size = font_size)

  k
}