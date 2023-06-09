#' Title
#'
#' @param model A model, created by [create_rds_file()]
#' @param posterior_regex A list of regular expressions to match key
#' posteriors. See the package data item [key_posteriors]
#' @param digits The number of decimal places to report in the table
#' @param font_size The size of the font for all text in the table
#'
#' @return A [kableExtra::kbl()] table object
#' @export
table_parameters_long_summary <- function(model,
                                          posterior_regex = key_posteriors,
                                          digits = 4,
                                          font_size = 12){

  mc <- model$mcmc |>
    as_tibble()
  mc_nms <- names(mc)

  # Create the data frame with correct format for the key posteriors
  key_grep <- unique(grep(paste(posterior_regex, collapse = "|"), mc_nms))
  key_par <- mc |>
    select(all_of(key_grep))
  key_nms <- tibble(Parameter = names(key_par))
  key_meds <- tibble(`Posterior median` = apply(key_par, 2, median))
  d <- key_nms |>
    bind_cols_quiet(key_meds)

  add_params <- function(d, mc, inds){

    meds <- mc |>
      select(all_of(inds)) |>
      apply(2, median)
    new_d <- tibble(Parameter = names(meds),
                    `Posterior median` = meds)
    d |>
      bind_rows(new_d)
  }

  # Add all Early_InitAge parameters
  ei <- grep("Early_InitAge_[0-9]+", mc_nms)
  d <- add_params(d, mc, ei)

  # Add all Recruitment deviation parameters
  rec <- c(grep(".*_RecrDev_[0-9]+", mc_nms),
           grep("ForeRecr_[0-9]+", mc_nms))
  d <- add_params(d, mc, rec)

  # Add all AgeSel parameters
  age_sel <- grep("AgeSel_.*", mc_nms)
  d <- add_params(d, mc, age_sel)

  d <- d |>
    mutate(`Posterior median` = f(`Posterior median`, digits))
  kbl(d,
      align = "lr",
      format = "latex",
      linesep = "",
      booktabs = TRUE,
      longtable = TRUE,
      caption = paste0("Medians of estimated parameters ",
                       "for the base model.")) |>
    row_spec(0, bold = TRUE) |>
    kable_styling(font_size = font_size,
                  latex_options = c("repeat_header"))
}
