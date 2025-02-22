#' Create a data frame containing model output index fits from a
#' model list which is in long format ready for [ggplot2::ggplot()]
#'
#' @param model_lst A list of models, each created by [create_rds_file()]
#' @param survey_type One of `age1`, `age2`, or `edna`
#' @param model_names A vector of model names,the same length as `model_lst`
#'
#' @return A list containing a [tibble::tibble()]
#'
#' @export
create_group_df_index <- function(model_lst = NULL,
                                  model_names = NULL,
                                  survey_type = c("age1",
                                                  "age2",
                                                  "edna")){

  survey_type <- match.arg(survey_type)

  fleet <- ifelse(survey_type == "age2", 2, 3)

  obs <- model_lst[[1]]$dat$CPUE |>
    as_tibble() |>
    dplyr::filter(index == fleet) |>
    select(year, index) |>
    setNames(c("year", "index_med")) |>
    mutate(year = as.numeric(year)) |>
    mutate(model = "Observed") |>
    mutate(index_lo = index_med,
           index_hi = index_med) |>
    select(model, year, index_lo, index_med, index_hi) |>
    mutate_at(.vars = vars(index_lo, index_med, index_hi), ~{.x = .x / 1e6})

  d <- bind_cols(extract_survey_index_fits(model_lst,
                                           model_names,
                                           survey_type = survey_type,
                                           "index_lo",
                                           TRUE),
                 extract_survey_index_fits(model_lst,
                                           model_names,
                                           survey_type = survey_type,
                                           "index_med"),
                 extract_survey_index_fits(model_lst,
                                           model_names,
                                           survey_type = survey_type,
                                           "index_hi")) |>
    bind_rows(obs) |>
    mutate(model = factor(model,
                          levels = c(model_names,
                                     "Observed")),
           year = as.numeric(year))

    # mutate_at(vars(index_lo, index_med, index_hi),
    #           ~{ifelse(model != "Last assessment base model",
    #                    .x / 1e6,
    #                    .x)})

  list(d)
}
