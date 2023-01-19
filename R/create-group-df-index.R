#' Create a data frame containing model output index fits from a
#' model list which is in long format ready for [ggplot2::ggplot()]
#'
#' @param model_lst A list of models, each created by [create_rds_file()]
#' @param model_names A vector of model names,the same length as `model_lst`
#' @param type The type of survey, must be one of `age1` or `age2`.
#' `age2` means age 2+ acoustic survey and `age1``is the age 1 acoustic survey
#'
#' @return A list containing a [tibble::tibble()]
#'
#' @export
create_group_df_index <- function(model_lst = NULL,
                                  model_names = NULL,
                                  type = c("age1", "age2")){

  type <- match.arg(type)

  fleet <- ifelse(type == "age2", 2, 3)

  obs <- model_lst[[1]]$dat$CPUE |>
    as_tibble() |>
    filter(index == fleet) |>
    select(-seas, -se_log, -index) |>
    setNames(c("year", "index.med")) |>
    mutate(year = as.numeric(year)) |>
    mutate(model = "Observed") |>
    mutate(index.025 = index.med,
           index.975 = index.med) |>
    select(model, year, index.025, index.med, index.975)

  d <- bind_cols(extract_survey_index_fits(model_lst,
                                           model_names,
                                           type,
                                           "index.025", TRUE),
                 extract_survey_index_fits(model_lst,
                                           model_names,
                                           type,
                                           "index.med"),
                 extract_survey_index_fits(model_lst,
                                           model_names,
                                           type,
                                           "index.975")) |>
    bind_rows(obs) |>
    mutate(model = factor(model,
                          levels = c(model_names,
                                     "Observed")),
           year = as.numeric(year)) |>
    mutate_at(vars(index.025, index.med, index.975),
              ~{.x / 1e6})

  list(d)
}
