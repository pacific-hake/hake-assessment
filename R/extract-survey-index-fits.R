#' Extract index fit MCMC quantiles from models and return them
#' in a [ggplot2::ggplot()]-ready data frame
#'
#' @param model_lst A list of models, each created by [create_rds_file()]
#' @param model_names A vector of model names,the same length as `models_lst`
#' @param survey_type The type of survey, must be one of `age1` or `age2`.
#' `age2` means age 2+ acoustic survey and `age1``is the age 1 acoustic survey
#' @param type A name as found in the `extra_mcmc[[type]]` object of a model
#' objects, for example if `type == index.med`, it is a table of index fit
#' @param inc_model_year Logical. If `TRUE`, include the model and year columns
#' in the output data frame
#'
#' @return A [tibble::tibble()]
#' @export
extract_survey_index_fits <- function(model_lst,
                                      model_names,
                                      survey_type = c("age1",
                                                      "age2"),
                                      type = c("index.med",
                                               "index.025",
                                               "index.975"),
                                      inc_model_year = FALSE){

  survey_type <- match.arg(survey_type)
  fleet <- ifelse(survey_type == "age2", 2, 3)

  type <- match.arg(type)

  out <- map2(model_lst, model_names, ~{
    .x$extra_mcmc[[type]] |>
      filter(Fleet == fleet) |>
      ungroup() |>
      mutate(model = .y) |>
      select(-Fleet) |>
      select(model, everything()) |>
      setNames(c("model", "year", type)) |>
      mutate(year = as.numeric(year))
  }) |>
    map_dfr(~{.x})

  if(inc_model_year){
    out
  }else{
    out |>
      select(-c(model, year))
  }
}
