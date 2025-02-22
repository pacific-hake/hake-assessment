#' Extract index fit MCMC quantiles from models and return them
#' in a [ggplot2::ggplot()]-ready data frame
#'
#' @param model_lst A list of models, each created by [create_rds_file()]
#' @param model_names A vector of model names,the same length as `models_lst`
#' @param survey_type One of `age1`, `age2`, or `edna`
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
                                                      "age2",
                                                      "edna"),
                                      type = c("index_med",
                                               "index_lo",
                                               "index_hi"),
                                      inc_model_year = FALSE){

  survey_type <- match.arg(survey_type)
  fleet <- ifelse(survey_type == "age2", 2, 3)

  type <- match.arg(type)
  out <- map2(model_lst, model_names, ~{
    .x$extra_mcmc[[type]] |>
      dplyr::filter(fleet == !!fleet) |>
      ungroup() |>
      mutate(model = .y) |>
      select(-fleet) |>
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
