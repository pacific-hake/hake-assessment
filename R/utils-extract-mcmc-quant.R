#' Extract MCMC quantiles from models and return them
#' in a [ggplot2::ggplot()]-ready data frame
#'
#' @param model_lst A list of models, each created by [create_rds_file()]
#' @param model_names A vector of model names,the same length as `models_lst`
#' @param type A name as found in an `mcmccalcs` object of a model object,
#' for example extraction of: `base_model$mcmccalcs$smed` would require
#' `type` = "smed"
#' @param inc_model_year Logical. If `TRUE`, include the `model` and `year`
#' columns in the data frame
#' @param end_yrs A vector of the end years for each model.
#' If one value, it will apply to all models
#'
#' @return A [tibble::tibble()]
#' @export
extract_mcmc_quant <- function(model_lst,
                               model_names,
                               type,
                               inc_model_year = FALSE,
                               end_yrs = year(Sys.time())){

  if(length(end_yrs) == 1){
    end_yrs <- rep(end_yrs, length(model_lst))
  }

  if(length(end_yrs) != length(model_lst)){
    stop("Length of `end_yrs` does not equal the length ",
         "of `model_lst`")
  }

  out <- map2(model_lst, end_yrs, ~{
    tmp <- .x$mcmccalcs[[type]]
    type_nms <- names(tmp)
    type_nms <- type_nms[suppressWarnings(!is.na(as.numeric(type_nms)))]
    tmp <- tmp[type_nms]
    tmp[as.numeric(type_nms) <= .y]
  }) |>
    map_dfr(~{.x}) |>
    mutate(model = model_names) |>
    select(model, everything()) |>
    pivot_longer(-"model",
                 names_to = "year",
                 values_to = type)

  if(inc_model_year){
    out
  }else{
    out |>
      select(-c(model, year))
  }
}