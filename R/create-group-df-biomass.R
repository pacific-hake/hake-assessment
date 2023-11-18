#' Create a data frame containing model output of biomass from a
#' model list which is in long format ready for [ggplot2::ggplot()]
#'
#' @param model_lst A list of models, each created by [create_rds_file()]
#' @param model_names A vector of model names,the same length as `model_lst`
#' @param rel Logical. If `TRUE` return a list of one element, the relative
#' biomass (no B0 element)
#' @param bo_yr The number of years prior to the start of the time series
#' that you want to use as a plot point for B0 on biomass plots.
#' @param ... Arguments passed to [extract_mcmc_quant()]
#'
#' @return A list or one or two [tibble::tibble()]. If `rel` is `TRUE`, one
#' (biomass) If `rel` is `FALSE`, two (biomass and B0)
#'
#' @export
create_group_df_biomass <- function(model_lst = NULL,
                                    model_names = NULL,
                                    rel = FALSE,
                                    bo_yr = 4,
                                    ...){

  init_year <- model_lst[[1]]$startyr - bo_yr

  vals <- paste0(ifelse(rel, "d", "s"),
                 c("lower", "med", "upper"))

  d <- bind_cols(extract_mcmc_quant(model_lst,
                                    model_names,
                                    type = vals[1],
                                    inc_model_year = TRUE,
                                    ...),
                 extract_mcmc_quant(model_lst,
                                    model_names,
                                    type = vals[2],
                                    ...),
                 extract_mcmc_quant(model_lst,
                                    model_names,
                                    type = vals[3],
                                    ...)) |>
    mutate(model = factor(model, levels = model_names),
           year = as.numeric(year))

  if(rel){
    return(list(d = d))
  }

  bo <- map(model_lst, ~{
    .x$mcmccalcs$sinit
  }) |>
    map_dfr(~{.x}) |>
    mutate(model = model_names) |>
    mutate(year = init_year) |>
    select(model, year, everything()) |>
    setNames(c("model", "year", "slower", "smed", "supper")) |>
    mutate(model = factor(model, levels = model_names),
           year = as.numeric(year))

  list(d = d, bo = bo)
}