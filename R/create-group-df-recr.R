#' Create a data frame containing model output recruitment from a
#' model list which is in long format ready for [ggplot2::ggplot()]
#'
#' @param model_lst A list of models, each created by [create_rds_file()]
#' @param model_names A vector of model names,the same length as `model_lst`
#' @param devs Logical. If `TRUE` return recruitment deviations, if `FALSE`,
#' return absolute recruitment
#'
#' @return A list containing a [tibble::tibble()]
#'
#' @export
create_group_df_recr <- function(model_lst = NULL,
                                 model_names = NULL,
                                 devs = FALSE){

  vals <- paste0(ifelse(devs, "dev", "r"),
                 c("lower", "med", "upper"))

  d <- bind_cols(extract_mcmc_quant(model_lst,
                                    model_names,
                                    vals[1], TRUE),
                 extract_mcmc_quant(model_lst,
                                    model_names,
                                    vals[2]),
                 extract_mcmc_quant(model_lst,
                                    model_names,
                                    vals[3])) |>
    mutate(model = factor(model, levels = model_names),
           year = as.numeric(year))

  list(d = d)
}