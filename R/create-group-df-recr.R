#' Create a data frame containing model output recruitment from a
#' model list which is in long format ready for [ggplot2::ggplot()]
#'
#' @param model_lst A list of models, each created by [create_rds_file()]
#' @param model_names A vector of model names,the same length as `model_lst`
#' @param devs Logical. If `TRUE` return recruitment deviations, if `FALSE`,
#' return absolute recruitment
#' @param ... Arguments passed to [extract_mcmc_quant()]
#'
#' @return A list containing a [tibble::tibble()]
#'
#' @export
create_group_df_recr <- function(model_lst = NULL,
                                 model_names = NULL,
                                 devs = FALSE,
                                 relative = FALSE,
                                 ...){

  vals <- paste0(ifelse(devs,
                        "dev",
                        ifelse(relative,
                               "r_rel_",
                               "r")),
                 c("lower", "med", "upper"))
  if(!devs){
    # Needed for the x's on the recruitment plot
    vals <-  c(vals, ifelse(relative,
                            "r_rel_mean",
                            "rmean"))
  }

  d <- bind_cols(extract_mcmc_quant(model_lst,
                                    model_names,
                                    vals[1],
                                    TRUE,
                                    ...),
                 extract_mcmc_quant(model_lst,
                                    model_names,
                                    vals[2],
                                    ...),
                 extract_mcmc_quant(model_lst,
                                    model_names,
                                    vals[3],
                                    ...))
  if(!devs){
    d <- bind_cols(d,
                   extract_mcmc_quant(model_lst,
                                      model_names,
                                      vals[4],
                                      ...))
  }
  d <- d |>
    mutate(model = factor(model, levels = model_names),
           year = as.numeric(year))

  list(d = d)
}