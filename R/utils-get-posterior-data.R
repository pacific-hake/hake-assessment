#' Get the posterior values for the given regular expressions of
#' parameter names
#'
#' @param model The SS model output as loaded by [create_rds_file()]
#' @param params_regex A vector of regular expressions used to extract data
#' for parameter names
#'
#' @return A data frame of posterior vectors, one column for each of
#' the regular expressions in `params_regex`
#' @export
#' @examples
#' \dontrun{
#' get_posterior_data(base_model, "BH_steep")
#' get_posterior_data(base_model, "e")
#' get_posterior_data(base_model, "asdfg")
#' get_posterior_data(base_model, c("NatM", "SR_LN", "SR_BH_steep",
#'  "Q_extraSD"))
#' }
get_posterior_data <- function(model,
                               params_regex = NULL){

  map_dfc(params_regex, \(param_regex){
    model$mcmc |>
      select(matches(param_regex))
  }) |>
    as_tibble()
}
