#' Get the posterior values for the given regular expressions of
#' parameter names
#'
#' @param model The SS model output as loaded by [create_rds_file()]
#' @param params_regex A vector of regular expressions used to extract data
#' for parameter names. Default value is [key_posteriors]
#' @param params_titles A list of names for the given elements of
#' `params_regex`. Default value is [key_posteriors_titles]
#' @param ... Absorbs arguments intended for other functions
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
                               params_regex = key_posteriors,
                               params_titles = key_posteriors_titles,
                               ...){

  stopifnot(class(params_regex) == "list")
  walk(params_regex, ~{
    stopifnot(class(.x) == "character")
  })
  stopifnot(length(params_regex) == length(params_titles))

  df <- map_dfc(params_regex, \(param_regex){
    model$mcmc |>
      select(matches(param_regex))
  }) |>
    as_tibble()

  if(ncol(df) == length(params_titles)){
    names(df) <- params_titles
  }else{
    warning("The number of columns that matched the regex's `params_regex` ",
            "does not match the length of the names argument ",
            "`params_titles` so no pretty names were set for the extracted ",
            "posteriors")
  }

  df
}
