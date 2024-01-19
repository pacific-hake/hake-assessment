#' Create the `nuisance` and `key` posteriors CSV files in the output
#' mcmc directory of the model
#'
#' @param model A model object representing the output from the SS model
#'
#' @return Nothing, creates two CSV files
#' @export
get_key_nuisance_posts <- function(model){

  mc <- model$mcmc |>
    as_tibble()

  nms <- grep(paste(key_posteriors, collapse = "|"),
              names(mc),
              value = TRUE)

  list(key_posts = mc |>
         select_at(.vars = vars(nms)),
       nuisance_posts = mc |>
         select_at(.vars = vars(-nms)))
}
