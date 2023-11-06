#' Create the `nuisance` and `key` posteriors CSV files in the output
#' mcmc directory of the model
#'
#' @param model A model object representing the output from the SS model
#'
#' @return Nothing, creates two CSV files
#' @export
create_kn_files <- function(model){

  if(model$extra_mcmc_exists){
    key_file <- file.path(model$extra_mcmc_path, key_posteriors_fn)
    nuisance_file <- file.path(model$extra_mcmc_path, nuisance_posteriors_fn)
  }else{
    key_file <- file.path(model$mcmc_path, key_post_fn)
    nuisance_file <- file.path(model$mcmc_path, nuisance_post_fn)
  }

  mc <- model$mcmc |>
    as_tibble()
  nms <- grep(paste(key_posteriors, collapse = "|"), names(mc), value = TRUE)

  keys <- mc |>
    select_at(.vars = vars(nms))
  nuisances <- mc |>
    select_at(.vars = vars(-nms))

  write_csv(keys, key_file)
  write_csv(nuisances, nuisance_file)
}
