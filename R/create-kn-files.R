#' Create the `nuisance` and `key` posteriors files
#'
#' @param model A model object representing the output from the SS model
#' @param posterior_regex A vector of key posterior names in regular
#' expression formats, which will be glued together with logical OR ('|') to
#' match the names
#' @param key_post_fn Filename for the key posterior file
#' @param nuisance_post_fn Filename for the nuisance posterior file
#'
#' @return Nothing
#' @export
create_kn_files <- function(model,
                            posterior_regex,
                            key_post_fn,
                            nuisance_post_fn){

  if(model$extra_mcmc_exists){
    key_file <- file.path(model$extra_mcmc_path, key_post_fn)
    nuisance_file <- file.path(model$extra_mcmc_path, nuisance_post_fn)
  }else{
    key_file <- file.path(model$mcmc_path, key_post_fn)
    nuisance_file <- file.path(model$mcmc_path, nuisance_post_fn)
  }

  mc <- model$mcmc
  mc_names <- names(mc)
  inds <- grep(paste(posterior_regex, collapse = "|"), mc_names) |>
    unique()
  mcmc_names <- mc_names[inds]
  keys <- mc[, inds]
  nuisances <- mc[, -inds]
  write.csv(keys, key_file, row.names = FALSE)
  write.csv(nuisances, nuisance_file, row.names = FALSE)
}
