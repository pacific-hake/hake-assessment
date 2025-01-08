#' Load values (priors) from the `parameters` data frame in the model output
#'
#' @param model A model list as created by [create_rds_file()]
#'
#' @return A list of values extracted from the `parameter` data frame
#' @export
load_parameter_priors <- function(model){

  out <- list()

  out$effn_priors <- model$parameters |>
    as_tibble() |>
    select(Label, Prior, Pr_SD) |>
    dplyr::filter(grepl("DM_theta", Label))
  out$effn_prior <- unlist(out$effn_priors[1, ])
  out$sel_phi_val <- model$parameters |>
    as_tibble() |>
    dplyr::filter(Label == "AgeSel_P3_Fishery(1)_dev_se") |>
    pull(Value)

  pat <- "AgeSel.*Fishery.*DEV"
  mp <- model$parameters |>
    as_tibble()
  out$param_recdevs <- mp |>
    dplyr::filter(grepl(pat, Label))

  out$param_recdevs_se <- mp |>
    dplyr::filter(Label == "AgeSel_P3_Fishery(1)_dev_se") |>
    pull(Value)

  out

}