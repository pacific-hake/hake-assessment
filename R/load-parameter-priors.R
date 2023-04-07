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
    filter(grepl("DM_theta", Label))
  out$effn_prior <- unlist(out$effn_priors[1, ])
  out$sel_phi_val <- model$parameters |>
    as_tibble() |>
    filter(Label == "AgeSel_P3_Fishery(1)_dev_se") |>
    pull(Value)

  out$log_theta_fishery <-
    round(model$parameters["ln(EffN_mult)_1", "Value"], 3)
  out$log_theta_survey <-
    round(model$parameters["ln(EffN_mult)_2", "Value"], 3)
  out$theta_fishery <-
    exp(model$parameters["ln(EffN_mult)_1", "Value"])
  out$theta_survey <-
    exp(model$parameters["ln(EffN_mult)_2", "Value"])

  pat <- "AgeSel.*Fishery.*DEV"
  mp <- model$parameters |>
    as_tibble()
  out$param_recdevs <- mp |>
    filter(grepl(pat, Label))

  out$param_recdevs_se <- mp |>
    filter(Label == "AgeSel_P3_Fishery(1)_dev_se") |>
    pull(Value)

  out

}