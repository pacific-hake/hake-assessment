#' Calculate Autocorrelation, Effective sample size, Geweke statistic, and
#' Heidelberger & Welch statistic histograms. This is a re-coded version of
#' https://github.com/r4ss/r4ss/blob/bioscale/R/mcmc.nuisance.R
#'
#' @param model A model as output by [create_rds_file()]
#'
#' @return A data frame with the 4 values as columns, with the same number
#' of rows as there are parameters calculated for
#' @export
calc_mcmc_param_stats <- function(model){

  param_nms <- model$post_names
  if(!all(param_nms %in% names(model$mcmc))){
    stop("Not all posterior names in the `model$post_names` object were ",
         "found in the `model$mcmc` data frame",
         call. = FALSE)
  }
  mc <- model$mcmc |>
    as_tibble() |>
    select(all_of(param_nms)) |>
    # `set_names(NULL)` is so that the `imap()` call below has indices for
    # the `ind` argument instead of names
    set_names(NULL)
  draws <- nrow(mc)

  d <- imap(mc, \(param, ind){
    acftemp <- acf(param,
                   lag.max = 1,
                   type = "correlation",
                   plot = FALSE)
    acoruse <- round(acftemp$acf[2], 3)
    out_vec <- acoruse
    # Geweke statistic
    if(acoruse > 0.4){
      gewuse <- 3
    }else{
      geweke <- geweke.diag(mcmc(param), frac1 = 0.1, frac2 = 0.5)
      gewuse <- round(geweke$z, 3)
    }
    gewuse <- ifelse(gewuse > 3,
                     3,
                     ifelse(gewuse < -3,
                            -2.9,
                            gewuse))
    out_vec <- c(out_vec, gewuse)
    # Effective sample size
    x <- enframe(param, name = NULL)
    spec <- spectrum0.ar(x)$spec
    effsize <- round(ifelse(spec == 0, 0, nrow(x) * var(x) / spec), 0)
    out_vec <- c(out_vec, min(effsize, draws))
    # Heidelberger and Welch statistic
    if(acoruse > 0.4){
      hwuse <- "No test"
    }else{
      hw <- as.vector(heidel.diag(mcmc(param), pvalue = 0.05))
      hwuse <- ifelse(hw[1], "Passed", "Failed")
    }
    out_vec <- c(out_vec, hwuse, param_nms[ind])

    vec2df(out_vec, nms = c("autocor",
                            "geweke",
                            "effn",
                            "heidelwelsch",
                            "label"))
  }) |>
    map_df(~{.x}) |>
    mutate(across(c("autocor",
                    "geweke",
                    "effn"), ~{as.numeric(.x)}))

  #ro_loc <- grep("^SR_LN\\(R0\\)$", param_nms)

  d
}

