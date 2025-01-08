#' Get the prior and MLE values for the given regular expressions of
#' parameter names
#'
#' @param model A model object as created by [create_rds_file()]
#' @param params_regex A vector of regular expressions used to extract data
#' for parameter names. Default value is [key_posteriors]
#' @param params_titles A list of names for the given elements of
#' `params_regex`. Default value is [key_posteriors_titles]
#' @param n_points_prior An integer specifying the number of points you want to
#' be generated from the `r<distribution>()` function, e.g.,
#' `rnorm(n_points_prior)`.
#' @param ... Absorbs arguments intended for other functions
#'
#' @return A list of prior and MLE data, one for each of the regular
#' expressions in `params_regex`
#' @export
#' @examples
#' \dontrun{
#' get_prior_data(base_model, "BH_steep")
#' get_prior_data(base_model, "e")
#' get_prior_data(base_model, "asdfg")
#' get_prior_data(base_model, c("NatM", "SR_LN", "SR_BH_steep", "Q_extraSD"))
#' }
get_prior_data <- function(model,
                           params_regex = key_posteriors,
                           params_titles = key_posteriors_titles,
                           n_points_prior = 1e5,
                           ...){

  stopifnot(class(params_regex) == "list")
  walk(params_regex, ~{
    stopifnot(class(.x) == "character")
  })
  stopifnot(length(params_regex) == length(params_titles))

  params <- model$parameters |>
    as_tibble() %>%
    set_names(tolower(names(.)))

  pconst <- 0.0001

  priors_lst <- map(params_regex, \(pat){

    parind <- grep(pat, params$label)
    if(length(parind) > 1){
      stop("get_prior_data(): The regular expression ", pat, " matched more than one (",
           length(parind), ") parameter names")
    }
    if(length(parind) < 1){
      message("get_prior_data(): The regular expression ", pat, " did not ",
              "match any parameter names")
      return(NULL)
    }else{
      parline <- params[parind, ]
      #message("get_prior_data(): The regular expression matched ", parline$label)
      initval <- parline$init
      finalval <- parline$value
      parsd <- parline$parm_stdev

      pmin <- parline$min
      pmax <- parline$max

      ptype <- ifelse(is.na(parline$pr_type),
                      "Normal",
                      parline$pr_type)
      psd <- parline$pr_sd
      pr <- parline$prior
      pval <- seq(pmin,
                  pmax,
                  length = ncol(model$mcmc))

      prior_like <- switch(ptype,
                           "Log_Norm" = {
                             0.5 * ((log(pval) - pr) / psd) ^ 2
                           },
                           "Full_Beta" = {
                             mu <- (pr - pmin) / (pmax - pmin);  # CASAL's v
                             tau <- (pr - pmin) * (pmax - pr) / (psd ^ 2) - 1.0
                             aprior <- tau * (1 - mu)  # CASAL's m and n
                             bprior <- tau * mu
                             if(aprior <= 1.0 | bprior <= 1.0) {
                               warning("Bad Beta prior for parameter ", parline$label)
                             }
                             (1.0 - bprior) * log(pconst + pval - pmin) +
                               (1.0 - aprior) * log(pconst + pmax - pval) -
                               (1.0 - bprior) * log(pconst + pr - pmin) -
                               (1.0 - aprior) * log(pconst + pmax - pr)
                           },
                           "No_prior" = {
                             rep(0.0, length(pval))
                           },
                           "Normal" = {
                             0.5 * ((pval - pr) / psd) ^ 2
                           },
                           {
                             warning("No prior found for parameter ", parline$Label)
                             NA
                           }
      )
      prior <- NA
      if(!is.na(prior_like[1])){
        prior <- exp(-1 * prior_like)
      }

      mle <- NULL
      if(!is.na(parsd) && parsd > 0){
        mle <- dnorm(pval, finalval, parsd)
        mlescale <- 1 / (sum(mle) * mean(diff(pval)))
        mle <- mle * mlescale
      }

      distribution_pts <- switch(
        ptype,
        "Normal" = rnorm(n_points_prior, pr, psd),
        "Log_Norm" = rlnorm(n_points_prior, pr, psd),
        "Full_Beta" = rbeta_ab(n_points_prior, pr, psd, min = pmin, max = pmax),
        "No_prior" = runif(n_points_prior, min = pmin, pmax)
      )
      distribution_pts[distribution_pts < pmin |
                         distribution_pts > pmax] <- NA
      return(list(initval = initval,
                  finalval = finalval,
                  parsd = parsd,
                  pmin = pmin,
                  pmax = pmax,
                  ptype = ptype,
                  psd = psd,
                  pr = pr,
                  pval = pval,
                  prior = prior,
                  mle = mle,
                  prior_random = distribution_pts))
    }
  })

  param_recdevs <- params |>
    dplyr::filter(grepl("AgeSel.*Fishery.*DEV", label))
  priors_lst <- c(priors_lst,
                  list(param_recdevs))
  params_titles <- c(params_titles,
                     "Fishery recruitment deviations")

  names(priors_lst) <- params_titles

  priors_lst
}
