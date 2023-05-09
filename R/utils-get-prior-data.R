#' Get the prior and MLE values for the given regular expressions of
#' parameter names
#'
#' @param model A model object as created by [create_rds_file()]
#' @param params_regex A vector of regular expressions used to extract data
#' for parameter names. If there are no matches, or more than one for any
#' regular expression, the program will stop.
#' @param n_points_prior An integer specifying the number of points you want to
#'   be generated from the `r<distribution>()` function, e.g.,
#'   `rnorm(n_points_prior)`.
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
                           params_regex = NULL,
                           param_titles = NULL,
                           n_points_prior = 1000){

  browser()
  stopifnot(class(params_regex) == "character")

  params <- model$parameters |>
    as_tibble() %>%
    set_names(tolower(names(.)))

  priors_list <- list()
  pconst <- 0.0001

  for(i in seq_along(params_regex)){
    parind <- grep(params_regex[i], params$label)
    if(length(parind) < 1){
      stop("The regular expression ", params_regex[i],
           " matched no parameter names", call. = FALSE)
    }
    if(length(parind) > 1){
      stop("The regular expression ", params_regex[i],
           " matched more than one (", length(parind),
           ") parameter names", call. = FALSE)
    }
    parline <- params[parind, ]
    message("The regular expression matched ", parline$label)
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

    if(ptype == "Log_Norm"){
      prior_like <- 0.5 * ((log(pval) - pr) / psd) ^ 2
    }else if(ptype == "Full_Beta"){
      mu <- (pr - pmin) / (pmax - pmin);  # CASAL's v
      tau <- (pr - pmin) * (pmax - pr) / (psd ^ 2) - 1.0
      aprior <- tau * (1 - mu)  # CASAL's m and n
      bprior <- tau * mu
      if(aprior <= 1.0 | bprior <= 1.0) {
        warning("Bad Beta prior for parameter ", parline$label)
      }
      prior_like <- (1.0 - bprior) * log(pconst + pval - pmin) +
        (1.0 - aprior) * log(pconst + pmax - pval) -
        (1.0 - bprior) * log(pconst + pr - pmin) -
        (1.0 - aprior) * log(pconst + pmax - pr)
    }else if(ptype == "No_prior"){
      prior_like <- rep(0.0, length(pval))
    }else if(ptype == "Normal"){
      prior_like <- 0.5*((pval - pr) / psd) ^ 2
    }else{
      warning("No prior found for parameter ", parline$Label)
      prior_like <- NA
    }

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

    random_points <- switch(
      ptype,
      "Normal" = rnorm(n_points_prior, pr, psd),
      "Log_Norm" = rlnorm(n_points_prior, pr, psd),
      "Full_Beta" = rbeta_ab(n_points_prior, pr, psd, min = pmin, max = pmax),
      "No_prior" = runif(n_points_prior, min = pmin, pmax)
    )
    random_points[random_points < pmin | random_points > pmax] <- NA

    priors_list[[i]] <- list(initval = initval,
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
                             prior_random = random_points)

  }

  priors_list
}
