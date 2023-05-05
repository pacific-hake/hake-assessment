#' Replace '+' with a newline in the given string
#'
#' @details
#' Used mainly for ggplot y axis labels, if they are too long
#' and get cut off
#'
#' @param x A character string
#' @param ... Absorbs other arguments not meant for this function
#'
#' @return A modified character string
add_newlines <- function(x, ...){
  gsub("\\+", "\n", x)
}

# Functions to make table generation easier -----------------------------------

#' Create an RGB string of the specified color and opacity
#'
#' @details Format of returned string is #RRGGBBAA where
#' RR = red, a 2-hexadecimal-digit string
#' GG = green, a 2-hexadecimal-digit string
#' BB = blue, a 2-hexadecimal-digit string
#' AA = opacity, 2-digit string
#'
#' @param color A vector of R color strings or numbers
#' @param opacity A number between 0 and 99
#'
#' @return An RGB string of the specified color and opacity
#' @export
get.shade <- function(color, opacity){

  stopifnot(opacity > 0 & opacity < 100)

  colorDEC <- col2rgb(color)
  if(is.matrix(colorDEC)){
    colorHEX <- matrix(nrow = 3, ncol = ncol(colorDEC))
    shade <- NULL
    for(col in 1:ncol(colorDEC)){
      for(row in 1:nrow(colorDEC)){
        colorHEX[row, col] <- sprintf("%X", colorDEC[row,col])
        if(nchar(colorHEX[row,col]) == 1){
          colorHEX[row, col] <- paste0("0", colorHEX[row,col])
        }
      }
      shade[col] <- paste0("#",
                           colorHEX[1, col],
                           colorHEX[2, col],
                           colorHEX[3, col],
                           opacity)
    }
  }else{
    colorHEX <- sprintf("%X", colorDEC)
    for(i in 1:length(colorHEX)){
      if(nchar(colorHEX[i]) == 1){
        colorHEX[i] <- paste0("0", colorHEX[i])
      }
    }
    shade <- paste0("#", colorHEX[1], colorHEX[2], colorHEX[3], opacity)
  }
  shade
}


#' Add a polygon to a plot
#'
#' @param yrvec A vector of years
#' @param lower A vector of lower CI values
#' @param upper A vector of upper CI values
#' @param color The color to make the polygon lines
#' @param shade.col The shade color to fill in the polygon with
#'
#' @return [base::invisible()]
#' @export
addpoly <- function(yrvec,
                    lower,
                    upper,
                    color = 1,
                    shade.col = NA){

  # max of value or 0
  lower[lower < 0] <- 0
  if(is.na(shade.col)){
    shade.col <- rgb(t(col2rgb(color)), alpha = 0.2 * 255, maxColorValue = 255)
  }
  polygon(x = c(yrvec, rev(yrvec)),
          y = c(lower, rev(upper)),
          border = NA,
          col = shade.col)
  lines(yrvec, lower, lty = 3, col = color)
  lines(yrvec, upper, lty = 3, col = color)
  invisible()
}

#' Get the posterior values for the given regular expressions of
#' parameter names
#'
#' @param model The SS model output as loaded by [create_rds_file()]
#' @param param_regex A vector of regular expressions used to extract data
#' for parameter names. If there are no matches, or more than one for any
#' regular expression, the program will stop
#'
#' @return A list of posterior vectors, one for each of the regular
#' expressions in `param_regex`
#' @export
#' @examples
#' \dontrun{
#' get_posterior_data(base_model, "BH_steep")
#' get_posterior_data(base_model, "e")
#' get_posterior_data(base_model, "asdfg")
#' get_posterior_data(base_model, c("NatM", "SR_LN", "SR_BH_steep",
#'  "Q_extraSD"))
#' }
get_posterior_data <- function(model, param_regex){

  mcmc <- model$mcmc
  if(length(mcmc) == 1 && is.na(mcmc)){
    return(NA)
  }

  params <- model$parameters
  posts_list <- list()

  for(i in seq_along(param_regex)){
    parind <- grep(param_regex[i], params$Label)
    if(length(parind) < 1){
      stop("The regular expression ", param_regex[i],
           " matched no parameter names", call. = FALSE)
    }
    if(length(parind) > 1){
      stop("The regular expression ", param_regex[i],
           " matched more than one (", length(parind),
           ") parameter names", call. = FALSE)
    }
    postparname <- params[parind, ]$Label
    message("The regular expression matched ", postparname)

    # Figure out which column of the mcmc output contains the parameter
    jpar <- grep(param_regex[i], names(mcmc))
    if(length(jpar) == 1){
      posts_list[[i]] <- mcmc[ ,jpar]
    }else{
      warning("Parameter ", postparname, " not found in posteriors")
      posts_list[[i]] <- NA
    }
  }

  #if(length(posts_list) == 1){
  #  posts_list <- posts_list[[1]]
  #}
  posts_list
}

#' Get the prior and MLE values for the given regular expressions of
#' parameter names
#'
#' @param model The SS model output as loaded by [create_rds_file()]
#' @param param_regex A vector of regular expressions used to extract data
#' for parameter names. If there are no matches, or more than one for any
#' regular expression, the program will stop.
#' @param n_points_prior An integer specifying the number of points you want to
#'   be generated from the `r<distribution>()` function, e.g.,
#'   `rnorm(n_points_prior)`.
#'
#' @return A list of prior and MLE data, one for each of the regular
#' expressions in `param_regex`
#' @export
#' @examples
#' \dontrun{
#' get_prior_data(base_model, "BH_steep")
#' get_prior_data(base_model, "e")
#' get_prior_data(base_model, "asdfg")
#' get_prior_data(base_model, c("NatM", "SR_LN", "SR_BH_steep", "Q_extraSD"))
#' }
get_prior_data <- function(model, param_regex, n_points_prior = 3000000){

  stopifnot(class(param_regex) == "character")

  params <- model$parameters
  priors_list <- list()
  Pconst <- 0.0001

  for(i in seq_along(param_regex)){
    parind <- grep(param_regex[i], params$Label)
    if(length(parind) < 1){
      stop("The regular expression ", param_regex[i],
           " matched no parameter names", call. = FALSE)
    }
    if(length(parind) > 1){
      stop("The regular expression ", param_regex[i],
           " matched more than one (", length(parind),
           ") parameter names", call. = FALSE)
    }
    parline <- params[parind, ]
    message("The regular expression matched ", parline$Label)
    initval <- parline$Init
    finalval <- parline$Value
    parsd <- parline$Parm_StDev

    Pmin <- parline$Min
    Pmax <- parline$Max

    Ptype <- ifelse(is.na(parline$Pr_type), "Normal", parline$Pr_type)
    Psd <- parline$Pr_SD
    Pr <- parline$Prior
    Pval <- seq(Pmin, Pmax, length = nrow(model$mcmc))

    if(Ptype == "Log_Norm"){
      Prior_Like <- 0.5 * ((log(Pval) - Pr) / Psd) ^ 2
    }else if(Ptype == "Full_Beta"){
      mu <- (Pr - Pmin) / (Pmax - Pmin);  # CASAL's v
      tau <- (Pr - Pmin) * (Pmax - Pr) / (Psd ^ 2) - 1.0
      Aprior <- tau * (1 - mu)  # CASAL's m and n
      Bprior <- tau * mu
      if(Aprior <= 1.0 | Bprior <= 1.0) {
        warning("Bad Beta prior for parameter ", parline$Label)
      }
      Prior_Like <- (1.0 - Bprior) * log(Pconst + Pval - Pmin) +
        (1.0 - Aprior) * log(Pconst + Pmax - Pval) -
        (1.0 - Bprior) * log(Pconst + Pr - Pmin) -
        (1.0 - Aprior) * log(Pconst + Pmax - Pr)
    }else if(Ptype == "No_prior"){
      Prior_Like <- rep(0.0, length(Pval))
    }else if(Ptype == "Normal"){
      Prior_Like <- 0.5*((Pval - Pr) / Psd) ^ 2
    }else{
      warning("No prior found for parameter ", parline$Label)
      Prior_Like <- NA
    }

    prior <- NA
    if(!is.na(Prior_Like[1])){
      prior <- exp(-1 * Prior_Like)
    }

    mle <- NULL
    if(!is.na(parsd) && parsd > 0){
      mle <- dnorm(Pval, finalval, parsd)
      mlescale <- 1 / (sum(mle) * mean(diff(Pval)))
      mle <- mle * mlescale
    }

    random_points <- switch(
      Ptype,
      "Normal" = rnorm(n_points_prior, Pr, Psd),
      "Log_Norm" = rlnorm(n_points_prior, Pr, Psd),
      "Full_Beta" = rbeta_ab(n_points_prior, Pr, Psd, min = Pmin, max = Pmax),
      "No_prior" = runif(n_points_prior, min = Pmin, Pmax)
    )
    random_points[random_points < Pmin | random_points > Pmax] <- NA

    priors_list[[i]] <- list(initval = initval,
                             finalval = finalval,
                             parsd = parsd,
                             Pmin = Pmin,
                             Pmax = Pmax,
                             Ptype = Ptype,
                             Psd = Psd,
                             Pr = Pr,
                             Pval = Pval,
                             prior = prior,
                             mle = mle,
                             prior_random = random_points)

  }

  priors_list
}
