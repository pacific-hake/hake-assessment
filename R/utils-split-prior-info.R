#' Extract priors information from a string with a particular format into
#' a vector
#'
#' @param prior_str A string with the format `Lognormal(2.0,1.01)`
#' @param dec_points The number of decimal points to use
#' @param first_to_lower Make the first letter of the prior name lower case
#'
#' @return A vector of length 3 with the following format:
#' `c("Lognormal", 2.0, 1.01)`
#' @export
split_prior_info <- function(prior_str,
                             dec_points = 1,
                             first_to_lower = FALSE){

  p <- strsplit(prior_str, "\\(")[[1]]

  if(first_to_lower){
    p[1] <- paste0(tolower(substr(p[1], 1, 1)),
                   substr(p[1],
                          2,
                          nchar(p[1])))
  }
  p_type <- p[1]
  p <- strsplit(p[2], ",")[[1]]
  p_mean <- f(as.numeric(p[1]), dec_points)
  p_sd <- f(as.numeric(gsub(")", "", p[2])), dec_points)

  c(p_type, p_mean, p_sd)
}
