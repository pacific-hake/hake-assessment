#' Looks at the prior type p_type and phase, and if uniform will return
#'  "Uniform"
#' If not uniform, it will parse the `vals` and build a string defining
#'  the prior info.
#' If Fixed, it will return the initial value
#' If Lognormal, it will parse the `vals` and build a string defining the
#'  prior info, with the exp function applied
#'
#' @param vals The parameter initial values and settings from the control file
#' @param digits The number of decimal points to return
#'
#' @return A string with the prior type and initial values in parentheses
get_prior_string <- function(row, digits = 2){

  prior_type <- c("0" = "Uniform",
                  "-1" = "Uniform",
                  "2" = "Beta",
                  "3" = "Lognormal",
                  "4" = "Normal",
                  "6" = "Normal")

  if(row$type <= 0 && row$phase > 0){
    return("Uniform")
  }
  if(row$type <= 0 && row$phase < 0){
    # Fixed parameter, initial value
    return(as.character(row$init))
  }

  if(!row$type %in% names(prior_type)){
    stop("Prior type `", row$type, "` not found. See `prior_type` vector in ",
         "get_prior_string()")
  }

  mean <- row$mean
  if(!is.na(suppressWarnings(as.numeric(row$mean)))){
    mean <- f(as.numeric(row$mean), digits)
  }
  sd <- row$sd
  if(!is.na(suppressWarnings(as.numeric(row$sd)))){
    sd <- f(as.numeric(row$sd), digits)
  }

  paste0(prior_type[names(prior_type) == row$type], " (",
         mean, ", ", sd, ")")
}