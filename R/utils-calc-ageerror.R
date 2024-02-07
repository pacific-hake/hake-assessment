#' Calculate ageing error matrix
#'
#' Ageing error for Pacific Hake is based on an overall ageing error for each
#' mean age and a multiplier to decrease ageing error for strong cohorts. If you
#' were to plot ageing error, you could see diagonal patterns for cohorts that
#' are moving through the population. This necessitates an ageing error that is
#' specific to each year of data since 1975, i.e., the first year of fishery
#' ages.
#'
#' @return
#' A 2 x 21 matrix is returned with column names. The rationale for the number
#' of columns comes from the number of population age bins plus one because
#' age-0 fish have an ageing error assigned to them as well. The first row is
#' the mean age and the second row is the standard deviation based on a normal
#' distribution. Mean age will be different from 0.5 + age if there is bias.
#' Standard deviation will be different than zero if there imprecision.
#'
#' @param ages The ages that have strong cohorts and decreased ageing error.
#'   Note that this should be the age-bin not the mean age in the first row of
#'   the returned matrix.
#' @param x A vector of ageing error used for each definition of ageing
#'   error in the stock assessment model.
#' @param multiplier A single value that is multiplied times certain
#'   entries of `x` to account for cohort effects. The default for Pacific
#'   Hake is 0.55.
#'
calc_ageerror <- function(ages,
                          x = c(
                            0.329242, 0.329242, 0.346917, 0.368632, 0.395312,
                            0.42809, 0.468362, 0.517841, 0.57863, 0.653316,
                            0.745076, 0.857813, 0.996322, 1.1665, 1.37557,
                            1.63244, 1.858, 2.172, 2.53, 2.934, 3.388
                          ),
                          multiplier = 0.55) {
  names <- 0:20
  names(x) <- names
  x[names(x) %in% ages] <- x[names(x) %in% ages] * multiplier
  out <- rbind(names + 0.5, x)
  rownames(out) <- NULL
  colnames(out) <- paste0("age", floor(names))
  return(out)
}
