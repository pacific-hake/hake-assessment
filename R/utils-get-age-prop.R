#' Create the age prop and the age itself for the ranking of age proportions
#'
#' @details Think of the question "Which is the second-highest number in
#' this vector and what is its index in the vector?" This function returns
#' a vector of those two numbers.
#'
#' @param vec A vector of age proportions
#' @param ranking 1 = max, 2 = second highest, etc.
#'
#' @return The age proportion and the age itself for the ranking of age
#' proportion
#' @export
get_age_prop <- function(vec, ranking = 1){
  prop <- rev(sort(vec))
  prop <- prop[ranking]
  age <- as.numeric(names(vec[vec == prop]))
  c(age, prop)
}
