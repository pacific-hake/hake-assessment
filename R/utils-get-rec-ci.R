#' Get recruitment information for a cohort from two models with
#' an CI envelope % difference calculation
#'
#' @param model1 A model object, created with [create_rds_file()]
#' @param model2 A model object, created with [create_rds_file()]
#' @param yr The cohort (year) to extract
#' @param probs The 3-vector of probabilities to be passes to the
#' [quantile()] function.
#' @param digits Number of decimal points to round values to
#' @param perc_digits Number of decimal points to round percentages to
#'
#' @return A list of length 3: Item 1 is a vector of the `model1`
#' recruitment lower, median, upper, and size of credible interval
#' (`env_diff` column). Item 2 is the same type object for `model2`,
#' and item 3 is the ratio of the `model2` `env_diff` divided by that
#' of `model1` as a percentage.
#' @export
get_rec_ci <- function(model1,
                       model2,
                       yr,
                       digits = 3,
                       perc_digits = 0){

  mc1 <- model1$mcmccalcs
  mc2 <- model2$mcmccalcs

  rec1 <- c(mc1$rlower[names(mc1$rlower) == yr],
            mc1$rmed[names(mc1$rmed) == yr],
            mc1$rupper[names(mc1$rupper) == yr])

  rec2 <- c(mc2$rlower[names(mc2$rlower) == yr],
            mc2$rmed[names(mc2$rmed) == yr],
            mc2$rupper[names(mc2$rupper) == yr])
  rec1_env <- rec1[3] - rec1[1]
  rec2_env <- rec2[3] - rec2[1]
  rec_diff_perc <- rec2_env / rec1_env * 100
  rec1 <- c(rec1, rec1_env)
  nms <- c("lo", "med", "hi", "env_diff")
  names(rec1) <- nms
  rec2 <- c(rec2, rec2_env)
  names(rec2) <-nms
  list(f(rec1, digits),
       f(rec2, digits),
       f(rec_diff_perc, perc_digits))
}
