#' Get recruitment information for a cohort from two models with
#' an CI envelope % difference calculation
#'
#' @param model1 A model, typically last year's model
#' @param model2 A model, typically this year's model
#' @param year The cohort to extract
#' @param probs The 3-vector of probabilities to be passes to the
#' [quantile()] function.
#' @param scale Scaling factor
#' @param decimals Number of decimal points to round values to
#' @param perc.decimals Number of decimal points to round percentages to
#'
#' @return A list of length 3: Item 1 is a vector of the `model1`
#' recruitment lower, median, upper, and size of credible interval
#' (`env_diff` column). Item 2 is the same type object for `model2`,
#' and item 3 is the ratio of the `model2` `env_diff` divided by that
#' of `model1` as a percentage.
#' @export
get_rec_ci <- function(model1,
                       model2,
                       year,
                       probs = c(0.025, 0.5, 0.975),
                       scale = 1e6,
                       decimals = 3,
                       perc.decimals = 0){
  col <- paste0("Recr_", year)
  rec1 <- quantile(model1$mcmc[[col]] / scale, probs)
  rec2 <- quantile(model2$mcmc[[col]] / scale, probs)
  rec1.env <- rec1[3] - rec1[1]
  rec2.env <- rec2[3] - rec2[1]
  rec.diff.perc <- rec2.env / rec1.env * 100
  rec1 <- c(rec1, rec1.env)
  names(rec1)[4] <- "env_diff"
  rec2 <- c(rec2, rec2.env)
  names(rec2)[4] <- "env_diff"
  list(f(rec1, decimals),
       f(rec2, decimals),
       f(rec.diff.perc,
         perc.decimals))
}

#' Get the median proportions of biomass-at-ages for the final year (MCMC)
#' for all cohorts
#'
#' @param model A model as loaded by [create_rds_file()]
#' @param curr_yr is the current year. The years will decrease from this to
#' give the cohorts year values
#' @param probs Probabilities used for quantile calculations
#'
#' @return A single-column tibble representing the proportion of biomass-at
#' ages for the terminal year
#' @export
get_baa <- function(model,
                    curr_yr,
                    probs = c(0.025, 0.5, 0.975)){
  model$extra_mcmc$natselwt.prop %>%
    map_df(function(x){
      quantile(x, probs = probs)
    }) %>%
    t() %>%
    as.data.frame() %>%
    as_tibble() %>%
    mutate(Cohort = curr_yr - row_number() + 1) %>%
    rename(`Lower CI` = V1,
           Median = V2,
           `Upper CI` = V3) %>%
    select(Cohort,
           `Lower CI`,
           Median,
           `Upper CI`)
}

baa_table <- function(d,
                      xcaption = NULL,
                      xlabel   = NULL,
                      font.size = 13,
                      space.size = 14,
                      decimals = 3){
  tab <- d %>%
    mutate_at(vars(-Cohort), ~{f(.x, decimals)})

  size.string <- latex_size_str(font.size, space.size)
  print(xtable(tab, caption = xcaption,
               label = xlabel,
               align = get.align(ncol(tab),
                                 first.left = FALSE,
                                 just = "r"),
               digits = rep(0, ncol(tab) + 1)),
        caption.placement = "top",
        include.rownames = FALSE,
        table.placement = "H",
        sanitize.text.function = function(x){x},
        size = size.string)
}

