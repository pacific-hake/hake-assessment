#' Extract the `sigma_R_info` table from a data frame of recruitment parameters
#'
#' @details Extract the `sigma_R_info` table that is available in the object
#' returned from [r4ss::SS_output()]. Typically, this table is determined from
#' MLE results, but this function also offers the capability to calculate this
#' info from the table of MCMC results as well.
#'
#' Based on the non-exported function `extract_sigma_R_info()` in [r4ss],
#' but with a wrapper for more than one model
#'
#' @param models A list of models extracted using [r4ss::SS_output()]. If
#' only one model,it must be a list with one element containing the model
#' @param model_names A vector of names, one for each model in `models`.
#' This vector must be the same length as `models`
#' @param sigma_r_in The input value used for sigma_R, this is also available
#' in the list output by [r4ss::SS_output()] and is used to scale the results
#'
#' @export
#' @return A data frame is returned with three rows for each model in the
#' `models` list and 11 columns. Rows provide summary statistics for a
#' given group of recruitments, where the rows are additive in their
#' inclusiveness, i.e., the data frames used to calculate rows two and three
#' include all data used in the calculations that led to the results
#' displayed in the first row. Columns include the following variables:
#' * `period`: Categories that specify which recruitments were included in the
#' subset used to calculate the summary
#' * `N_devs`: The number of recruitment estimates used in the summary
#' * `SD_of_devs`: The standard deviation (sd) of the recruitment deviations.
#' * `Var_of_devs`: The variance (var) of the recruitment deviations
#' * `mean_SE`: The mean of the estimated standard error (se) of the
#'    recruitment deviations, or more precisely, the mean of the `Parm_StDev`
#'    column of your data
#' * `mean_SEsquared`: The mean of the squared estimates of standard error of
#'    the recruitment deviations, i.e., `mean(Parm_StDev^2)`
#' * `sqrt_sum_of_components`: The square root of the sum of the deviation and
#'    the standard error
#' * `SD_of_devs_over_sigma_R`: The scaled version of `SD_of_devs`
#' * `sqrt_sum_over_sigma_R`: The scaled version of `sqrt_sum_of_components`
#' * `alternative_sigma_R`: The suggested value for a new input sigma_R, which
#'    is just a repeat of the sum of components column
extract_sigma_r <- function(models = NA,
                            model_names = NA,
                            sigma_r_in = NA) {

  stopifnot(!is.na(sigma_r_in))
  stopifnot(length(models) == length(model_names))

  map2(models, model_names, ~{
    mc <- as_tibble(.x$mcmc)
    mc_names <- names(mc)
    sig <- .x$sigma_R_in
    if(!"Value" %in% mc_names){
      ssb_yrs <- as.numeric(gsub("SSB_", "", grep("SSB_[0-9]+", mc_names, value = TRUE)))
      minyear <- min(ssb_yrs)
      mc <- mc %>%
        select(matches("InitAge|RecrDev|ForeRec")) %>%
        pivot_longer(everything(), names_to = "rows", values_to = "value") %>%
        group_by(rows) %>%
        summarize(Value = mean(value),
                  Parm_StDev = sd(value)) %>%
        mutate(type = sub("_[0-9]+", "", rows),
               number = as.numeric(sub("[a-zA-z_]+_", "", rows)),
               min = minyear - number,
               Yr = case_when(!grepl("InitAge", type) ~ number, TRUE ~ min)) %>%
        select(Yr, type, Value, Parm_StDev)
    }

    main <- mc %>%
      dplyr::filter(grepl("Main", type)) %>%
      mutate(period = "Main")
    early_main <- mc %>%
      dplyr::filter(grepl("Early|Main", type)) %>%
      mutate(period = "Early+Main")
    early_main_late <- mc %>%
      dplyr::filter(grepl("Early|Main|Late", type)) %>%
      mutate(period = "Early+Main+Late")

    lst <- list(main, early_main, early_main_late)

    df <- map(lst, ~{
      .x %>% summarize(period = first(period),
                       N_devs = n(),
                       SD_of_devs = sd(Value),
                       Var_of_devs = sd(Value) ^ 2,
                       mean_SE = mean(Parm_StDev),
                       mean_SEsquared = mean(Parm_StDev ^ 2))
    }) %>%
      map_df(~{.x}) %>%
      mutate(model = .y,
             sqrt_sum_of_components = sqrt(Var_of_devs + mean_SEsquared),
             SD_of_devs_over_sigma_R = SD_of_devs / sigma_r_in,
             sqrt_sum_over_sigma_R = sqrt_sum_of_components / sigma_r_in,
             alternative_sigma_R = sigma_r_in * sqrt_sum_over_sigma_R) %>%
      select(model, everything())

  }) %>%
    map_df(~{.x})
}
