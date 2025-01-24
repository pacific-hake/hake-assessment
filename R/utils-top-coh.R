#' Create text describing the top `num_cohorts` cohorts by year and
#' percentage as a sentence
#'
#' @details
#' top_coh(base_model, 2020, 2022, 2) produces:
#' "The 2020 cohort was the largest (33\\%), followed by the 2016 cohort
#' (24\\%)"
#'
#' @param model The model as returned from [create_rds_file()]
#' @param yr the year to calculate the cohort catch percentages for
#' @param num_cohorts The top `num_cohorts` cohorts will be mentioned in
#' the sentence
#' @param decimals The number of decimal points to use
#' @param cap Logical. Capitalize the first word in the sentence?
#' @param use_catage Logical. If `TRUE`, use the
#' `model$extra_mcmc$catage_median` object which are the estimates. If
#' `FALSE`, use the `model$dat$agecomp` object which are the input data
#' @param fleet A integer value allowing the selection of a given fleet, where
#' `fleet = 1`, the default, selects the fishery data. This is only used for
#' the case where `use_catage` is `FALSE`. If `use_catage` is `TRUE`, `fleet`
#' will be ignored and fishery data are used
#' @param ret_cohort If not `NA`, filter out this cohort from the result and
#' return the proportion for it for year `yr`
#'
#' @return Text describing the top `num_cohorts` cohorts by year and
#' percentage as a sentence
#' @export
top_coh <- function(model = NULL,
                    yr = year(Sys.Date()),
                    num_cohorts = 3,
                    decimals = 0,
                    cap = TRUE,
                    use_catage = FALSE,
                    fleet = 1,
                    ret_cohort = NA){

  stopifnot(!is.null(model),
            !is.na(yr))
  stopifnot(length(fleet) == 1)

  if(num_cohorts < 1){
    num_cohorts = 1
  }
  if(use_catage){
    tmp <- model$extra_mcmc$catage_med
  }else{
    # Need to change name of object b/c it is named the same as a column and
    # was leading to all rows being selected
    fleet_select <- fleet
    tmp <- model$dat$agecomp |>
      select(matches("^a|year|fleet", ignore.case = FALSE)) |>
      select(-ageerr) |>
      dplyr::filter(fleet %in% fleet_select) |>
      select(-fleet) |>
      mutate_all(list(as.numeric))
    names(tmp) <- gsub("^a", "", names(tmp))
  }
  x <- tmp %>%
    mutate(row_sum = rowSums(select(., -year))) %>%
    mutate_at(vars(-year, -row_sum), ~ . / row_sum) |>
    select(-row_sum) |>
    pivot_longer(-year) |>
    group_by(year) |>
    arrange(desc(value)) |>
    ungroup() |>
    dplyr::filter(year == yr)

  txt <- paste0(ifelse(cap, "The ", "the "),
                yr - as.numeric(x$name[1]),
                " cohort was the largest (",
                f(x$value[1] * 100, decimals),
                "%)")
  if(num_cohorts > 1){
    for(i in 2:num_cohorts){
      txt <- paste0(txt,
                    ifelse(i == 2, ", followed by the ", ", and then the "),
                    yr - as.numeric(x$name[i]),
                    " cohort (",
                    f(x$value[i] * 100, decimals),
                    "%)")
    }
  }

  if(!is.na(ret_cohort)){
    cohort_val <- x |>
      dplyr::filter(x$name == as.character(yr - ret_cohort)) |>
      pull(value)
    return(f(cohort_val * 100, decimals))
  }

  txt
}
