#' Calculate the total catch taken for a given cohort
#'
#' @param model The model to extract from
#' @param cohort The year the cohort was born
#' @param ages The ages to include in the summation calculation
#' @param trim_end_year Remove all years after this including this year
#'
#' @return A vector of total catch at age for a given cohort
#' @export
cohort_catch <- function(model,
                         cohort,
                         ages = 0:20,
                         trim_end_year = NA) {

  if(!model$extra_mcmc_exists){
    stop("The model does not have extra mcmc information")
  }
  if(missing("cohort")){
    stop("No `cohort` supplied")
  }
  if(length(cohort) > 1){
    stop("Only one `cohort` can be calculated at a time")
  }

  catage <- model$extra_mcmc$catage_med
  cohort_yrs <- cohort + ages
  tmp_caa <- catage |>
    dplyr::filter(yr %in% cohort_yrs) |>
    select(all_of(as.character(ages))) |>
    as.matrix()
  cohort_catch <- diag(tmp_caa)
  names(cohort_catch) <- cohort_yrs[seq_len(nrow(tmp_caa))]
  if(!is.na(trim_end_year)){
    cohort_catch <- cohort_catch[names(cohort_catch) < trim_end_year]
  }

  cohort_catch
}
