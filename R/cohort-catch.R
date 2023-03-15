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
    stop("The model does not have extra mcmc information",
         call. = FALSE)
  }
  if(missing("cohort")){
    stop("No `cohort` supplied",
         call. = FALSE)
  }
  if(length(cohort) > 1){
    stop("Only one `cohort` can be calculated at a time",
         call. = FALSE)
  }

  catage <- model$extra_mcmc$catage_median
  w <- model$wtatage |>
    as_tibble()
  cohort_yrs <- cohort + ages
  waa <- w |>
    filter(Fleet == 1 & Yr %in% cohort_yrs)
  tmp_caa <- catage |>
    filter(Yr %in% cohort_yrs) |>
    filter(Yr %in% waa$Yr)
  caa <- tmp_caa |>
    select(all_of(as.character(ages))) |>
    as.matrix()
  waa <- waa |>
    select(all_of(as.character(ages))) |>
    as.matrix()
  cwaa <- caa * waa
  cohort_catch <- diag(cwaa)
  names(cohort_catch) <- cohort_yrs[seq_len(nrow(cwaa))]
  if(!is.na(trim_end_year)){
    cohort_catch <- cohort_catch[names(cohort_catch) < trim_end_year]
  }

  cohort_catch
}
