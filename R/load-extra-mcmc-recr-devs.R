#' Load the inital numbers-at-age as defined in Report_mce_xxxx.sso files
#'
#' @param reps A list of vectors representing one element per report file
#' @param verbose Logical. Show messages
#' @param ... Absorbs arguments meant for other functions
#'
#' @return A list of outputs
#' @export
load_extra_mcmc_recr_devs <- function(reps,
                                      start_yr,
                                      end_yr,
                                      verbose = TRUE,
                                      ...){

  if(verbose){
    message("Extracting initial numbers-at-age ...")
  }

  x <- load_extra_mcmc_get_parameter_chunk(reps, ...)

  # Get the max age, so it can be subtracted from the start year to get the
  # initial recdev year
  age_label_pat <- "Early_InitAge_(\\d+)"
  age_labels <- grep(age_label_pat, x[[1]]$Label, value = T)
  ages <- gsub("Early_InitAge_(\\d+)", "\\1", age_labels) |>
    as.numeric()
  max_age <- max(ages)
  yrs <- (start_yr - max_age):end_yr
  j <- x |>
    map_dfr(~{
      .x |> transmute(yr = yrs,
                      value = as.numeric(Value)) |>
        arrange(yr)
    }) |>
    split(~yr) |>
    map_dfr(~{
      q <- quantile(.x$value, probs)
      vec <- c(yr = .x$yr[1], q)
      as_tibble_row(vec)
    })
}
