#' Load the inital numbers-at-age as defined in Report_mce_xxxx.sso files
#'
#' @param reps A list of vectors representing one element per report file
#' @param verbose Logical. Show messages
#' @param progress_n Report every time this many list items are processed.
#'  Consider how many posteriors there are, this should be a fairly large
#'  proportion of that (around 1/8th) or there will be too much output and
#'  it will run really slow
#' @param ... Absorbs arguments meant for other functions
#'
#' @return A list of outputs
#' @export
load_extra_mcmc_init_nage <- function(reps,
                                      verbose = TRUE,
                                      ...){

  if(verbose){
    message("Extracting initial numbers-at-age ...")
  }

  x <- load_extra_mcmc_get_parameter_chunk(reps, ...)
  # Change output to a tibble, with a column for age and columns for the
  # `probs` values for the quantiles
  x |>
    map_dfr(~{
      .x |> transmute(age = Label,
                      value = as.numeric(Value)) |>
        mutate(age = as.numeric(gsub("^Early_InitAge_", "", age))) |>
        arrange(age)
    }) |>
    split(~age) |>
    map_dfr(~{
      q <- quantile(.x$value, probs)
      vec <- c(age = .x$age[1], q)
      as_tibble_row(vec)
    })
}
