#' Load the likelihoods as defined in Report_mce_xxxx.sso files
#'
#' @param reps A list of vectors representing one element per report file
#' @param verbose Logical. Show messages
#' @param ... Absorbs arguments meant for other functions
#'
#' @return A list of outputs
#' @export
load_extra_mcmc_likelihoods <- function(reps,
                                        verbose = TRUE,
                                        ...){

  if(verbose){
    message("Extracting likelihoods ...")
  }
  p_names <- map_chr(probs, ~paste0(.x * 100, "%"))
  p_funs <- map(probs,
                ~partial(quantile, probs = .x, na.rm = TRUE)) |>
    set_names(nm = p_names)

  x <- load_extra_mcmc_get_parameter_chunk(reps, ...) |>
    map_dfr(~{
      .x <- .x[-1, ]
      .x <- .x[-nrow(.x), ]
      .x
    }) |>
    select(-Value) |>
    rename(type = Num,
           value = Label) |>
    mutate(value = as.numeric(value)) |>
    split(~type) |>
    map_dfr(~{
      j <- tibble(value = .x$value) |>
        summarize(across(value, p_funs)) |>
        mutate(type = .x$type[1]) |>
        select(type, everything())

      names(j) <- gsub("value_", "", names(j))
      j
    })
}
