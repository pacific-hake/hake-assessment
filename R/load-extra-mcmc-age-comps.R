#' Load the age composition estimates from the extra MCMC output
#'
#' @param compreps A list of vectors representing one element per
#' compreport file
#' @param verbose Logical. If `TRUE`, show messages
#' @param start_yr Filter years earlier than this out of the results. If
#' `NULL`, no filtering will occur for the start year
#' @param end_yr Filter years greater than this out of the results. If
#' `NULL`, no filtering will occur for the end year
#' @param progress_n Report every time this many list items are processed.
#'  Consider how many posteriors there are, this should be a fairly large
#'  proportion of that (around 1/8th) or there will be too much output and
#'  it will run really slow
#' @param ret_quants Logical. If `TRUE`, return the quantiles of the data.
#' If `FALSE`, return all posteriors (the `iter` column will be present in
#' this case)
#' @param ... Arguments passed to [load_extra_mcmc_get_chunk()]
#'
#' @return A data frame with columns fleet, yr, iter (posterior number) and
#' a column for each age in the output. If `ret_quants` is `TRUE`, the data
#' frame will be in the long format and have columns fleet, yr, age, and
#' one column for each of the quantiles found in the [probs] variable
#' (normally 2.5%, 50%, and 97.5%)
#'
#' @export
load_extra_mcmc_age_comps <- function(compreps,
                                      verbose = TRUE,
                                      start_yr = NULL,
                                      end_yr = NULL,
                                      progress_n,
                                      ret_quants = TRUE,
                                      ...){


  if(verbose){
    message("Extracting age comps...")
  }

  x <- load_extra_mcmc_get_chunk(compreps, ...)

  comps_df <- x$lst |>
    imap_dfr(\(posterior_vec, ind){
      posterior_vec |>
        map_dfr(~{
          vec <- strsplit(.x, "\\s+")[[1]]
          vec <- c(ind, vec)
          hdr <- x$header[seq_along(vec)[-length(vec)]]
          names(vec) <- c("iter", hdr)
          vec |>
            as_tibble_row()
        })
    }) |>
    transmute(iter,
              yr = Yr,
              fleet = Fleet,
              age = Bin,
              prop = Exp) |>
    pivot_wider(names_from = "age", values_from = "prop") |>
    dplyr::filter(fleet > 0) |>
    mutate(across(everything(), ~{.x = as.numeric(.x)}))

  # These may be introduced by short lines in the output file which delineate
  # groups but serve no purpose otherwise
  if("NA" %in% names(comps_df)){
    comps_df <- comps_df |>
      select(-`NA`)
  }
  if("-1" %in% names(comps_df)){
    comps_df <- comps_df |>
      select(-`-1`)
  }

  # Survey will have NAs for age 1's, so replace them with zeros
  comps_df[is.na(comps_df)] <- 0

  if(!is.null(start_yr)){
    comps_df <- comps_df |>
      dplyr::filter(yr >= start_yr)
  }

  if(!is.null(end_yr)){
    comps_df <- comps_df |>
      dplyr::filter(yr <= end_yr)
  }

  if(ret_quants){
    # Calculate quantiles by year and fleet

    split_comps_df <- comps_df |>
      split(~yr + fleet)

    comps_df <- map_dfr(split_comps_df, \(fleet_yr_item){

      if(!nrow(fleet_yr_item)){
        return(NULL)
      }
      yr <- fleet_yr_item |> slice(1) |> pull(yr)
      fleet <- fleet_yr_item |> slice(1) |> pull(fleet)
      age <- names(fleet_yr_item)
      age <- age[!age %in% c("iter", "yr", "fleet")] |>
        as.numeric()
      fleet_yr_item |>
        select(-c(iter, yr, fleet)) |>
        imap_dfr(~{
          quantile(.x, probs)}
          ) |>
        mutate(yr = yr,
               fleet = fleet,
               age = age) |>
        select(fleet, yr, age, everything())
    })
  }

  comps_df
}