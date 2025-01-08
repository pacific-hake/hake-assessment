#' Load the biomass outputs from the extra MCMC output
#'
#' @param reps A list of vectors representing one element per report file
#' @param verbose Logical. Show messages
#' @param start_yr Filter years earlier than this out of the results. If
#' `NULL`, no filtering will occur for the start year
#' @param end_yr Filter years greater than this out of the results. If
#' `NULL`, no filtering will occur for the end year
#' @param progress_n Report every time this many list items are processed.
#'  Consider how many posteriors there are, this should be a fairly large
#'  proportion of that (around 1/8th) or there will be too much output and
#'  it will run really slow
#' @param ... Absorbs arguments meant for other functions
#'
#' @return A list of outputs
#' @export
load_extra_mcmc_biomass <- function(reps,
                                    verbose = TRUE,
                                    start_yr = NULL,
                                    end_yr = NULL,
                                    progress_n,
                                    ...){

  if(verbose){
    message("Extracting biomass...")
  }

  x <- load_extra_mcmc_get_chunk(reps, ...)

  ts <- extract_rep_table(reps_lst = x$lst,
                          header = x$header,
                          verbose = verbose,
                          ...) |>
    select(Iter, Yr, Bio_all, Bio_smry)

  names(ts) <- tolower(names(ts))

  calc_quants_by_group <- \(d, col){
    p_names <- map_chr(probs, ~paste0(.x * 100, "%"))
    p_funs <- map(probs,
                  ~partial(quantile, probs = .x, na.rm = TRUE)) |>
      set_names(nm = p_names)

    col_sym <- sym(col)
    ts_quants <- ts |>
      select(yr, !!col_sym) |>
      group_by(yr) |>
      summarize_at(vars(!!col_sym), p_funs)

    ts_mean <- ts |>
      select(yr, !!col_sym) |>
      group_by(yr) |>
      summarize_at(vars(!!col_sym), ~mean(.)) |>
      rename(mean = !!col_sym)

    bind_cols(ts_quants,
              ts_mean |> select(mean))
  }

  if(!is.null(start_yr)){
    ts <- ts |>
      dplyr::filter(yr >= start_yr)
  }
  if(!is.null(end_yr)){
    ts <- ts |>
      dplyr::filter(yr <= end_yr)
  }
  total_biomass_quants <- calc_quants_by_group(ts, "bio_all")
  total_age2_plus_biomass_quants <- calc_quants_by_group(ts, "bio_smry")

  out <- list(total_biomass_quants = total_biomass_quants,
              total_age2_plus_biomass_quants = total_age2_plus_biomass_quants)

  out
}