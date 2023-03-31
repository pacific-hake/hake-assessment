#' Load the biomass outputs from the extra MCMC output
#'
#' @param reps A list of vectors representing one element per report file
#' @param probs The quantile values to use on the MCMC posterior data
#' @param verbose Logical. Show messages
#' @param progress_n Report every time this many list items are processed.
#'  Consider how many posteriors there are, this should be a fairly large
#'  proportion of that (around 1/8th) or there will be too much output and
#'  it will run really slow
#' @param ... Absorbs arguments meant for other functions
#'
#' @return A list of outputs
#' @export
load_extra_mcmc_biomass <- function(reps,
                                    probs,
                                    verbose = TRUE,
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

  calc_quants_by_group <- function(d, col){
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

  list(total_biomass_quants =
    calc_quants_by_group(ts, "bio_all"),
  total_age2_plus_biomass_quants =
    calc_quants_by_group(ts, "bio_smry"))
}