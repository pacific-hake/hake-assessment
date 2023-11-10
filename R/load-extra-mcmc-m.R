#' Load the natural mortality outputs from the extra MCMC output
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
load_extra_mcmc_m <- function(reps,
                              probs,
                              verbose = TRUE,
                              progress_n,
                              ...){

  if(verbose){
    message("Extracting natural mortality...")
  }

  x <- load_extra_mcmc_get_chunk(reps, ...)

  d <- extract_rep_table(reps_lst = x$lst,
                          header = x$header,
                          verbose = verbose,
                          ...)
  col <- "NatM_uniform_Fem_GP_1"
  if(!col %in% names(d)){
    col <- "NatM_p_1_Fem_GP_1"
    if(!col %in% names(d)){
      stop("Neither `NatM_uniform_Fem_GP_1` nor `NatM_p_1_Fem_GP_1` were ",
           "found in the output data for Natural mortality")
    }
  }

  d <- d |>
    select(Iter, Yr, {{col}})

  names(d) <- tolower(names(d))

  calc_quants_by_group <- function(d, col){
    p_names <- map_chr(probs, ~paste0(.x * 100, "%"))
    p_funs <- map(probs,
                  ~partial(quantile, probs = .x, na.rm = TRUE)) |>
      set_names(nm = p_names)

    d_quants <- d |>
      select(yr, {{col}}) |>
      group_by(yr) |>
      summarize_at(vars({{col}}), p_funs)

    d_mean <- d |>
      select(yr, {{col}}) %>%
      group_by(yr) |>
      summarize_at(vars({{col}}), ~mean(.)) |>
      rename(mean = {{col}})

    bind_cols(d_quants,
              d_mean |> select(mean))
  }

  df <- calc_quants_by_group(d, tolower(col))
  df_distinct <- distinct(select(df, -yr))
  if(nrow(df_distinct) == 1){
    return(df_distinct)
  }

  df
}