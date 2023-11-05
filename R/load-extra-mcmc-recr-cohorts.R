#' Load the recruitment outputs from the extra MCMC output for certain cohorts
#'
#' @param reps A list of vectors representing one element per report file
#' @param cohorts A vector of years of birth for cohorts to extract
#' recruitment quantiles for
#' @param verbose Logical. Show messages
#' @param progress_n Report every time this many list items are processed.
#'  Consider how many posteriors there are, this should be a fairly large
#'  proportion of that (around 1/8th) or there will be too much output and
#'  it will run really slow
#' @param ... Absorbs arguments meant for other functions
#'
#' @return A list of outputs
#' @export
load_extra_mcmc_recr_cohorts <- function(reps,
                                         cohorts = NULL,
                                         verbose = TRUE,
                                         ...){

  if(verbose){
    message("Extracting recruitment cohorts...")
  }

  stopifnot(!is.null(cohorts))
  stopifnot(is.numeric(cohorts))

  cohorts_str <- paste0("Recr_", cohorts)

  x <- load_extra_mcmc_get_chunk(reps, ...)

  # Extract all posteriors for each cohort into a data frame
  d <- map(cohorts_str, \(cohort){
    map_df(x[[2]], \(post){
      str <- grep(cohort, post, value = TRUE)
      strsplit(str, " +")[[1]][2] |>
        as.numeric() |>
        enframe(name = NULL)
    })
   }) |>
    # The name repair stuff just silences the New names.... messages
    # caused during the binding of columns
    bind_cols_quiet() |>
    set_names(cohorts)

  calc_quants_by_group <- \(d, col){
    p_names <- map_chr(probs, ~paste0(.x * 100, "%"))
    p_funs <- map(probs,
                  ~partial(quantile, probs = .x, na.rm = TRUE)) |>
      set_names(nm = p_names)

    col_sym <- sym(col)
    d_quants <- d |>
      select(!!col_sym) |>
      summarize_at(vars(!!col_sym), p_funs)

    d_mean <- d |>
      select(!!col_sym) %>%
      summarize_at(vars(!!col_sym), ~mean(.)) |>
      rename(mean = !!col_sym)

    bind_cols(d_quants,
              d_mean |> select(mean))
  }

  map(as.character(cohorts), \(col_nm){
    calc_quants_by_group(d, col_nm)
  }) |>
    bind_rows() |>
    mutate(yr = cohorts) |>
    select(yr, everything())
}