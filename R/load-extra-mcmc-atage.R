#' Load at-age data chunks from a list of report file output,
#' and calculate the medians at-age by year
#'
#' @param reps A list of vectors representing one element per report file
#' @param probs The quantile values to use on the MCMC posterior data
#' @param verbose Logical. Show messages
#' @param progress_n Report every time this many list items are processed.
#'  Consider how many posteriors there are, this should be a fairly large
#'  proportion of that (around 1/8th) or there will be too much output and
#'  it will run really slow
#' @param txt Text to write to the screen while processing
#' @param beg_pat A regular expression that marks the beginning of the chunk
#' of data
#' @param end_pat A regular expression that marks the end of the chunk of data
#' @param beg_off Offset from the beginning marker to the start of the data
#' @param end_off Offset from the beginning marker to the end of the data.
#' Should be a negative number
#' @param scale Value to divide the values by in the output table
#' @param start_yr Filter years earlier than this out of the results. If
#' `NULL`, no filtering will occur for the start year
#' @param end_yr Filter years greater than this out of the results. If
#' `NULL`, no filtering will occur for the end year
#' @param ... Absorbs arguments meant for other functions
#'
#' @return A list with one element being the data frame of values by year with
#' every posterior included (`atage`) and the other being a much smaller data
#' frame containing the median across posteriors by year for the values (`med`)
#' @export
load_extra_mcmc_atage <- function(reps,
                                  verbose = TRUE,
                                  txt = "output",
                                  scale = 1,
                                  start_yr = NULL,
                                  end_yr = NULL,
                                  ...){

  if(verbose){
    message(paste0("Extracting ", txt, "..."))
  }
  x <- load_extra_mcmc_get_chunk(reps, ...)

  # Create a data frame from the vector of strings in `x`
  atage <- extract_rep_table(reps_lst = x$lst,
                             header = x$header,
                             verbose = verbose,
                             ...)

  if("Beg/Mid" %in% names(atage)){
    atage <- atage |>
      filter(`Beg/Mid` == "B")
  }

  ages <- grep("[0-9]+", names(atage), value = TRUE)

  atage <- atage |>
    rename(yr = Yr, iter = Iter) |>
    select("yr", "iter", all_of(ages)) |>
    mutate_at(.vars = vars(-yr, -iter), ~{.x / scale})

  if(!is.null(start_yr)){
    atage <- atage |>
      filter(yr >= start_yr)
  }
  if(!is.null(end_yr)){
    atage <- atage |>
      filter(yr <= end_yr)
  }
  med <- atage |>
    select(-iter) |>
    group_by(yr) |>
    summarize_all(median)

  list(atage = atage, med = med)
}
