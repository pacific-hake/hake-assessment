#' Load the vulnerability outputs from the extra MCMC output
#'
#' @rdname load_extra_mcmc_biomass
#' @param end_yr End year in the model
#'
#' @export
load_extra_mcmc_vuln <- function(reps,
                                 probs,
                                 progress_n,
                                 verbose = TRUE,
                                 end_yr,
                                 ...){

  # Use this as the output to match regular expressions for
  rep_example <- reps[[1]]

  if(verbose){
    message("Extracting vulnerable biomass...")
  }
  next_yr <- end_yr + 1
  pat <- paste0(next_yr, "_1_sel\\*wt")
  selwt_ind <- grep(pat, rep_example)
  if(!length(selwt_ind)){
    stop("Could not find a line in the report file matching the regular ",
         "expression `", pat, "`",
         call. = FALSE)
  }
  if(length(selwt_ind) > 1){
    stop("There is more than 1 matching line for the regular expression `",
         pat, "`",
         call. = FALSE)
  }
  # Get the age selectivity header row
  # This code is the same as that found in `load_extra_selectivity()`
  pat <- "^COMBINED_ALK"
  sel_header_ind <- grep(pat, rep_example)

  if(!length(sel_header_ind)){
    stop("Could not find a line in the report file matching the regular ",
         "expression `", pat, "`",
         call. = FALSE)
  }
  if(length(sel_header_ind) > 1){
    stop("There is more than 1 matching line for the regular expression `",
         pat, "`",
         call. = FALSE)
  }
  sel_header_ind <- sel_header_ind + 1
  sel_header_line <- rep_example[sel_header_ind]
  sel_header <- str_split(sel_header_line, " +")[[1]]

  reps_selwt <- map(reps, ~{.x[selwt_ind]})
  selwt <- extract_rep_table(reps_selwt,
                             sel_header,
                             verbose = verbose,
                             ...)
  selwt |>
    select(-c("Factor",
              "Fleet",
              "Seas",
              "Sex",
              "Morph",
              "Label")) |>
    map_df(as.numeric) |>
    filter(Yr == next_yr) |>
    select(-c(Iter, Yr))
}