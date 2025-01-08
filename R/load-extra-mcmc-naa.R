load_extra_mcmc_naa <- function(reps,
                                probs,
                                progress_n,
                                verbose = TRUE,
                                end_yr,
                                ...){

  # Use this as the output to match regular expressions for
  rep_example <- reps[[1]]

  if(verbose){
    message("Extracting Numbers-at-age...")
  }
  pat <- "^NUMBERS_AT_AGE"
  natage_header_ind <- grep(pat, rep_example)
  if(!length(natage_header_ind)){
    stop("Could not find a line in the report file matching the regular ",
         "expression `", pat, "`")
  }
  natage_header_ind <- natage_header_ind[1]
  natage_header_ind <- natage_header_ind + 1
  natage_header <- str_split(rep_example[natage_header_ind], " +")[[1]]
  natage_start_ind <- natage_header_ind[1] + 1
  natage_end_ind <- grep("^BIOMASS_AT_AGE", rep_example)
  if(!length(natage_end_ind)){
    stop("Could not find a line in the report file matching the regular ",
         "expression `", pat, "`")
  }
  if(length(natage_end_ind) > 1){
    stop("There is more than 1 matching line for the regular expression `",
         pat, "`")
  }
  natage_end_ind <- natage_end_ind - 2
  reps_natage <- map(reps, ~{.x[natage_start_ind:natage_end_ind]})
  natage <- extract_rep_table(reps_natage,
                              natage_header,
                              verbose = verbose,
                              ...) |>
    dplyr::filter(`Beg/Mid` == "B") |>
    select(-c("Area",
              "Bio_Pattern",
              "Iter",
              "Sex",
              "BirthSeas",
              "Settlement",
              "Platoon",
              "Morph",
              "Seas",
              "Beg/Mid",
              "Era",
              "Time")) |>
    map_df(as.numeric) |>
    dplyr::filter(Yr >= start_yr) |>
    mutate_at(.vars = vars(-Yr), ~{.x / 1e3})

  out <- list()
  out$natage_median <- natage |>
    group_by(Yr) |>
    summarize_all(median)

  out
}