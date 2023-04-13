#' Create a table of cohort biomass and natural mortality as they pass
#' through their life from year to year
#'
#' @param model A model, created by [create_rds_file()]
#' @param cohorts A vector of cohorts (years of birth) to use
#' @param csv_dir Directory for CSV output
#' @param start_yr Start year in table
#' @param end_yr End year in table
#' @param digits Number of decimal points to show in values in the table
#' @param font_size The table data and header font size in points
#' @param header_font_size The font size for the headers only. If `NULL`,
#' the headers will have the same font size as the table cell data
#' @param vert_spacing The vertical spacing between newlines for this font.
#' If `NULL` this will be calculated as `header_font_size * header_vert_scale`
#' @param header_vert_scale Scale factor to create the vertical spacing value.
#' See `header_vert_spacing`
#' @param ... Arguments passed to [knitr::kable()]
#'
#' @return An [knitr::kable()] object
#' @export
table_cohort <- function(model,
                         cohorts,
                         digits = 0,
                         csv_dir = here::here("doc", "out-csv"),
                         font_size = 10,
                         header_font_size = 10,
                         header_vert_spacing = 12,
                         header_vert_scale = 1.2,
                         ...){

  caa <- model$extra_mcmc$catage_med
  # All data have the same start and end year, the exact same dimensions.
  # They were built that way in extra-mcmc.R
  min_yr <- min(caa$yr)
  max_yr <- max(caa$yr)

  caa <- model$extra_mcmc$catage_med
  naa <- model$extra_mcmc$natage_med |>
    mutate_at(vars(-yr), ~{.x * 1e3}) |>
    filter(yr <= max_yr)
  naa_next <- model$extra_mcmc$natage_med |>
    mutate_at(vars(-yr), ~{.x * 1e3}) |>
    filter(yr %in% (min_yr + 1):(max_yr + 1)) |>
    mutate(yr = yr - 1)
  caa_b <- model$extra_mcmc$cbatage_med |>
    filter(yr <= max_yr)
  baa <- model$extra_mcmc$batage_med |>
    filter(yr <= max_yr)
  waa <- model$wtatage |>
    as_tibble() |>
    filter(Fleet == 1) |>
    filter(Yr <= max_yr)
  waa_ages <- grep("^[0-9]+", names(waa), value = TRUE)
  waa <- waa |>
    select(Yr, all_of(waa_ages)) |>
    rename(yr = Yr)

  # Get the diagonals of the cohort data from the data frame
  #
  # @param d Data frame with the -at-age data
  # @param cohorts A vector of cohorts (years) to extract
  # @return A list of length of cohort vector with a vector of the cohort data for each one
  get_coh <- function(d, cohorts){
    yrs <- d |>
      pull(yr)
    d_noyr <- d |>
      select(-yr)
    coh_inds <- as.character(which(yrs %in% cohorts) - 1)
    delta <- row(d_noyr) - col(d_noyr)
    coh_lst <- split(as.matrix(d_noyr), delta)
    map(coh_inds, ~{get(.x, coh_lst)})
  }

  coh_baa <- get_coh(baa, cohorts)
  coh_caa_b <- get_coh(caa_b, cohorts) |>
    map(~{
      tmp <- .x / 1e3
      tmp[-length(.x)]
      })

  coh_naa <- get_coh(naa, cohorts)

  coh_naa_next <- get_coh(naa_next, cohorts - 1) |>
    map2(seq_along(cohorts), ~{
      tmp <- .x[-1]
      if(.y == 1) tmp else tmp[-length(tmp)]
    })

  coh_waa <- get_coh(waa, cohorts) |>
    map(~{
      .x[-length(.x)]
    })

  if(length(coh_naa_next[[1]]) != length(coh_waa[[1]])){
    coh_naa_next[[1]] <- head(coh_naa_next[[1]], -1)
  }

  coh_survive_b <- map2(coh_naa_next, coh_waa, ~{
    .x * .y
  }) |>
    map(~{.x / 1e3})

  coh_m <- map(seq_along(cohorts), \(.x, ba, ca, surv){
    ba[[.x]] <- ba[[.x]][-length(ba[[.x]])]
    ba[[.x]] - surv[[.x]] - ca[[.x]]
  }, ba = coh_baa, ca = coh_caa_b, surv = coh_survive_b)

  # Pad all vectors in the list with `NA`s so that they are all `num` long
  # @param lst A list of vectors
  # @param num The length to make all vectors
  pad_vects <- \(lst, num){
    map(lst, ~{.x[1:num]})
  }

  # Make a list of length 4, one list for each element which contains one of the four value types
  coh_lst <- list(coh_baa, coh_caa_b, coh_m, coh_survive_b)
  lst <- map2(coh_lst, c("baa", "caa", "m", "surv"), ~{
    pad_vects(.x, length(coh_baa[[1]])) |>
      `names<-`(paste0(.y, "_", cohorts))
  }) |>
    `names<-`(c("baa", "caa", "m", "surv"))

  # Make a list of length of number of cohorts, each with a list of 4 value types. This is getting the
  # list into the correct structure for the table
  d <- map(seq_along(cohorts), ~{
    map_df(lst, ~{
      .x[[.y]]
    }, .y = .x)
  }) |>
    `names<-`(cohorts)

  # Make unique names for the cohort baa, caa, m, and surv columns and bind the data frames into one
  d <- map2(d, cohorts, ~{
    names(.x) <- paste0(names(.x), "_", .y)
    .x
  }) |>
    bind_cols()

  # Add the Age column
  age_df <- enframe(0:(nrow(d) - 1), name = NULL, value = "Age")
  d <- bind_cols(age_df, d)

  # Table constructed, write to a CSV
  csv_headers <- c("Age", map(seq_along(cohorts), ~{
    c(paste(cohorts[.x], "Start Biomass"),
      paste(cohorts[.x], "Catch Weight"),
      paste(cohorts[.x], "M Weight"),
      paste(cohorts[.x], "Surviving Biomass"))
  }) |>
    unlist())
  csv_out <- d |>
    mutate_all(~{as.character(.x)})
  colnames(csv_out) <- csv_headers
  csv_out[is.na(csv_out)] <- ""

  if(!dir.exists(csv_dir)){
    dir.create(csv_dir)
  }
  write_csv(csv_out,
            file.path(csv_dir, "cohort-effects.csv"))

  # Change the number of decimal points displayed and remove NAs
  d <- d |>
    mutate_at(vars(-Age), ~{f(.x, digits)}) |>
    map_df(~{gsub("NA", "", .x)})

  nm_vec <- c("Start\nBiomass\n(kt)",
              "Catch\nWeight\n(kt)",
              "M\n(kt)",
              "Surviving\nBiomass\n(kt)")
  col_names <- map(cohorts, ~{
    paste0(.x, "\ncohort\n", nm_vec)
  }) |>
    unlist()
  col_names <- c("Age", col_names)

  # Insert custom header fontsize before linebreaker
  if(is.null(header_font_size)){
    header_font_size <- font_size
  }
  hdr_font_str <- create_fontsize_str(header_font_size,
                                      header_vert_spacing,
                                      header_vert_scale)

  col_names <- gsub("\\n", paste0("\n", hdr_font_str$quad), col_names)
  col_names <- paste0(hdr_font_str$dbl, col_names)
  # Add \\makecell{} latex command to headers with newlines
  col_names <- linebreaker(col_names, align = "c")

  kbl(d,
      format = "latex",
      booktabs = TRUE,
      align = "c",
      linesep = "",
      col.names = col_names,
      escape = FALSE,
      ...) |>
    row_spec(0, bold = TRUE) |>
    kable_styling(font_size = font_size,
                  latex_options = c("repeat_header"))

}
