#' Create a table of cohort biomass and natural mortality as they pass
#' through their life from year to year
#'
#' @param model A model, created by [create_rds_file()]
#' @param cohorts A vector of cohorts (years of birth) to use
#' @param cohort_italics Logical. If `TRUE`, make the cohort header lines
#' italicized
#' @param cohort_bold Logical. If `TRUE`, make the cohort header lines
#' boldface
#' @param cohort_underline Logical. If `TRUE`, make the cohort header lines
#' underlined
#' @param cohort_line_above Logical. If `TRUE`, place a horizontal line above
#' cohort header lines
#' @param cohort_line_below Logical. If `TRUE`, place a horizontal line below
#' cohort header lines
#' @param reverse_cohorts Logical. If `TRUE`, show the cohorts in the table in
#' descending order, with the most recent cohort at the top of the table
#' @param csv_dir Directory for CSV output
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
                         cohort_italics = TRUE,
                         cohort_bold = TRUE,
                         cohort_underline = TRUE,
                         cohort_line_above = TRUE,
                         cohort_line_below = TRUE,
                         reverse_cohorts = FALSE,
                         digits = 1,
                         csv_dir = here::here("doc", out_csv_path),
                         font_size = 10,
                         header_font_size = 10,
                         header_vert_spacing = 12,
                         header_vert_scale = 1.2,
                         ...){

  stopifnot(is.numeric(cohorts))
  cohorts <- sort(cohorts)

  caa <- model$extra_mcmc$catage_med
  # All data have the same start and end year, the exact same dimensions.
  # They were built that way in extra-mcmc.R
  min_yr <- min(caa$yr)
  max_yr <- max(caa$yr)

  caa <- model$extra_mcmc$catage_med
  naa <- model$extra_mcmc$natage_med |>
    mutate_at(vars(-yr), ~{.x * 1e3}) |>
    dplyr::filter(yr <= max_yr)
  naa_next <- model$extra_mcmc$natage_med |>
    mutate_at(vars(-yr), ~{.x * 1e3}) |>
    dplyr::filter(yr %in% (min_yr + 1):(max_yr + 1)) |>
    mutate(yr = yr - 1)
  caa_b <- model$extra_mcmc$cbatage_med |>
    dplyr::filter(yr <= max_yr)
  baa <- model$extra_mcmc$batage_med |>
    dplyr::filter(yr <= max_yr)
  waa <- model$wtatage |>
    as_tibble() |>
    dplyr::filter(fleet == 1) |>
    dplyr::filter(year <= max_yr)
  waa_ages <- grep("^[0-9]+", names(waa), value = TRUE)
  waa <- waa |>
    select(year, all_of(waa_ages)) |>
    rename(yr = year)

  # Get the diagonals of the cohort data from the data frame
  #
  # @param d Data frame with the -at-age data
  # @param cohorts A vector of cohorts (years) to extract
  # @return A list of length of cohort vector with a vector of the cohort
  # data for each one
  get_coh <- \(d, cohorts){
    yrs <- d |>
      pull(yr)
    d_noyr <- d |>
      select(-yr)
    coh_inds <- as.character(which(yrs %in% cohorts) - 1)
    # The magic is here
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

  # Make a list with one sublist for each element which contains one of the
  # value types (columns in the table)
  value_lst <- list(coh_baa, coh_caa_b, coh_m, coh_survive_b)
  value_nms <- c("baa", "caa", "m", "surv")
  value_lst <- map2(value_lst, value_nms, \(value, nm){
    pad_vects(value, length(coh_baa[[1]])) |>
      set_names(paste0(nm, "_", cohorts))
  }) |>
    set_names(value_nms)

  # Make a list of length of number of cohorts, each with a list value types.
  # Add the age column. This is getting the list into the correct structure
  # for the table
  d <- map(seq_along(cohorts), \(cohort_ind){
    tmp <- map_df(value_lst, \(value){
      value[[cohort_ind]]
    }) %>%
      filter_all(any_vars(!is.na(.))) %>%
      mutate(age = seq(0, nrow(.) - 1)) |>
      select(age, everything()) |>
      mutate_at(vars(-age), ~{f(.x, digits)}) |>
      map_df(~{gsub(" *NA *", "", .x)})
  }) |>
    set_names(cohorts)

  if(reverse_cohorts){
    d <- rev(d)
  }

  # Need this for later, to add bold/italics and horizontal lines to the
  # table to section off cohorts
  num_rows_per_section <- d |> map_dbl(~{nrow(.x)})

  # Table constructed, write to a CSV
  # Need special data frame for CSV, csv_d
  d <- imap(d, \(tbl, nm){
    vec2df(c(paste0(nm, " cohort"), rep("", ncol(tbl) - 1)),
           nms = names(tbl)) |>
      bind_rows(tbl)
  }) |>
    bind_rows()

  csv_d <- d
  names(csv_d) <- c("Age",
                    "Start Biomass",
                    "Catch Weight",
                    "M Weight",
                    "Surviving Biomass")

  if(!dir.exists(csv_dir)){
    dir.create(csv_dir)
  }
  write_csv(csv_d,
            file.path(csv_dir, "cohort-effects.csv"))

  # Back to table construction for the document
  col_names <- c("Age",
                 "Start\nBiomass\n(kt)",
                 "Catch\nWeight\n(kt)",
                 "Natural\nMortality\n(kt)",
                 "Surviving\nBiomass\n(kt)")

  # Find the actual row indices for the cohort header lines
  cohort_row_inds <-
    cumsum(num_rows_per_section) +
    seq_along(num_rows_per_section) + 1
  cohort_row_inds <- c(1, cohort_row_inds[-length(cohort_row_inds)])

  if(cohort_underline){
    # Add bold and italics to the cohort headers
    d[cohort_row_inds, 1] <- map(d[cohort_row_inds, 1], ~{
      latex_under(.x)
    })
  }
  if(cohort_italics){
    # Add bold and italics to the cohort headers
    d[cohort_row_inds, 1] <- map(d[cohort_row_inds, 1], ~{
      latex_italics(.x)
    })
  }
  if(cohort_bold){
    # Add bold and italics to the cohort headers
    d[cohort_row_inds, 1] <- map(d[cohort_row_inds, 1], ~{
      latex_bold(.x)
    })
  }

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

  k <- kbl(d,
           format = "latex",
           booktabs = TRUE,
           align = "r",
           linesep = "",
           col.names = col_names,
           escape = FALSE,
           ...) |>
    row_spec(0, bold = TRUE)

  if(cohort_line_above){
    cohort_row_above_inds <- cohort_row_inds[cohort_row_inds != 1]
    cohort_row_above_inds <- cohort_row_above_inds - 1
    k <- k |>
      row_spec(cohort_row_above_inds,
               extra_latex_after = paste0("\\cline{",
                                          1,
                                          "-",
                                          length(col_names),
                                          "}"))
  }
  if(cohort_line_below){
    k <- k |>
      row_spec(cohort_row_inds,
               extra_latex_after = paste0("\\cline{",
                                          1,
                                          "-",
                                          length(col_names),
                                          "}"))
  }

  k |>
    kable_styling(font_size = font_size,
                  latex_options = c("repeat_header"))
}
