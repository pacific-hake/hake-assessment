#' Returns a [kableExtra::kbl()] containing a comparison of parameter
#' estimates for 1 model compared to hard coded values for the base
#' model in 2023. This function is meant to be destroyed after the
#' 2024 assessment season. It is used for table 26 only because we
#' could not locate the output files to reproduce the table.
#'
#' @param model A list of models which contain the MCMC output
#' @param model_nms A vector of names of the same length as the number of
#' models in the models list
#' @param show_loglike Logical. If `TRUE` show the negative log likelihood
#' values in the table
#' @param section_italics Logical. If `TRUE`, make the section header lines
#' italicized
#' @param section_bold Logical. If `TRUE`, make the section header lines
#' boldface
#' @param section_underline Logical. If `TRUE`, make the section header lines
#' underlined
#' @param section_line_above Logical. If `TRUE`, place a horizontal line above
#' section header lines
#' @param section_line_below Logical. If `TRUE`, place a horizontal line below
#' section header lines
#' @param digits Number of decimal points for the estimates
#' @param right_cols_cm The number of centimeters wide to make all of the
#' rightmost columns (all the value columns)
#' @param font_size The table data and header font size in points
#' @param header_font_size The font size for the headers only. If `NULL`,
#' the headers will have the same font size as the table cell data
#' @param vert_spacing The vertical spacing between newlines for this font.
#' If `NULL` this will be calculated as `header_font_size * header_vert_scale`
#' @param header_vert_scale Scale factor to create the vertical spacing value.
#' See `header_vert_spacing`
#' @param longtable Logical. Passed to [kableExtra::kbl()]
#' @param caption The table caption, or `NULL` for none. Passed to
#' [kableExtra::kbl()]
#' @param ... Arguments passed to [knitr::kable()]
#'
#' @return An [knitr::kable()] object
#' @export
table_param_est_last_year_hardcoded <- function(
    d = NULL,
    model = NULL,
    model_nms = NULL,
    show_loglike = FALSE,
    section_row_inds = c(1, 11, 20, 21 + length(large_cohorts)),
    section_row_headers = c("Parameters",
                            "Derived Quantities",
                            paste0("Reference Points based on ",
                                   fspr_40_bold_for_latex_table),
                            "Negative log likelihoods"),
    section_italics = TRUE,
    section_bold = TRUE,
    section_underline = TRUE,
    section_line_above = FALSE,
    section_line_below = TRUE,
    digits = 3,
    right_cols_cm = 1.8,
    font_size = 10,
    header_font_size = 10,
    header_vert_spacing = 12,
    header_vert_scale = 1.2,
    longtable = FALSE,
    caption = NULL,
    ...){

  if(!show_loglike){
    section_row_headers <- section_row_headers[-length(section_row_headers)]
    section_row_inds <- section_row_inds[-length(section_row_inds)]
  }

  if(longtable && missing(caption)){
    caption <- paste0(
      "\\textcolor{red}{Run post\\_process\\_beamer('filename.tex') ",
      "and then lualatex filename.tex to remove this caption}")
  }

  if(is.null(d)){
    d <- get_param_est_comparison_df(model,
                                     model_nms,
                                     inc_loglike = show_loglike,
                                     ...)
  }

  # Hard-code 2023 values
  d[1, 3] <- "0.233"
  d[2, 3] <- "2,547"
  d[3, 3] <- "0.808"
  d[4, 3] <- "0.286"
  d[5, 3] <- "0.833"
  d[6, 3] <- "0.375"
  d[7, 3] <- "0.398"
  d[8, 3] <- "-0.629"
  d[9, 3] <- "2.595"
  d[10, 3] <- "9,165"
  d[11, 3] <- "6,374"
  d[12, 3] <- "11,409"
  d[13, 3] <- "1,815"
  d[14, 3] <- "34.8\\%"
  d[15, 3] <- "104\\%"
  d[16, 3] <- "--"
  d[17, 3] <- "--"
  d[18, 3] <- "642"
  d[19, 3] <- "40.0\\%"
  d[20, 3] <- "18.6\\%"
  d[21, 3] <- "309"

  # Insert header rows at the row indices where the section headers are
  walk2(section_row_inds,
        section_row_headers, \(ind, hdr){
    row_vec <- c(hdr, rep("", ncol(d) - 1))
    d <<- insert_row(d, row_vec, ind)
  })

  if(section_underline){
    # Make section headers bold
    d[section_row_inds, "parameter"] <-
      map(d[section_row_inds, "parameter"],
          ~{latex_under(.x)})
  }
  if(section_italics){
    # Make section headers italics
    d[section_row_inds, "parameter"] <-
      map(d[section_row_inds, "parameter"],
          ~{latex_italics(.x)})
  }
  if(section_bold){
    # Make section headers bold
    d[section_row_inds, "parameter"] <-
      map(d[section_row_inds, "parameter"],
          ~{latex_bold(.x)})
  }

  # Replace NA and "NA" with "--"
  d <- map_df(d, ~{
    gsub(" *NA *", "--", .x)
  })
  d[is.na(d)] <- "--"

  if(is.null(model_nms)){
    # Remove leading spaces from column names
    names(d) <- gsub("^\\s+", "", names(d))
    col_names <- c("Parameter, Quantity, or Reference point",
                   gsub(" +", "\n", names(d)[-1]))
  }else{
    col_names <- c("Parameter, Quantity, or Reference point",
                   gsub(" +", "\n", model_nms))
  }

  # Introduce newline at a slash separator
  col_names <- gsub("\\/", "\\\\/\n", col_names)

  # Insert custom header font size before linebreaker
  if(is.null(header_font_size)){
    header_font_size <- font_size
  }
  hdr_font_str <- create_fontsize_str(header_font_size,
                                      header_vert_spacing,
                                      header_vert_scale)

  col_names <- gsub("\\n",
                    paste0("\n", hdr_font_str$quad),
                    col_names)
  col_names <- paste0(hdr_font_str$dbl, col_names)
  # Add \\makecell{} latex command to headers with newlines to create
  # multi-line header cells
  col_names <- linebreaker(col_names, align = "c")

  k <- kbl(d,
           format = "latex",
           booktabs = TRUE,
           align = c("l",
                     rep(paste0("R{",
                                right_cols_cm,
                                "cm}"),
                         ncol(d) - 1)),
           linesep = "",
           col.names = col_names,
           escape = FALSE,
           longtable = longtable,
           caption = caption,
           ...) |>
    row_spec(0, bold = TRUE)

  if(section_line_above){
    # Do not show a line above if a section starts on the first line
    # as it creates a double line at the top of the table
    sec_inds_above <- section_row_inds[section_row_inds != 1]
    k <- k |>
      row_spec(sec_inds_above - 1,
               extra_latex_after = paste0("\\cline{",
                                          1,
                                          "-",
                                          length(col_names),
                                          "}"))
  }
  if(section_line_below){
    k <- k |>
      row_spec(section_row_inds,
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
