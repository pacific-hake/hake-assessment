#' Creates a table containing reference point estimates
#'
#' @param model A model, created by [create_rds_file()]
#' @param hdr_italics Logical. If `TRUE`, make the cohort header lines
#' italicized
#' @param category_bold Logical. If `TRUE`, make the category header lines
#' boldface
#' @param category_underline Logical. If `TRUE`, make the category header lines
#' underlined
#' @param category_line_above Logical. If `TRUE`, place a horizontal line above
#' category header lines
#' @param category_line_below Logical. If `TRUE`, place a horizontal line below
#' category header lines
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
table_reference_points <- function(model,
                                   category_bold = TRUE,
                                   category_italics = TRUE,
                                   category_underline = TRUE,
                                   category_line_above = FALSE,
                                   category_line_below = FALSE,
                                   font_size = 10,
                                   header_font_size = 10,
                                   header_vert_spacing = 12,
                                   header_vert_scale = 1.2,
                                   ...){

  d <- model$mcmccalcs$refpts |>
    map_df(~{.x}) |>
    t() |>
    as_tibble(rownames = "Quantity")

  descr <- c(paste0("Unfished female spawning biomass (", b_0_for_latex_table, ", kt)"),
             paste0("Unfished recruitment (", r_0_for_latex_table, ", millions)"),
             paste0("Reference points (equilibrium) based on ", fspr_40_bold_for_latex_table),
             paste0("Female spawning biomass at ", fspr_40_for_latex_table, "(", bspr_40_for_latex_table, ", kt)"),
             paste0("SPR at ", fspr_40_for_latex_table),
             paste0("Exploitation fraction corresponding to ", fspr_40_for_latex_table),
             paste0("Yield associated with ", fspr_40_for_latex_table, " (kt)"),
             paste0("Reference points ",
                    "(equilibrium) based on ", b_40_bold_for_latex_table,
                    "(40\\% of ", b_0_bold_for_latex_table, ")"),
             paste0("Female spawning biomass (", b_40_for_latex_table, ", kt)"),
             paste0("SPR at ", b_40_for_latex_table),
             paste0("Exploitation fraction resulting in ", b_40_for_latex_table),
             paste0("Yield at ", b_40_for_latex_table, " (kt)"),
             "Reference points (equilibrium) based on estimated MSY",
             paste0("Female spawning biomass (", b_msy_for_latex_table, ", kt)"),
             "SPR at MSY",
             "Exploitation fraction corresponding to SPR at MSY",
             "MSY (kt)")

  hdr_inds <- c(3, 8, 13)

  # Insert empty rows at the row indices where the section headers are
  row_vec <- rep("", 4)
  walk(hdr_inds, ~{
    d <<- insert_row(d, row_vec, .x)
  })

  d <- d |>
    mutate(Quantity = descr)

  if(category_underline){
    # Make section headers bold
    d[hdr_inds, "Quantity"] <- map(d[hdr_inds, "Quantity"], ~{
      latex_under(.x)
    })
  }
  if(category_italics){
    # Make section headers italics
    d[hdr_inds, "Quantity"] <- map(d[hdr_inds, "Quantity"], ~{
      latex_italics(.x)
    })
  }
  if(category_bold){
    # Make section headers bold
    d[hdr_inds, "Quantity"] <- map(d[hdr_inds, "Quantity"], ~{
      latex_bold(.x)
    })
  }

  col_names <- c("Quantity",
                 "2.5\\%",
                 "Median",
                 "97.5\\%")

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
           align = c("l", "r", "r", "r"),
           linesep = "",
           col.names = col_names,
           escape = FALSE,
           ...) |>
    row_spec(0, bold = TRUE)

  if(category_line_above){
    k <- k |>
      row_spec(hdr_inds - 1,
               extra_latex_after = paste0("\\cline{",
                                          1,
                                          "-",
                                          length(col_names),
                                          "}"))
  }
  if(category_line_below){
    k <- k |>
      row_spec(hdr_inds,
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