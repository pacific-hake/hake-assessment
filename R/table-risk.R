#' Creates LaTeX code to make a probability of risk table with various
#' probabilities of things happening with the stock
#'
#' @param model A model from this project
#' @param forecast_yrs A vector of forecast years
#' @param index Index for which forecast year data to use. e.g. 1 = second
#' forecast year compared to the first. If there were N forecast years, this
#' can be from 1 to N-1
#' @param digits The number of decimal places to print in the table output
#' @param type One of `probability` or `percent`. If `probability`, the table
#' columns will be decimal probabilities, if `percent` they will be expressed
#' as percentages and a percent symbol (%) will be added to the column headers
#' @param font_size The table data and header font size in points
#' @param header_font_size The font size for the headers only. If `NULL`,
#' the headers will have the same font size as the table cell data
#' @param vert_spacing The vertical spacing between newlines for this font.
#' If `NULL` this will be calculated as `header_font_size * header_vert_scale`
#' @param header_vert_scale Scale factor to create the vertical spacing value.
#' See `header_vert_spacing`
#' @param ... Arguments passed to [knitr::kable()]
#'
#' @return LaTeX code to render the table
#' @export
table_risk <- function(model,
                       forecast_yrs,
                       index = 1,
                       digits = 2,
                       type = c("probability", "percent"),
                       font_size = 10,
                       header_font_size = 10,
                       header_vert_spacing = 12,
                       header_vert_scale = 1.2,
                       ...){

  type <- match.arg(type)

  if(index > length(model$risks)){
    stop("`index` must be less than or equal to ", length(model$risks))
  }
  risk <- model$risks[[index]] |>
                        as_tibble()
  # Remove columns about MSY which are DFO values
  risk <- risk |>
    select(-contains("MSY"))
  # Fix tiny catch of less than 0.49 to zero, only for first (catch) column
  ct_col <- grep(paste0("^ForeCatch_",
                        model$endyr + index, "$"),
                 names(risk),
                 value = TRUE)
  ct_col_sym <- sym(ct_col)
  risk <- risk |>
    mutate(!!ct_col_sym := ifelse(!!ct_col_sym < 0.49,
                                  0,
                                  !!ct_col_sym))

  # Format all columns except catch to be zero decimal points and have a
  # percent sign and the catch to have a comma separator
  if(type == "probability"){
    risk <- risk |>
      mutate_at(vars(-(!!ct_col_sym)), ~{f(.x / 100, digits)})
  }else{
    risk <- risk |>
      mutate_at(vars(-(!!ct_col_sym)), ~{paste0(f(.x, digits), "\\%")})
  }
risk <- risk |>
  mutate(!!ct_col_sym := f(!!ct_col_sym)) |>
  mutate(let = paste0(letters[seq_len(nrow(risk))], ":")) |>
  select(let, everything()) %>%
  setNames(c("", names(.)[-1]))

  # Add nice header names
  col_names <- c("",
                 paste0("Catch (t)\nin ", forecast_yrs[index]),
                 paste0("$\\bm{\\mathrm{B}_{",
                        forecast_yrs[index + 1],
                        "}}$\n< $\\bm{\\mathrm{B}_{",
                        forecast_yrs[index],
                        "}}$"),
                 paste0("$\\bm{\\mathrm{B}_{",
                        forecast_yrs[index + 1],
                        "}}$\n< ", b_40_for_latex_table),
                 paste0("$\\bm{\\mathrm{B}_{",
                        forecast_yrs[index + 1],
                        "}}$\n< ", b_25_for_latex_table),
                 paste0("$\\bm{\\mathrm{B}_{",
                        forecast_yrs[index + 1],
                        "}}$\n< ", b_10_for_latex_table),
                 paste0(forecast_yrs[index],
                        "\nFishing\nintensity\n",
                        "> 100\\%"),
                 paste0(forecast_yrs[index + 1],
                        "\nDefault HR\ncatch\n",
                        "< ",
                        forecast_yrs[index],
                        "\ncatch"))

  if(type == "percent"){
    col_names[-1] <- paste0(col_names[-1], "\n(\\%)")
  }

  # Insert header fontsize if it wasn't supplied
  if(is.null(header_font_size)){
    header_font_size <- font_size
  }
  hdr_font_str <- create_fontsize_str(header_font_size,
                                      header_vert_spacing,
                                      header_vert_scale)

  # Insert font specs around all column headers
  col_names <- gsub("\\n", paste0("\n", hdr_font_str$quad), col_names)
  col_names <- paste0(hdr_font_str$dbl, col_names)
  # Add \\makecell{} latex command to headers with newlines (\n)
  col_names <- linebreaker(col_names, align = "c")

  kable(risk,
        format = "latex",
        booktabs = TRUE,
        align = c("l", rep("r", ncol(risk) - 1)),
        linesep = "",
        col.names = col_names,
        escape = FALSE,
        ...) |>
    row_spec(0, bold = TRUE) |>
    kable_styling(font_size = font_size,
                  latex_options = c("repeat_header"))
}
