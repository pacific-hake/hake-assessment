#' Creates LaTeX code to make a probability of risk table with various
#' probabilities of things happening with the stock
#'
#' @param model A model from this project
#' @param forecast_yrs A vector of forecast years
#' @param index Index for which forecast year data to use. e.g. 1 = second
#' forecast year compared to the first. If there were N forecast years, this
#' can be from 1 to N-1
#' @param digits The number of decimal places to print in the table output
#'
#' @return LaTeX code to render the table
#' @export
table_risk <- function(model,
                       forecast_yrs,
                       index = 1,
                       digits = 0,
                       font_size = 9,
                       ...){

  if(index > length(model$risks)){
    stop("`index` must be less than or equal to ", length(model$risks),
         call. = FALSE)
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
  risk <- risk |>
    mutate_at(vars(-(!!ct_col_sym)), ~{paste0(f(.x, digits), "\\%")}) |>
    mutate(!!ct_col_sym := f(!!ct_col_sym)) |>
    mutate(let = paste0(letters[seq_len(nrow(risk))], ":")) |>
    select(let, everything()) %>%
    setNames(c("", names(.)[-1]))

  # Add nice header names
  col_names <- c("",
                 paste0("Catch (t)\nin ", forecast_yrs[index]),
                 paste0("Probability\n",
                        "$\\bm{\\mathrm{B}_{",
                        forecast_yrs[index + 1],
                        "}}$ < $\\bm{\\mathrm{B}_{",
                        forecast_yrs[index],
                        "}}$"),
                 paste0("Probability\n",
                        "$\\bm{\\mathrm{B}_{",
                        forecast_yrs[index + 1],
                        "}}$ < $\\bm{\\Bforty}$"),
                 paste0("Probability\n",
                        "$\\bm{\\mathrm{B}_{",
                        forecast_yrs[index + 1],
                        "}}$ < $\\bm{\\Btwentyfive}$"),
                 paste0("Probability\n",
                        "$\\bm{\\mathrm{B}_{",
                        forecast_yrs[index + 1],
                        "}}$ < $\\bm{\\Bten}$"),
                 paste0("Probability\n",
                        forecast_yrs[index],
                        " relative\nfishing\nintensity\n",
                        "> 100\\%"),
                 paste0("Probability\n",
                        forecast_yrs[index + 1],
                        " default\nharvest policy\ncatch\n",
                        "> ",
                        forecast_yrs[index],
                        " catch"))

  col_names <- linebreak(col_names, align = "c")

  kable(risk,
        format = "latex",
        booktabs = TRUE,
        align = c("l", rep("r", ncol(risk) - 1)),
        linesep = "",
        col.names = col_names,
        escape = FALSE,
        ...) |>
    row_spec(0, bold = TRUE) |>
    kable_styling(font_size = font_size)
}
