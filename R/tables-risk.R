#' Creates LaTeX code to make a probability of risk table with various probabilities
#' of things happening with the stock
#'
#' @param model A model from this project
#' @param forecast.yrs A vector of forecast years
#' @param index Index for which forecast year data to use. e.g. 1 = second forecast year
#' compared to the first. If there were N forecast years, this can be from 1 to N-1.
#' @param xcaption Caption to appear in the calling document
#' @param xlabel Label used to reference the table in LaTeX
#' @param font.size Point size of font in table
#' @param space.size Vertical space between rows
#' @param placement LaTeX code for placement of the table, e.g. "H" or "tbp"
#' @param digits Number of decimal places to round to for values in table
#' @param let_col_width_cm Column width in cm for the letters column
#' @param catch_col_width_cm Column width in cm for the catch column
#' @param col_width_cm  Column width in cm for all other columns
#'
#' @return LaTeX code to render the table
#' @export
make.risk.table <- function(model,
                            forecast.yrs,
                            index = 1,
                            digits = 0,
                            xcaption   = "default",
                            xlabel     = "default",
                            font.size  = 9,
                            space.size = 10,
                            let_col_width_cm = 0.2,
                            catch_col_width_cm = 1.6,
                            col_width_cm = 1.6,
                            placement = "H"){

  risk <- model$risks[[index]] |>
                        as_tibble()
  # Remove columns about MSY which are DFO values
  risk <- risk |>
    select(-contains("MSY"))
  # Fix tiny catch of less than 0.49 to zero, only for first (catch) column
  ct_col <- grep(paste0("^ForeCatch_", model$endyr + index, "$"), names(risk), value = TRUE)
  ct_col_sym <- sym(ct_col)
  risk <- risk |>
    mutate(!!ct_col_sym := ifelse(!!ct_col_sym < 0.49, 0, !!ct_col_sym))
  # Format all columns except catch to be zero decimal points and have a
  # percent sign and the catch to have a comma separator
  risk <- risk |>
    mutate_at(vars(-(!!ct_col_sym)), ~{paste0(f(.x, digits), "\\%")}) |>
    mutate(!!ct_col_sym := f(!!ct_col_sym)) |>
    mutate(let = paste0(letters[seq_len(nrow(risk))], ":")) |>
    select(let, everything()) %>%
    setNames(c("", names(.)[-1]))
  #mutate(!!ct_col_sym := paste0(letters[seq_len(nrow(risk))], ": ", f(!!ct_col_sym)))

  addtorow <- list()
  addtorow$pos <- list()
  addtorow$pos[[1]] <- -1
  addtorow$pos[[2]] <- nrow(risk)
  addtorow$command <-
    c(paste0("\\toprule \n",
             latex.amp(),
             latex.mlc(c("Catch (t)", paste("in",
                                        forecast.yrs[index]))),
             latex.amp(),
             latex.mlc(c("Probability",
                         paste0(latex.subscr("B", forecast.yrs[index+1]),
                                " < ",
                                latex.subscr("B", forecast.yrs[index])))),
             latex.amp(),
             latex.mlc(c("Probability",
                         paste0(latex.subscr("B", forecast.yrs[index+1]),
                                " < ",
                                latex.subscr("B", "40\\%")))),
             latex.amp(),
             latex.mlc(c("Probability",
                         paste0(latex.subscr("B", forecast.yrs[index+1]),
                                " < ",
                                latex.subscr("B", "25\\%")))),
             latex.amp(),
             latex.mlc(c("Probability",
                         paste0(latex.subscr("B", forecast.yrs[index+1]),
                                " < ",
                                latex.subscr("B", "10\\%")))),
             latex.amp(),
             latex.mlc(c("Probability",
                         paste(forecast.yrs[index], "relative"),
                         "fishing",
                         "intensity",
                         " > 100\\%")),
             latex.amp(),
             latex.mlc(c("Probability",
                         paste(forecast.yrs[index + 1], "default"),
                         "harvest policy",
                         "catch",
                         paste0(" < ", forecast.yrs[index], " catch"))),
             latex.nline,
             "\\midrule \n"),
      "\\bottomrule \n")

  # Make the size string for font and space size
  size.string <- latex.size.str(font.size, space.size)
  align <- c("l",
             paste0("R{", let_col_width_cm, "cm} "),
             paste0("R{", catch_col_width_cm, "cm} "),
             rep(paste0("R{", catch_col_width_cm, "cm} "), ncol(risk) - 2))

  print(xtable(risk,
               caption = xcaption,
               label = xlabel,
               align = align),
        caption.placement = "top",
        include.rownames = FALSE,
        include.colnames = FALSE,
        sanitize.text.function = function(x){x},
        size = size.string,
        add.to.row = addtorow,
        table.placement = placement,
        hline.after = NULL,
        tabular.environment = "tabular",
        booktabs = TRUE)
}
