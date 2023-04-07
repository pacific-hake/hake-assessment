#' Creates a table containing reference point estimates
#'
#' @param model A model, created by [create_rds_file()]
#' @param font_size Size of the font in points for the table data
#' @param ... Arguments passed to [knitr::kable()]
#'
#' @return a [knitr::kable()] object
#' @export
table_reference_points <- function(model,
                                   font_size = 10,
                                   ...){

  df <- model$mcmccalcs$refpts |>
    map_df(~{.x}) |>
    t() |>
    as_tibble(rownames = "Quantity")

  names(df) <- c("Quantity",
                 "2.5\\textsuperscript{th}\npercentile",
                 "Median",
                 "97.5\\textsuperscript{th}\npercentile")

  descr <- c("Unfished female spawning biomass ($\\Bzero$, thousand t)",
             "Unfished recruitment ($\\Rzero$, millions)",
             "Female spawning biomass at $\\FSPRforty$ ($\\BSPRforty$, thousand t)",
             "SPR at $\\FSPRforty$",
             "Exploitation fraction corresponding to $\\FSPRforty$",
             "Yield associated with $\\FSPRforty$ (thousand t)",
             "Female spawning biomass ($\\Bforty$, thousand t)",
             "SPR at $\\Bforty$",
             "Exploitation fraction resulting in $\\Bforty$",
             "Yield at $\\Bforty$ (thousand t)",
             "Female spawning biomass ($\\Bmsy$, thousand t)",
             "SPR at MSY",
             "Exploitation fraction corresponding to SPR at MSY",
             "MSY (thousand t)")

  df <- df |>
    mutate(Quantity = descr)
  col_names <- linebreaker(names(df), align = "c")

  # Insert a new row made up of the vector `row_vec` at row `row_ind` in
  # data frame `d`
  insert_row <- function(d, row_vec, row_ind) {
    d[seq(row_ind + 1, nrow(d) + 1), ] <- d[seq(row_ind, nrow(d)),]
    d[row_ind, ] <- vec2df(row_vec)
    d
  }

  df <- insert_row(df, c("", "", "", ""), 3)
  df <- insert_row(df,
                   c(paste0("\\textbf{\\underline{Reference points ",
                            "(equilibrium) based on $\\bm{\\FSPRforty}$}}"),
                     "", "", ""), 4)

  df <- insert_row(df, c("", "", "", ""), 9)
  df <- insert_row(df,
                   c(paste0("\\textbf{\\underline{Reference points ",
                            "(equilibrium) based on $\\bm{\\Bforty}$ ",
                            "(40\\% of $\\bm{\\Bzero}$)}}"),
                     "", "", ""), 10)

  df <- insert_row(df, c("", "", "", ""), 15)
  df <- insert_row(df,
                   c(paste0("\\textbf{\\underline{Reference points ",
                            "(equilibrium) based on estimated MSY}}"),
                     "", "", ""), 16)

  kable(df,
        format = "latex",
        booktabs = TRUE,
        align = c("l", "c", "c", "c"),
        linesep = "",
        col.names = col_names,
        escape = FALSE,
        ...) |>
    row_spec(0, bold = TRUE) |>
    kable_styling(font_size = font_size)
}