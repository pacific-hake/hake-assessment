#' Create a table comparing this year's biomass estimate and uncertainty to
#' this year's estimate of last year's biomass, and last year's estimate of
#' last year's biomass
#'
#' @param digits The number of decimal places to report in the table
#' @param font_size Size of the font for cell values
#' @param ly_model Last year's model object (typically `ly_model`)
#' @param cy_model This year's model object 9typically `base_model`)
#' @param last_yr Last year (typically `last_assess_yr`)
#' @param curr_yr Current year (typically `assess_yr`)
#'
#' @return a [kableExtra::kbl()] table
#' @export
table_yearly_model_compare <- function(ly_model,
                                       cy_model,
                                       last_yr,
                                       curr_yr,
                                       digits = 3,
                                       font_size = 8){

  ly_ly_slower <- f(ly_model$mcmccalcs$slower[as.character(last_assess_yr)],
                    digits)
  ly_ly_smed <- f(ly_model$mcmccalcs$smed[as.character(last_assess_yr)],
                  digits)
  ly_ly_supper <- f(ly_model$mcmccalcs$supper[as.character(last_assess_yr)],
                    digits)

  ly_cy_slower <- f(cy_model$mcmccalcs$slower[as.character(last_assess_yr)],
                    digits)
  ly_cy_smed <- f(cy_model$mcmccalcs$smed[as.character(last_assess_yr)],
                  digits)
  ly_cy_supper <- f(cy_model$mcmccalcs$supper[as.character(last_assess_yr)],
                    digits)

  cy_cy_slower <- f(cy_model$mcmccalcs$slower[as.character(assess_yr)],
                    digits)
  cy_cy_smed <- f(cy_model$mcmccalcs$smed[as.character(assess_yr)],
                  digits)
  cy_cy_supper <- f(cy_model$mcmccalcs$supper[as.character(assess_yr)],
                    digits)

  d <- tibble(`Calculation` =
                c(paste0(last_assess_yr,
                         " assessment $B_{",
                         last_assess_yr,
                         "}$"),
                  paste0(assess_yr,
                         " assessment $B_{",
                         last_assess_yr,
                         "}$"),
                  paste0(assess_yr,
                         " assessment $B_{",
                         assess_yr,
                         "}$")),
              `Low` = c(ly_ly_slower, ly_cy_slower, cy_cy_slower),
              `Median` = c(ly_ly_smed, ly_cy_smed, cy_cy_smed),
              `High` = c(ly_ly_supper, ly_cy_supper, cy_cy_supper))

  k <- kbl(d,
           format = "latex",
           booktabs = TRUE,
           align = c("l", "r", "r", "r", "r"),
           linesep = "",
           #col.names = col_names,
           escape = FALSE) |>
    row_spec(0, bold = TRUE) |>
    kable_styling(font_size = font_size)

  k
}
