#' Make a latex table of credible intervals with columns for female spawning biomass,
#' relative spawning biomass, total biomass, age 2+ biomass, age-0 recruits, relative
#' fishing intensity, and exploitation fraction. Also output a CSV file with the table contents.
#'
#' @param model A model list as output by [create_rds_file()]
#' @param start_yr Start year in the table
#' @param end_yr End year in the table
#' @param weight.factor Factor to divide biomass values by and multiply the recruitment by
#' @param xcaption Caption for the latex table
#' @param xlabel Label for the latex table
#' @param font.size Size of font
#' @param space.size Vertical spacing between text in table
#' @param digits Number of decimal points to round to
#' @param lower_col Character string name of the column containing the lower credible interval values
#' @param upper_col Character string name of the column containing the upper credible interval values
#'
#' @return An [xtable::xtable()]
#' @export
make.ci.posterior.table <- function(model,
                                    start_yr,
                                    end_yr,
                                    weight.factor = 1000,
                                    xcaption = "default",
                                    xlabel   = "default",
                                    font.size = 9,
                                    space.size = 10,
                                    digits = 1,
                                    lower_col = NULL,
                                    upper_col = NULL){

  stopifnot(!is.null(lower_col))
  stopifnot(!is.null(upper_col))

  lower_col_sym <- sym(lower_col)
  upper_col_sym <- sym(upper_col)
  yrs <- start_yr:end_yr
  df <- map(model$mcmccalcs, ~{.x[names(.x) %in% yrs]})
  ts <- model$timeseries

  tot_bm <- model$extra_mcmc$total_biomass_quants %>%
    filter(Yr %in% yrs)
  stopifnot(lower_col %in% names(tot_bm))
  stopifnot(upper_col %in% names(tot_bm))
  tot_bm_lower <- tot_bm %>%
    pull(!!lower_col_sym)
  tot_bm_upper <- tot_bm %>%
    pull(!!upper_col_sym)

  age2plus_bm <- model$extra_mcmc$total_age2_plus_biomass_quants %>%
    filter(Yr %in% yrs)
  stopifnot(lower_col %in% names(age2plus_bm))
  stopifnot(upper_col %in% names(age2plus_bm))
  age2plus_bm_lower <- age2plus_bm %>%
    pull(!!lower_col_sym)
  age2plus_bm_upper <- age2plus_bm %>%
    pull(!!upper_col_sym)

  tab.filt <- cbind(yrs,
                    paste0(f(df$slower * weight.factor),
                           "-",
                           f(df$supper * weight.factor)),
                    paste0(f(df$dlower * 100, digits),
                           "-",
                           f(df$dupper * 100, digits), "\\%"),
                    paste0(f(tot_bm_lower / weight.factor),
                           "-",
                           f(tot_bm_upper / weight.factor)),
                    paste0(f(age2plus_bm_lower / weight.factor),
                           "-",
                           f(age2plus_bm_upper / weight.factor)),
                    paste0(f(df$rlower * weight.factor),
                           "-",
                           f(df$rupper * weight.factor)),
                    paste0(f(df$plower * 100, digits),
                           "-",
                           f(df$pupper * 100, digits),
                           "\\%"),
                    paste0(f(df$flower * 100, digits),
                           "-",
                           f(df$fupper * 100, digits),
                           "\\%"))

  ## Make current year have dashes for exploitation rate and fishing intensity
  tab.filt[nrow(tab.filt), ncol(tab.filt)] <- latex_bold("--")
  tab.filt[nrow(tab.filt), ncol(tab.filt) - 1] <- latex_bold("--")

  ## Add latex headers
  colnames(tab.filt) <- c(latex_bold("Year"),
                          latex_mlc(c("Female",
                                      "spawning",
                                      "biomass",
                                      "(thousand t)")),
                          latex_mlc(c("Relative",
                                      "spawning",
                                      "biomass")),
                          latex_mlc(c("Total",
                                      "biomass",
                                      "(thousand t)")),
                          latex_mlc(c("Age-2+",
                                      "biomass",
                                      "(thousand t)")),
                          latex_mlc(c("Age-0",
                                      "recruits",
                                      "(millions)")),
                          latex_mlc(c("(1-SPR)",
                                      "/",
                                      paste0("(1-",
                                             latex_subscr("SPR", "40\\%"),
                                             ")"))),
                          latex_mlc(c("Exploitation",
                                      "fraction")))

  size.string <- latex_size_str(font.size, space.size)
  last_row <- tab.filt[nrow(tab.filt),]
  last_row <- gsub("-", " - ", last_row)
  last_row <- gsub(" -  - ", "--", last_row)
  tab.filt <- tab.filt[1:(nrow(tab.filt) - 1),]
  tab.filt <- map_df(as_tibble(tab.filt), ~{gsub("-", " - ", .x)}) %>%
    as.data.frame %>%
    bind_rows(last_row)

  addtorow <- list()
  addtorow$pos <- list()
  addtorow$pos[[1]] <- -1
  addtorow$command <-
    paste0(latex_hline,
           paste(colnames(tab.filt), collapse = latex_amp()),
           latex_nline,
           latex_hline)

  addtorow$command <- paste0(addtorow$command,
                             latex_continue(ncol(tab.filt), addtorow$command))

  print(xtable(tab.filt,
               caption = xcaption,
               label = xlabel,
               align = get.align(ncol(tab.filt)),
               digits = digits),
        caption.placement = "top",
        include.rownames = FALSE,
        include.colnames = FALSE,
        add.to.row = addtorow,
        table.placement = "H",
        tabular.environment = "longtable",
        sanitize.text.function = function(x){x},
        size = size.string,
        hline.after = NULL)
}


