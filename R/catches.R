#' Make summary of catch for given country (US, Canada, or both).
#' Default is a longtable for main-tables.rnw, use tabular
#' for Executive Summary.
#'
#' @param ct Output from [load_catches()]
#' @param country 1 for Canada, 2 for US, anything else for both
#' @param start_yr Start year in table
#' @param end_yr End year in table
#' @param xcaption Latex caption
#' @param xlabel Latex label
#' @param font_size Font size
#' @param space_size Between-letter space size
#' @param table_placement Latex for where to put table (H, tbp, etc.)
#' @param tabular_environment longtable or tabular
#' @param hline_after Default NULL is for longtable, gets changed for tabluar
#' @return An [xtable::xtable()]
#' @export
make_catch_table_latex <- function(ct,
                             country = 3,
                             start_yr,
                             end_yr,
                             xcaption = "default",
                             xlabel   = "default",
                             font_size = 9,
                             space_size = 10,
                             table_placement = "H",
                             tabular_environment = "longtable",
                             hline_after = NULL){

  ct <- as_tibble(ct)
  yrs <- ct$year
  start_yr <- ifelse(start_yr < min(yrs), min(yrs), start_yr)
  end_yr <- ifelse(end_yr > max(yrs) | end_yr < start_yr, max(yrs), end_yr)
  yrs <- start_yr:end_yr

  if(tabular_environment == "tabular"){
    hline_after <- length(yrs)
  }

  if(country == 1){
    ct <- ct %>%
      select(year, can_foreign_xx, can_jv_xx, can_shore_xx, can_freeze_xx, can_catch)
    ct_names <- c(latex_bold("Year"),
                  latex_bold("Foreign"),
                  latex_bold("JV"),
                  latex_bold("Shoreside"),
                  latex_bold("Freezer-Trawler"),
                  latex_bold("Total"))
  }else if(country == 2){
    ct <- ct %>%
      select(year, us_foreign_xx, us_jv_xx, us_ms_xx, us_cp_xx, us_shore_xx, us_research_xx, us_catch)
    ct_names <- c(latex_bold("Year"),
                  latex_bold("Foreign"),
                  latex_bold("JV"),
                  latex_bold("Mothership"),
                  latex_bold("Catcher-Processor"),
                  latex_bold("Shore-based"),
                  latex_bold("Research"),
                  latex_bold("Total"))
  }else{
    ct <- ct %>%
      select(year, us_ms_xx, us_cp_xx, us_shore_xx, us_research_xx, us_catch,
             can_jv_xx, can_shore_xx, can_freeze_xx, can_catch, tot_catch)
    ct_names <- c(latex_bold("Year"),
                  latex_mlc(c("US",
                              "Mother-",
                              "ship")),
                  latex_mlc(c("US",
                              "Catcher-",
                              "Processor")),
                  latex_mlc(c("US",
                              "Shore-",
                              "Based")),
                  latex_mlc(c("US",
                              "Research")),
                  latex_mlc(c("US",
                              "Total")),
                  latex_mlc(c("CAN",
                              "Joint-",
                              "Venture")),
                  latex_mlc(c("CAN",
                              "Shoreside")),
                  latex_mlc(c("CAN",
                              "Freezer-",
                              "Trawler")),
                  latex_mlc(c("CAN",
                              "Total")),
                  latex_bold("Total"))
  }

  ct <- ct %>%
    filter(year %in% yrs) %>%
    mutate(year = as.character(year)) %>%
    mutate_at(.vars = vars(-year), ~{f(.x, 0)})

  addtorow <- list()
  addtorow$pos <- list()
  addtorow$pos[[1]] <- -1

  addtorow$command <-
    paste0(latex_hline,
           paste(ct_names, collapse = latex_amp()),
           latex_nline,
           latex_hline)

  if(tabular_environment == "longtable"){
    addtorow$command <- paste0(addtorow$command,
                               latex_continue(ncol(ct), addtorow$command))
  }

  size_string <- latex_size_str(font_size, space_size)
  print(xtable(ct,
               caption = xcaption,
               label = xlabel,
               align = get.align(ncol(ct))),
        caption.placement = "top",
        include.rownames = FALSE,
        include.colnames = FALSE,
        add.to.row = addtorow,
        table.placement = table_placement,
        tabular.environment = tabular_environment,
        sanitize.text.function = function(x){x},
        size = size_string,
        hline.after = hline_after)
}


