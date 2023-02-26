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
make_catch_table <- function(ct,
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

#' Make a summary table of landings and TACS
#'
#' @param d Data frame as output from [load_catches()]
#' @param start_yr Start year for the table
#' @param end_yr End year for the table. If past data range, the last year
#' in `d` will be used
#' @param xcaption Caption for the table
#' @param xlabel Latex label to use
#' @param placement Latex table placement character
#' @param font_size Size of font in table
#' @param space_size Vertica spacing between text in table
#'
#' @return An [xtable::xtable()]
#' @export
make_landings_tac_table <- function(d,
                                    start_yr,
                                    end_yr,
                                    xcaption = "default",
                                    xlabel   = "default",
                                    font_size = 9,
                                    space_size = 10,
                                    placement = "H"){

  tab <- as_tibble(d)
  yrs <- tab$year
  start_yr <- ifelse(start_yr < min(yrs), min(yrs), start_yr)
  end_yr <- ifelse(end_yr > max(yrs) | end_yr < start_yr, max(yrs), end_yr)
  yrs <- start_yr:end_yr

  prop_cols <- grep("prop|attain", names(tab))
  xx_cols <- grep("_xx", names(tab))

  tab <- tab %>%
    select(-all_of(xx_cols)) %>%
    filter(year %in% yrs) %>%
    mutate(year = as.character(year)) %>%
    mutate_at(.vars = vars(all_of(prop_cols)), ~{paste0(f(.x, 1), "\\%")}) %>%
    mutate_at(.vars = vars(-c(year, all_of(prop_cols))), ~{f(.x, 0)}) %>%
    mutate_all(~{str_replace_all(., "NA", "")}) %>%
    mutate_all(~{str_replace_all(., " \\\\%", "")}) %>%
    mutate_all(~{str_replace_all(., "^ +$", "--")})

  names(tab) <- c(latex_bold("Year"),
                  latex_mlc(c("U.S.",
                              "landings (t)")),
                  latex_mlc(c("Canada",
                              "landings (t)")),
                  latex_mlc(c("Total",
                              "landings (t)")),
                  latex_mlc(c("U.S.",
                              "proportion",
                              "of total",
                              "catch")),
                  latex_mlc(c("Canada",
                              "proportion",
                              "of total",
                              "catch")),
                  latex_mlc(c("U.S.",
                              "catch",
                              "target (t)")),
                  latex_mlc(c("Canada",
                              "catch",
                              "target (t)")),
                  latex_mlc(c("Coast-wide",
                              "catch",
                              "target (t)")),
                  latex_mlc(c("U.S.",
                              "proportion",
                              "of catch",
                              "target",
                              "removed")),
                  latex_mlc(c("Canada",
                              "proportion",
                              "of catch",
                              "target",
                              "removed")),
                  latex_mlc(c("Total",
                              "proportion",
                              "of catch",
                              "target",
                              "removed")))

  addtorow <- list()
  addtorow$pos <- list()
  addtorow$pos[[1]] <- -1

  addtorow$command <-
    paste0(latex_hline,
           paste(names(tab), collapse = latex_amp()),
           latex_nline,
           latex_hline)

  addtorow$command <- paste0(addtorow$command,
                             latex_continue(ncol(tab), addtorow$command))

  size_string <- latex_size_str(font_size, space_size)
  print(xtable(tab,
               caption = xcaption,
               label = xlabel,
               align = get.align(ncol(tab),
                                 first.left = FALSE,
                                 just = "r")),
               #digits = c(0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1)),
        caption.placement = "top",
        include.rownames = FALSE,
        include.colnames = FALSE,
        add.to.row = addtorow,
        table.placement = placement,   # think longtable ignores this anyway,
                                       # gives warning
        tabular.environment = "longtable",
        sanitize.text.function = function(x){x},
        size = size_string,
        hline.after = NULL)
}

make.catches.plot <- function(ct,
                              mar = c(4, 4, 6, 2) + 0.1,
                              leg.y.loc = 430,
                              leg.cex = 1){
  ## Plot the catches in a stacked-barplot with legend
  ##
  ## leg.y.loc - y-based location to place the legend
  ## leg.cex - text size for legend

  yrs <- ct$year
  ct[is.na(ct)] <- 0
  ct <- ct %>%
    select(can_foreign_xx,
           can_jv_xx,
           can_shore_xx,
           can_freeze_xx,
           us_foreign_xx,
           us_jv_xx,
           us_ms_xx,
           us_cp_xx,
           us_shore_xx)

  cols <- c(rgb(0, 0.8, 0),
            rgb(0, 0.6, 0),
            rgb(0.8, 0, 0),
            rgb(0.4, 0, 0),
            rgb(0, 0.2, 0),
            rgb(0, 0.4, 0),
            rgb(0, 0, 0.7),
            rgb(0, 0, 0.4),
            rgb(0, 0, 1))

  legOrder <- c(6, 5, 2, 1, 4, 3, NA, NA, 9, 8, 7)

  oldpar <- par()
  on.exit(par(oldpar))

  par(las = 1,
      mar = mar,
      cex.axis = 0.9)

  # Gives x-values for tick marks (since years not in ct)
  tmp <- barplot(t(as.matrix(ct)) / 1000,
                 beside = FALSE,
                 names = ct$year,
                 col = cols,
                 xlab = "Year",
                 ylab = "",
                 cex.lab = 1,
                 xaxt = "n",
                 xaxs = "i",
                 mgp = c(2.2, 1, 0),
                 ylim = c(0, 475))

  grid(NA, NULL, lty = 1, lwd = 1)
  mtext("Catch (thousand t)",
        side = 2,
        line = 2.8,
        las = 0,
        cex = 1.3)
  barplot(t(as.matrix(ct)) / 1000,
          beside = FALSE,
          names = ct$year,
          col = cols,
          xlab = "Year",
          ylab = "",
          cex.lab = 1,
          xaxt = "n",
          xaxs = "i",
          add = TRUE,
          mgp = c(2.2, 1, 0))
  # Big tick every five years:
  axis(1,
       at = tmp[(yrs %% 5) == 0],
       lab = yrs[(yrs %% 5) == 0])
  axis(1,
       at = tmp,
       lab = rep("",length(tmp)), tcl = -0.3)

  legend(x = 0, y = leg.y.loc,
         c("Canada Foreign",
           "Canada Joint-Venture",
           "Canada Shoreside",
           "Canada Freezer-Trawler",
           "U.S. Foreign",
           "U.S. Joint-Venture",
           "U.S. Mothership",
           "U.S. Catcher-Processor",
           "U.S. Shore-Based")[legOrder],
         bg = "white",
         horiz = FALSE,
         xpd = NA,
         cex = leg.cex,
         ncol = 3,
         fill = cols[legOrder],
         border = cols[legOrder],
         bty = "n")
}
