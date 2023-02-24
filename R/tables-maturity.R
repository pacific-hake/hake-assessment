#' Make the ovary collection table
#'
#' @details The header in the file is used as the header in the table. If you wish to place a newline
#' in any header label, insert a `\\n` in the label
#' @param ovary_samples_df The values as read in from the ovary samples CSV file ("ovary-samples.csv")
#'  using [readr::read_csv()]
#' @param xcaption The caption
#' @param xlabel The latex label to use
#' @param font.size Size of the font for the table
#' @param space.size Size of the vertical spaces for the table
#'
#' @return An [xtable] object
#' @export
make.maturity.samples.table <- function(ovary_samples_df,
                                        xcaption = "default",
                                        xlabel   = "default",
                                        font.size = 10,
                                        space.size = 10){

  tab <- ovary_samples_df %>% as.data.frame

  tabnew <- tab %>%
    select(-Year) %>%
    mutate(Total = rowSums(.))

  tabsums <- tabnew %>%
    summarize_all(.funs = ~{if(is.numeric(.)) sum(.) else "Total"})

  yr_col <- c(tab$Year, "Total") %>%
    enframe %>%
    select(-name) %>%
    rename(Year = value)
  names(yr_col) <- latex.bold(names(yr_col))
  tab <- bind_rows(tabnew, tabsums)
  names(tab) <- map_chr(names(tab), ~{latex.mlc(str_split(.x, "\\\\n")[[1]])})
  tab <- bind_cols(yr_col, map_dfr(tab, function(x) f(x)))
  tab[nrow(tab),] <- as.list(latex.bold(tab[nrow(tab),]))
  tab[-nrow(tab), ncol(tab)] <- latex.bold(tab[-nrow(tab), ncol(tab)] %>%
                                             pull())

  size.string <- latex.size.str(font.size, space.size)
  print(xtable(tab, caption = xcaption,
               label = xlabel,
               align = get.align(ncol(tab),
                                 first.left = FALSE,
                                 just = "r"),
               digits = rep(0, ncol(tab) + 1)),
        caption.placement = "top",
        include.rownames = FALSE,
        table.placement = "H",
        sanitize.text.function = function(x){x},
        size = size.string)
}


make.maturity.ogives.table <- function(maturity.ogives,
                                       xcaption = "default",
                                       xlabel   = "default",
                                       font.size = 10,
                                       space.size = 10){
  ## Returns an xtable
  ##
  ## maturity.ogives - data as read in from the csv file
  ## xcaption - caption to appear in the calling document
  ## xlabel - the label used to reference the table in latex
  ## font.size - size of the font for the table
  ## space.size - size of the vertical spaces for the table

  tab <- maturity_ogives_df
  corder <- sapply(c("age", "n.ovaries", "maturity", "avg.wt",
    "new.fecundity"), grep, x = colnames(tab), ignore.case = TRUE)
  tab <- tab[, corder]
  ## format all non-year-column values with a thousands seperator
  colnames(tab) <-
    c(latex.bold("Age"),
      latex.mlc(c("Number of",
                  "samples")),
      latex.mlc(c("Maturity",
                  "ogive")),
      latex.mlc(c("Mean",
                  "weight")),
      latex.mlc(c("Mean",
                  "fecundity")))
  ## Make the size string for font and space size
  size.string <- latex.size.str(font.size, space.size)

  print(xtable(tab,
               caption = xcaption,
               label = xlabel,
               align = get.align(ncol(tab),
                                 first.left = FALSE,
                                 just = "r"),
               digits = c(0, 0, 0, 3, 3, 3)),
        caption.placement = "top",
        include.rownames = FALSE,
        table.placement = "H",
        sanitize.text.function = function(x){x},
        size = size.string)
}
