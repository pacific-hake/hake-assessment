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
  names(yr_col) <- latex_bold(names(yr_col))
  tab <- bind_rows(tabnew, tabsums)
  names(tab) <- map_chr(names(tab), ~{latex_mlc(str_split(.x, "\\\\n")[[1]])})
  tab <- bind_cols(yr_col, map_dfr(tab, function(x) f(x)))
  tab[nrow(tab),] <- as.list(latex_bold(tab[nrow(tab),]))
  tab[-nrow(tab), ncol(tab)] <- latex_bold(tab[-nrow(tab), ncol(tab)] %>%
                                             pull())

  size.string <- latex_size_str(font.size, space.size)
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


