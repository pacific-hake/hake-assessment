make.sampling.history.table <- function(dat,
                                        xcaption = "default", ## Caption to use
                                        xlabel   = "default", ## Latex label to use
                                        font.size = 9,        ## Size of the font for the table
                                        space.size = 10){       ## Size of the spaces for the table
  ## dat is a data frame containing the sampling history

  dat[is.na(dat)] <- "--"

  ## Add extra header top part of which is spanning multiple columns
  addtorow <- list()
  addtorow$pos <- list()
  addtorow$pos[[1]] <- -1
  addtorow$pos[[2]] <- nrow(dat)
  addtorow$command <- c(paste0("\\hline & \\multicolumn{6}{c|}{\\textbf{U.S.}} & \\multicolumn{4}{c|}{\\textbf{Canada}} \\\\ ",
                               " \\hline \\multirow{-2}{*}{\\textbf{Year}}",
                               " & \\specialcell{\\textbf{Foreign}\\\\\\textbf{(hauls)}}",
                               " & \\specialcell{\\textbf{Joint-}\\\\\\textbf{Venture}\\\\\\textbf{(hauls)}}",
                               " & \\specialcell{\\textbf{Mother-}\\\\\\textbf{ship}\\\\\\textbf{(hauls)}}",
                               " & \\specialcell{\\textbf{Combined}\\\\\\textbf{Mother-}\\\\\\textbf{ship}\\\\\\textbf{Catcher-}\\\\\\textbf{processor}\\\\\\textbf{(hauls)}}",
                               " & \\specialcell{\\textbf{Catcher-}\\\\\\textbf{processor}\\\\\\textbf{(hauls)}}",
                               " & \\specialcell{\\textbf{Shore-}\\\\\\textbf{based}\\\\\\textbf{(trips)}}",
                               " & \\textbf{Foreign}",
                               " & \\specialcell{\\textbf{Joint-}\\\\\\textbf{Venture}\\\\\\textbf{(hauls)}}",
                               " & \\specialcell{\\textbf{Shoreside}\\\\\\textbf{(trips)}}",
                               " & \\specialcell{\\textbf{Freezer-}\\\\\\textbf{trawl}\\\\\\textbf{(hauls)}} \\\\ \\hline "),
                        "\\hline ")

  ## Make the size string for font and space size
  size.string <- paste0("\\fontsize{",font.size,"}{",space.size,"}\\selectfont")
  return(print(xtable(dat,
                      caption = xcaption,
                      label = xlabel,
                      align = c("c", "|Y", "|Y", "Y", "Y", "Y", "Y", "Y|", "Y", "Y", "Y", "Y|")),
                      # align = c("c", "|r", "|r", "r", "r", "r", "r", "r|", "r", "r", "r", "r|")),    # Andy trying unsuccessfully to right justify columns
                      # align = get.align(ncol(dat))), # cannot use since need |
               caption.placement = "top",
               add.to.row = addtorow,
               include.rownames = FALSE,
               include.colnames = FALSE,
               table.placement="H",
               tabular.environment = "tabularx",
               width = "\\textwidth",
               sanitize.text.function=function(x){x},
               size=size.string,
               hline.after = NULL))
}
