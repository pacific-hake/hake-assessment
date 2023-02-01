make.sampling.history.table <- function(dat,
                                        xcaption = "default",
                                        xlabel   = "default",
                                        font.size = 9,
                                        space.size = 10){
  ## Returns an xtable in the proper format for the sampling effort
  ## dat is a data frame containing the sampling history
  ## xcaption - caption to appear in the calling document
  ## xlabel - the label used to reference the table in latex
  ## font.size - size of the font for the table
  ## space.size - size of the vertical spaces for the table

  dat[is.na(dat)] <- "--"

  addtorow <- list()
  addtorow$pos <- list()
  addtorow$pos[[1]] <- -1
  addtorow$pos[[2]] <- nrow(dat)
  addtorow$command <-
    c(paste0(latex.hline,
             latex.amp(),
             latex.mcol(6, "c|", latex.bold("U.S.")),
             latex.amp(),
             latex.mcol(4, "c|", latex.bold("Canada")),
             latex.nline,
             latex.hline,
             latex.mrow(-2, "*", latex.bold("Year")),
             latex.amp(),
             latex.mlc(c("Foreign",
                         "(hauls)")),
             latex.amp(),
             latex.mlc(c("Joint-",
                         "Venture",
                         "(hauls)")),
             latex.amp(),
             latex.mlc(c("Mother-",
                         "ship",
                         "(hauls)")),
             latex.amp(),
             latex.mlc(c("Combined",
                         "Mother-",
                         "ship",
                         "Catcher-",
                         "processor",
                         "(hauls)")),
             latex.amp(),
             latex.mlc(c("Catcher-",
                         "processor",
                         "(hauls)")),
             latex.amp(),
             latex.mlc(c("Shore-",
                         "based",
                         "(trips)")),
             latex.amp(),
             latex.mlc(c("Foreign",
                         "(hauls)")),
             latex.amp(),
             latex.mlc(c("Joint-",
                         "Venture",
                         "(hauls)")),
             latex.amp(),
             latex.mlc(c("Shoreside",
                         "(trips)")),
             latex.amp(),
             latex.mlc(c("Freezer",
                         "Trawlers",
                         "(hauls)")),
             latex.nline,
             latex.hline),
      latex.hline)

  size.string <- latex.size.str(font.size, space.size)
  print(xtable(dat,
               caption = xcaption,
               label = xlabel,
               align = c("c",
                         "|c",
                         "|c",
                         "c",
                         "c",
                         "c",
                         "c",
                         "c|",
                         "c",
                         "c",
                         "c",
                         "c|")),
        caption.placement = "top",
        add.to.row = addtorow,
        include.rownames = FALSE,
        include.colnames = FALSE,
        table.placement = "H",
        tabular.environment = "tabular",
        sanitize.text.function = function(x){x},
        size = size.string,
        hline.after = NULL)
}
