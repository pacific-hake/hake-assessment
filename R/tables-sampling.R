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
    c(paste0(latex_hline,
             latex_amp(),
             latex_mcol(6, "c|", latex_bold("U.S.")),
             latex_amp(),
             latex_mcol(4, "c|", latex_bold("Canada")),
             latex_nline,
             latex_hline,
             latex_mrow(-2, "*", latex_bold("Year")),
             latex_amp(),
             latex_mlc(c("Foreign",
                         "(hauls)")),
             latex_amp(),
             latex_mlc(c("Joint-",
                         "Venture",
                         "(hauls)")),
             latex_amp(),
             latex_mlc(c("Mother-",
                         "ship",
                         "(hauls)")),
             latex_amp(),
             latex_mlc(c("Combined",
                         "Mother-",
                         "ship",
                         "Catcher-",
                         "processor",
                         "(hauls)")),
             latex_amp(),
             latex_mlc(c("Catcher-",
                         "processor",
                         "(hauls)")),
             latex_amp(),
             latex_mlc(c("Shore-",
                         "based",
                         "(trips)")),
             latex_amp(),
             latex_mlc(c("Foreign",
                         "(hauls)")),
             latex_amp(),
             latex_mlc(c("Joint-",
                         "Venture",
                         "(hauls)")),
             latex_amp(),
             latex_mlc(c("Shoreside",
                         "(trips)")),
             latex_amp(),
             latex_mlc(c("Freezer",
                         "Trawlers",
                         "(hauls)")),
             latex_nline,
             latex_hline),
      latex_hline)

  size.string <- latex_size_str(font.size, space.size)
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
