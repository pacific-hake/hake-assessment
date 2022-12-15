make.assessment.changes.table <- function(assessment.changes,
                                          start_yr = 2011,
                                          end.yr = NULL,
                                          xcaption = "default",
                                          xlabel   = "default",
                                          font.size = 10,
                                          space.size = 10){
  ## Returns an xtable in the proper format for changes to the model structure
  ##
  ## assessment.changes - the data as read in from the csv file
  ## xcaption - caption to appear in the calling document
  ## xlabel - the label used to reference the table in latex
  ## font.size - size of the font for the table
  ## space.size - size of the vertical spaces for the table
  tab <- assessment.changes

  tab$Comp_Method <- gsub("<NA>",
                          "NA",
                          gsub("Dir.+",
                               "DM",
                               gsub("McAl.+",
                                    "MI",
                                    tab$Comp_Method)))
  tab$Comp_Method <- apply(tab[, grep("Comp_", colnames(tab))], 1,
                           function(x) paste0(x[1], " (", x[2], ", ", x[3], ")"))
  tab <- tab[, -grep("Comp_F|Comp_S", colnames(tab))]
  tab <- tab[tab$Year >= start_yr, ]
  #tab$Bias_Adjust <- format(tab$Bias_Adjust, digits = 2)
  tab$MCMC <- f(tab$MCMC)
  #tab$Change <- sapply(strsplit(tab$Change, "; |, "), latex.mlc, make.bold = FALSE)

  colnames(tab) <- sapply(strsplit(colnames(tab), "_"), latex.mlc)

  addtorow <- list()
  addtorow$pos <- list()
  addtorow$pos[[1]] <- -1

  addtorow$command <-
    paste0(latex.hline,
           paste(colnames(tab), collapse = latex.amp()),
           latex.nline,
           latex.hline)

  addtorow$command <- paste0(addtorow$command,
                             latex_continue(ncol(tab), addtorow$command))

  size.string <- latex.size.str(font.size, space.size)
  # Cannot have multiple periods in a number (software version must be separated by dashes
  # instead to satisfy accessibility requirements)
  tab[,2] <- gsub("\\.", "-", tab[,2])

  print(xtable(tab,
               caption = xcaption,
               label = xlabel,
               align = c(rep("r", ncol(tab)-1), "r", "p{5cm}"),
               digits = rep(0, ncol(tab) + 1)),
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
