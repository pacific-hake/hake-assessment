make.maturity.samples.table <- function(ovary.samples,
                                        start.yr,
                                        end.yr,
                                        xcaption = "default",
                                        xlabel   = "default",
                                        font.size = 10,
                                        space.size = 10){
  ## Returns an xtable in the proper format for the fishery sample sizes
  ##
  ## ovary.samples - the data as read in from the csv file
  ## xcaption - caption to appear in the calling document
  ## xlabel - the label used to reference the table in latex
  ## font.size - size of the font for the table
  ## space.size - size of the vertical spaces for the table

  tab <- ovary.samples

  tab[,-1] <- f(tab[,-1])
  tab[is.na(tab)] <- "--"
  colnames(tab) <-
    c(latex.bold("Year"),
      latex.mlc(c("NWFSC",
                  "Trawl",
                  "Survey")),
      latex.mlc(c("Acoustic",
                  "survey/Research",
                  "(Summer)")),
      latex.mlc(c("Acoustic",
                  "survey/Research",
                  "(Winter)")),
      latex.mlc(c("U.S. At-Sea Hake",
                  "Observer",
                  "Program (Spring)")),
      latex.mlc(c("U.S. At-Sea Hake",
                  "Observer",
                  "Program (Fall)")),
      latex.bold("Total"))

  size.string <- latex.size.str(font.size, space.size)
  ## Make the totals row all bold
  tab[nrow(tab),] <- latex.bold(tab[nrow(tab),])
  ## Make the totals column all bold (except for the last one which was
  ##  made bold on previous call)
  tab[1:(nrow(tab) - 1), ncol(tab)] <-
    latex.bold(tab[1:(nrow(tab) - 1), ncol(tab)])

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

  tab <- maturity.ogives

  ## format all non-year-column values with a thousands seperator
  colnames(tab) <-
    c(latex.bold("Age"),
      latex.mlc(c("Number of",
                  "samples")),
      latex.mlc(c("Old",
                  "fecundity")),
      latex.mlc(c("New",
                  "fecundity")),
      latex.mlc(c("Maturity",
                  "ogive")),
      latex.mlc(c("Mean",
                  "weight")))
  ## Make the size string for font and space size
  size.string <- latex.size.str(font.size, space.size)

  print(xtable(tab,
               caption = xcaption,
               label = xlabel,
               align = get.align(ncol(tab),
                                 first.left = FALSE,
                                 just = "c"),
               digits = c(0, 0, 0, 3, 3, 3, 3)),
        caption.placement = "top",
        include.rownames = FALSE,
        table.placement = "H",
        sanitize.text.function = function(x){x},
        size = size.string)
}
