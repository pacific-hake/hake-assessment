make.maturity.samples.table <- function(ovary.samples,
                                        start.yr,             ## start.yr is the first year to show in the table
                                        end.yr,               ## end.yr is the last year to show in the table
                                        xcaption = "default", ## Caption to use
                                        xlabel   = "default", ## Latex label to use
                                        font.size = 10,       ## Size of the font for the table
                                        space.size = 10       ## Size of the spaces for the table
                                    ){
  ## Returns an xtable in the proper format for the fishery sample sizes
  tab <- ovary.samples

  ## Table now includes "Total" row so no longer should filter by year range
  ## Filter for correct years to show and make thousand-seperated numbers (year assumed to be column 1)
                                        #tab <- tab[tab$Year >= start.yr & tab$Year <= end.yr,]
  ## The next line formats all non-year-column values with a thousands seperator
  tab[,-1] <- f(tab[,-1])
  tab[is.na(tab)] <- "--"
  colnames(tab) <- c("\\textbf{Year}",
                     "\\specialcell{\\textbf{NWFSC}\\\\\\textbf{Trawl}\\\\\\textbf{Survey}}",
                     "\\specialcell{\\textbf{Acoustic}\\\\\\textbf{Survey/Research}\\\\\\textbf{(Summer)}}",
                     "\\specialcell{\\textbf{Acoustic}\\\\\\textbf{Survey/Research}\\\\\\textbf{(Winter)}}",
                     "\\specialcell{\\textbf{U.S. At-Sea Hake}\\\\\\textbf{Observer}\\\\\\textbf{Program (Spring)}}",
                     "\\specialcell{\\textbf{U.S. At-Sea Hake}\\\\\\textbf{Observer}\\\\\\textbf{Program (Fall)}}",
                     "\\textbf{Total}")
  ## Make the size string for font and space size
  size.string <- paste0("\\fontsize{", font.size, "}{", space.size, "}\\selectfont")
  ## Make the totals row all bold
  tab[nrow(tab),] <- paste0("\\textbf{", tab[nrow(tab),], "}")
  ## Make the totals column all bold (except for the last on which was made bold on previous call)
  tab[1:(nrow(tab) - 1), ncol(tab)] <- paste0("\\textbf{", tab[1:(nrow(tab) - 1), ncol(tab)], "}")

  return(print(xtable(tab, caption=xcaption,
                      label = xlabel,
                      align = get.align(ncol(tab), first.left = FALSE, just = "r"),
                      digits = rep(0, ncol(tab) + 1)),
               caption.placement = "top",
               include.rownames = FALSE,
               table.placement = "H",
               sanitize.text.function = function(x){x},
               size = size.string))
}



make.maturity.ogives.table <- function(maturity.ogives,
                                       interval = 2, ## use display 2cm values (vs 1cm)
                                       xcaption = "default", ## Caption to use
                                       xlabel   = "default", ## Latex label to use
                                       font.size = 10,       ## Size of the font for the table
                                       space.size = 10       ## Size of the spaces for the table
                                       ){
  ## This function based on make.maturity.samples.table above, but created by
  ## Ian who doesn't have much experience with xtable so may contain errors
  ## or silly-looking code
  ##
  ## Returns an xtable

  tab <- maturity.ogives
  # subset to values evenly divisible by the 'interval' input (e.g. 2cm instead of 1cm)
  tab <- tab[tab$length.cm %% interval == 0,]

  ## The next line formats all non-year-column values with a thousands seperator
  colnames(tab) <- c("\\textbf{Length (cm)}",
                     # note: could be a ref but don't know how to do that in a table
                     "\\specialcell{\\textbf{Dorn and }\\\\\\textbf{Saunders (1997)}}",
                     "\\specialcell{\\textbf{2015}\\\\\\textbf{assessment}}")
  ## Make the size string for font and space size
  size.string <- paste0("\\fontsize{", font.size, "}{", space.size, "}\\selectfont")

  return(print(xtable(tab, caption=xcaption,
                      label = xlabel,
                      align = get.align(ncol(tab), first.left = FALSE, just = "c"),
                      digits = c(0, 0, 2, 2)),
               caption.placement = "top",
               include.rownames = FALSE,
               table.placement = "H",
               sanitize.text.function = function(x){x},
               size = size.string))
}
