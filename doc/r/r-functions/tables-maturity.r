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

  ## Filter for correct years to show and make thousand-seperated numbers (year assumed to be column 1)
  tab <- tab[tab$Year >= start.yr & tab$Year <= end.yr,]
  #tab[,-1] <- fmt0(tab[,-1])
  tab[is.na(tab)] <- "--"

  colnames(tab) <- c("\\textbf{Year}",
                     "\\specialcell{\\textbf{NWFSC}\\\\\\textbf{Trawl}\\\\\\textbf{Survey}}",
                     "\\specialcell{\\textbf{Acoustic}\\\\\\textbf{Survey}}",
                     "\\specialcell{\\textbf{At-Sea Hake}\\\\\\textbf{Observer}\\\\\\textbf{Program}}")
  ## Make the size string for font and space size
  size.string <- paste0("\\fontsize{", font.size, "}{", space.size, "}\\selectfont")
  return(print(xtable(tab, caption=xcaption,
                      label = xlabel,
                      align = get.align(ncol(tab), first.left = FALSE, just = "c"),
                      digits = rep(0, ncol(tab) + 1)),
               caption.placement = "top",
               include.rownames = FALSE,
               table.placement = "H",
               sanitize.text.function = function(x){x},
               size = size.string))
}
