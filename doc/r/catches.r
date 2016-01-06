## Functions to load catches table from a csv file, display it as a table,
## and plot the figure.

load.catches <- function(fn ## fn is the filename with reletive path
                         ){
  ## Reads in the catches file, replacing any NA's with zeroes
  catches <- read.csv(fn)
  catches[is.na(catches)] <- 0
  return(catches)
}

make.catches.table <- function(catches,              ## The output of the load.catches function above.
                               start.yr,             ## start.yr is the first year to show in the table
                               end.yr,               ## end.yr is the last year to show in the table
                               weight.factor = 1000, ## divide catches by this factor
                               xcaption = "default", ## Caption to use
                               xlabel   = "default", ## Latex label to use
                               font.size = 9,        ## Size of the font for the table
                               space.size = 10       ## Size of the spaces for the table
                               ){
  ## Returns an xtable in the proper format for the executive summary catches
  if(start.yr > 1991){
    ## If start.yr > 1991 then US foreign, US JV, and Canadian foreign will be removed since they are all zeroes.
    catches <- catches[,c("Year","atSea_US_MS","atSea_US_CP","US_shore","USresearch","Ustotal",
                          "CAN_JV","CAN_Shoreside","CAN_FreezeTrawl","CANtotal","TOTAL")]
    colnames(catches) <- c("\\specialcell{\\textbf{Year}}",
                           "\\specialcell{\\textbf{US}\\\\\\textbf{Mother-}\\\\\\textbf{ship}}",
                           "\\specialcell{\\textbf{US}\\\\\\textbf{Catcher}-\\\\\\textbf{Processor}}",
                           "\\specialcell{\\textbf{US}\\\\\\textbf{Shore-}\\\\\\textbf{based}}",
                           "\\specialcell{\\textbf{US}\\\\\\textbf{Research}}",
                           "\\specialcell{\\textbf{US}\\\\\\textbf{Total}}",
                           "\\specialcell{\\textbf{CAN}\\\\\\textbf{Joint}\\\\\\textbf{Venture}}",
                           "\\specialcell{\\textbf{CAN}\\\\\\textbf{Shore-}\\\\\\textbf{side}}",
                           "\\specialcell{\\textbf{CAN}\\\\\\textbf{Freezer-}\\\\\\textbf{Trawler}}",
                           "\\specialcell{\\textbf{CAN}\\\\\\textbf{Total}}",
                           "\\specialcell{\\textbf{Total}}")
  }else{
    colnames(catches) <- c("Year","US\nForeign","US\nJV","US\nMother-\nship","US\nCatcher-\nProcessor","US\nShore-\nbased","US\nResearch","US\nTotal",
                           "CAN\nForeign","CAN\nJoint-\nVenture","CAN\nShoreside","CAN\nFreezer-Trawler","CAN\nTotal","Total")
  }
  ## Filter for correct years to show and make thousand-seperated numbers (year assumed to be column 1)
  catches <- catches[catches[,1] >= start.yr & catches[,1] <= end.yr,]
  catches[,-1] <- fmt0(catches[,-1])  ## -1 means leave the years alone and don't comma-seperate them

  ## Make the size string for font and space size
  size.string <- paste0("\\fontsize{",font.size,"}{",space.size,"}\\selectfont")
  return(print(xtable(catches, caption=xcaption, label=xlabel, align=get.align(ncol(catches))),
               caption.placement = "top", include.rownames=FALSE, sanitize.text.function=function(x){x}, size=size.string))
}

make.catches.plot <- function(catches,
                              leg.y.loc = 430  ## Y-based location to place the legend
                              ){
  ## Plot the catches in a stacked-barplot with legend
  years <- catches$Year
  catches <- catches[,c("CAN_forgn","CAN_JV","CAN_Shoreside","CAN_FreezeTrawl","US_foreign","US_JV","atSea_US_MS","atSea_US_CP","US_shore")]
  cols <- c(rgb(0,0.8,0), rgb(0,0.6,0), rgb(0.8,0,0), rgb(0.4,0,0), rgb(0,0.2,0),
            rgb(0,0.4,0), rgb(0,0,0.7), rgb(0,0,0.4), rgb(0,0,1))
  legOrder <- c(6,5,2,1,4,3,NA,NA,9,8,7)
  oldpar <- par()
  par(las=1,mar=c(4, 4, 6, 2) + 0.1,cex.axis=0.9)
  tmp <- barplot(t(as.matrix(catches))/1000, beside=FALSE, names=catches[,1],
                 col=cols,xlab="Year", ylab="", cex.lab=1, xaxt="n", mgp=c(2.2,1,0))
  axis(1,at=tmp, labels=years, line=-0.12)
  grid(NA,NULL,lty=1,lwd = 1)
  mtext("Catch ('000 mt)",side=2,line=2.8,las=0,cex=1.3)
  barplot(t(as.matrix(catches))/1000,beside=FALSE,names=catches[,1],
          col=cols, xlab="Year",ylab="",cex.lab=1,xaxt="n",add=TRUE,mgp=c(2.2,1,0))

legend(x=0,y=leg.y.loc,
         c("Canadian Foreign","Canadian Joint-Venture","Canadian Shoreside","Canadian Freezer Trawl",
           "U.S. Foreign","U.S. Joint-Venture","U.S. MS","U.S. CP","U.S. Shore-based")[legOrder],
         bg="white",horiz=F,xpd=NA,cex=1,ncol=3,fill=cols[legOrder],border=cols[legOrder],bty="n")
  par <- oldpar
}


