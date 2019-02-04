## Functions to load catches table from a csv file, display it as a table,
##  and plot the figure. Also calculates years.Can.JV.catch.eq.0.

load.catches <- function(fn){
  ## Reads in the catches file, splits it up into two data frames,
  ##  one for catches and one for the catches/tac.
  ## fn - the filename with relative path
  ## In the catches data frame, any NA's will be replaced by zeroes.
  ## In the landings/tac table, they will remain NAs
  catches <- read.csv(fn)
  landings.vs.tac <-
    as.data.frame(cbind(Year = catches$Year,
                        Ustotal = catches$Ustotal,
                        CANtotal = catches$CANtotal,
                        TOTAL = catches$TOTAL,
                        TAC = catches$TAC,
                        TACCAN = catches$TACCAN,
                        TACUSA = catches$TACUSA))

  landings.vs.tac <-
    as.data.frame(cbind(landings.vs.tac,
                        landings.vs.tac$Ustotal / landings.vs.tac$TACUSA * 100,
                        landings.vs.tac$CANtotal / landings.vs.tac$TACCAN * 100,
                        landings.vs.tac$TOTAL / landings.vs.tac$TAC * 100))

  colnames(landings.vs.tac) <- c("Year",
                                 "Ustotal",
                                 "CANtotal",
                                 "TOTAL",
                                 "TAC",
                                 "TACCAN",
                                 "TACUSA",
                                 "USATTAIN",
                                 "CANATTAIN",
                                 "ATTAIN")
  catches <- catches[,!names(catches) %in% c("TAC", "TACCAN", "TACUSA")]
  catches[is.na(catches)] <- 0
  list(catches = catches,
       landings.vs.tac = landings.vs.tac)
}

make.catches.table <- function(catches,
                               start.yr,
                               end.yr,
                               xcaption = "default",
                               xlabel   = "default",
                               font.size = 9,
                               space.size = 10,
                               placement = "H"){
  ## Returns an xtable in the proper format for the executive summary catches
  ##
  ## catches - output of the load.catches function above.
  ## start.yr - the first year to show in the table
  ## end.yr - the last year to show in the table
  ## xcaption - caption to appear in the calling document
  ## xlabel - the label used to reference the table in latex
  ## font.size - size of the font for the table
  ## space.size - size of the vertical spaces for the table
  ## digits - number of decimal points on % columns
  ## placement - latex code for placement of the table in document

  if(start.yr > 1991){
    ## If start.yr > 1991 then US foreign, US JV, and Canadian foreign
    ##  will be removed since they are all zeroes.
    catches <- catches[,c("Year",
                          "atSea_US_MS",
                          "atSea_US_CP",
                          "US_shore",
                          "USresearch",
                          "Ustotal",
                          "CAN_JV",
                          "CAN_Shoreside",
                          "CAN_FreezeTrawl",
                          "CANtotal",
                          "TOTAL")]
    colnames(catches) <- c(latex.bold("Year"),
                           latex.mlc(c("US",
                                       "Mother-",
                                       "ship")),
                           latex.mlc(c("US",
                                       "Catcher-",
                                       "processor")),
                           latex.mlc(c("US",
                                       "Shore-",
                                       "based")),
                           latex.mlc(c("US",
                                       "Research")),
                           latex.mlc(c("US",
                                       "Total")),
                           latex.mlc(c("CAN",
                                       "Joint-",
                                       "Venture")),
                           latex.mlc(c("CAN",
                                       "Shore-",
                                       "side")),
                           latex.mlc(c("CAN",
                                       "Freezer",
                                       "Trawlers")),
                           latex.mlc(c("CAN",
                                       "Total")),
                           latex.bold("Total"))
  }else{
    colnames(catches) <- c("Year",
                           "US\nForeign",
                           "US\nJV",
                           "US\nMother-\nship",
                           "US\nCatcher-\nProcessor",
                           "US\nShore-\nbased",
                           "US\nResearch",
                           "US\nTotal",
                           "CAN\nForeign",
                           "CAN\nJoint-\nVenture",
                           "CAN\nShoreside",
                           "CAN\nFreezer-Trawler",
                           "CAN\nTotal",
                           "Total")
  }
  ## Filter for correct years to show and make thousand-seperated numbers
  ##  (year assumed to be column 1)
  catches <- catches[catches[,1] >= start.yr & catches[,1] <= end.yr,]
  ## -1 below means leave the years alone and don't comma-seperate them
  catches[,-1] <-f(catches[-1])

  size.string <- latex.size.str(font.size, space.size)
  print(xtable(catches,
               caption = xcaption,
               label = xlabel,
               align = get.align(ncol(catches))),
        caption.placement = "top",
        include.rownames = FALSE,
        table.placement = placement,
        sanitize.text.function = function(x){x},
        size = size.string)
}

make.catches.plot <- function(catches,
                              mar = c(4, 4, 6, 2) + 0.1,
                              leg.y.loc = 430,
                              leg.cex = 1){
  ## Plot the catches in a stacked-barplot with legend
  ##
  ## leg.y.loc - y-based location to place the legend
  ## leg.cex - text size for legend

  years <- catches$Year
  catches <- catches[,c("CAN_forgn",
                        "CAN_JV",
                        "CAN_Shoreside",
                        "CAN_FreezeTrawl",
                        "US_foreign",
                        "US_JV",
                        "atSea_US_MS",
                        "atSea_US_CP",
                        "US_shore")]
  cols <- c(rgb(0, 0.8, 0),
            rgb(0, 0.6, 0),
            rgb(0.8, 0, 0),
            rgb(0.4, 0, 0),
            rgb(0, 0.2, 0),
            rgb(0, 0.4, 0),
            rgb(0, 0, 0.7),
            rgb(0, 0, 0.4),
            rgb(0, 0, 1))
  legOrder <- c(6, 5, 2, 1, 4, 3, NA, NA, 9, 8, 7)
  oldpar <- par()
  par(las = 1,
      mar = mar,
      cex.axis = 0.9)
  tmp <- barplot(t(as.matrix(catches)) / 1000,
                 beside = FALSE,
                 names = catches[,1],
                 col = cols,
                 xlab = "Year",
                 ylab = "",
                 cex.lab = 1,
                 xaxt = "n",
                 mgp = c(2.2, 1, 0),
                 ylim = c(0, 475))
  axis(1, at = tmp, labels = years, line = -0.12)
  grid(NA, NULL, lty = 1, lwd = 1)
  mtext("Catch (thousand t)",
        side = 2,
        line = 2.8,
        las = 0,
        cex = 1.3)
  barplot(t(as.matrix(catches)) / 1000,
          beside = FALSE,
          names = catches[,1],
          col = cols,
          xlab = "Year",
          ylab = "",
          cex.lab = 1,
          xaxt = "n",
          add = TRUE,
          mgp = c(2.2, 1, 0))

  legend(x = 0, y = leg.y.loc,
         c("Canada Foreign",
           "Canada Joint-Venture",
           "Canada Shoreside",
           "Canada Freezer Trawl",
           "U.S. Foreign",
           "U.S. Joint-Venture",
           "U.S. MS",
           "U.S. CP",
           "U.S. Shore-based")[legOrder],
         bg = "white",
         horiz = FALSE,
         xpd = NA,
         cex = leg.cex,
         ncol = 3,
         fill = cols[legOrder],
         border = cols[legOrder],
         bty = "n")
  par <- oldpar
}

make.catches.table.US <- function(catches,
                                  start.yr,
                                  end.yr,
                                  xcaption = "default",
                                  xlabel   = "default",
                                  font.size = 9,
                                  space.size = 10){
  ## Returns an xtable in the proper format for the full catches from the U.S.
  ##
  ## catches - output of the load.catches function above.
  ## start.yr - the first year to show in the table
  ## end.yr - the last year to show in the table
  ## xcaption - caption to appear in the calling document
  ## xlabel - the label used to reference the table in latex
  ## font.size - size of the font for the table
  ## space.size - size of the vertical spaces for the table

  catches <- catches[,c("Year",
                        "US_foreign",
                        "US_JV",
                        "atSea_US_MS",
                        "atSea_US_CP",
                        "US_shore",
                        "USresearch",
                        "Ustotal")]
  colnames(catches) <- c(latex.bold("Year"),
                         latex.bold("Foreign"),
                         latex.bold("JV"),
                         latex.bold("Mothership"),
                         latex.bold("Catcher-Processor"),
                         latex.bold("Shore-based"),
                         latex.bold("Research"),
                         latex.bold("Total"))

  ## Filter for correct years to show and make thousand-seperated numbers
  ##  (year assumed to be column 1)
  catches <- catches[catches[,1] >= start.yr & catches[,1] <= end.yr,]
  ## -1 below means leave the years alone and don't comma-seperate them
  catches[,-1] <-f(catches[-1])

  size.string <- latex.size.str(font.size, space.size)
  print(xtable(catches,
               caption = xcaption,
               label = xlabel,
               align = get.align(ncol(catches))),
        caption.placement = "top",
        include.rownames = FALSE,
        table.placement = "H",
        tabular.environment = "longtable",
        sanitize.text.function = function(x){x},
        size = size.string)
}

make.catches.table.Can <- function(catches,
                                   start.yr,
                                   end.yr,
                                   xcaption = "default",
                                   xlabel   = "default",
                                   font.size = 9,
                                   space.size = 10){
  ## Returns an xtable in the proper format for the full catches from Canada.
  ##
  ## catches - output of the load.catches function above.
  ## start.yr - the first year to show in the table
  ## end.yr - the last year to show in the table
  ## xcaption - caption to appear in the calling document
  ## xlabel - the label used to reference the table in latex
  ## font.size - size of the font for the table
  ## space.size - size of the vertical spaces for the table

  catches <- catches[,c("Year",
                        "CAN_forgn",
                        "CAN_JV",
                        "CAN_Shoreside",
                        "CAN_FreezeTrawl",
                        "CANtotal")]
  colnames(catches) <- c(latex.bold("Year"),
                         latex.bold("Foreign"),
                         latex.bold("JV"),
                         latex.bold("Shoreside"),
                         latex.bold("Freezer-trawl"),
                         latex.bold("Total"))

  ## Filter for correct years to show and make thousand-seperated numbers
  ##  (year assumed to be column 1)
  catches <- catches[catches[,1] >= start.yr & catches[,1] <= end.yr,]
  ## -1 below means leave the years alone and don't comma-seperate them
  catches[,-1] <-f(catches[-1])

  size.string <- latex.size.str(font.size, space.size)
  print(xtable(catches,
               caption = xcaption,
               label = xlabel,
               align = get.align(ncol(catches))),
        caption.placement = "top",
        include.rownames = FALSE,
        table.placement = "H",
        tabular.environment = "longtable",
        sanitize.text.function = function(x){x},
        size = size.string)
}

make.catches.table.total <- function(catches,
                                     start.yr,
                                     end.yr,
                                     xcaption = "default",
                                     xlabel   = "default",
                                     font.size = 9,
                                     space.size = 10){
  ## Returns an xtable in the proper format for the full catches from Canada
  ##  and US combined
  ##
  ## catches - output of the load.catches function above.
  ## start.yr - the first year to show in the table
  ## end.yr - the last year to show in the table
  ## xcaption - caption to appear in the calling document
  ## xlabel - the label used to reference the table in latex
  ## font.size - size of the font for the table
  ## space.size - size of the vertical spaces for the table
  catches <- catches[,c("Year",
                        "Ustotal",
                        "CANtotal",
                        "TOTAL")]
  catches[,5] <- 100 * catches[,2] / catches[,4]
  catches[,6] <- 100 * catches[,3] / catches[,4]
  colnames(catches) <- c(latex.bold("Year"),
                         latex.bold("Total U.S."),
                         latex.bold("Total Canada"),
                         latex.bold("Total coastwide"),
                         latex.bold("Percent U.S."),
                         latex.bold("Percent Canada"))

  ## Filter for correct years to show and make thousand-seperated numbers
  ##  (year assumed to be column 1)
  catches <- catches[catches[,1] >= start.yr & catches[,1] <= end.yr,]
  catches[,-c(1, 5, 6)] <- f(catches[,-c(1, 5, 6)])

  catches[,c(5, 6)] <- f(catches[,c(5, 6)], 1)

  size.string <- latex.size.str(font.size, space.size)
  print(xtable(catches,
               caption = xcaption,
               label = xlabel,
               align = get.align(ncol(catches))),
        caption.placement = "top",
        include.rownames = FALSE,
        table.placement = "H",
        tabular.environment = "longtable",
        sanitize.text.function = function(x){x},
        size = size.string)
}

make.landings.tac.table <- function(landings.vs.tac,
                                    start.yr,
                                    end.yr,
                                    xcaption = "default",
                                    xlabel   = "default",
                                    font.size = 9,
                                    space.size = 10,
                                    placement = "H",
                                    tabular.env = "tabular"){
  ## Returns an xtable in the proper format for the executive summary landings
  ##  vs. TAC for management performance section
  ##
  ## landings.vs.tac - data frame read in from the csv file
  ## start.yr - the first year to show in the table
  ## end.yr - the last year to show in the table
  ## xcaption - caption to appear in the calling document
  ## xlabel - the label used to reference the table in latex
  ## font.size - size of the font for the table
  ## space.size - size of the vertical spaces for the table
  ## placement - latex code for the placement of the table in the document
  ## tabular.env - which tabular environment to use, e.g. "tabularx" or "tabular"

  tab <- landings.vs.tac

  ## Filter for correct years to show and make thousand-seperated numbers
  ##  (year assumed to be column 1)
  tab <- tab[tab$Year >= start.yr & tab$Year <= end.yr,]
  tab[,-c(1, 8, 9, 10)] <- f(tab[,-c(1, 8, 9, 10)])

  ## Round the proportions to one decimal place
  tab[,8] <- paste0(f(tab[,8], 1), "\\%")
  tab[,9] <- paste0(f(tab[,9], 1), "\\%")
  tab[,10] <- paste0(f(tab[,10], 1), "\\%")
  ## Switch TACCAN and TACUSA columns for consistency
  tmp <- tab[,6]
  tab[,6] <- tab[,7]
  tab[,7] <- tmp
  colnames(tab) <- c(latex.bold("Year"),
                     latex.mlc(c("US",
                                 "landings (t)")),
                     latex.mlc(c("Canada",
                                 "landings (t)")),
                     latex.mlc(c("Total",
                                 "landings (t)")),
                     latex.mlc(c("Coast-wide",
                                 "catch",
                                 "target (t)")),
                     latex.mlc(c("US",
                                 "catch",
                                 "target (t)")),
                     latex.mlc(c("Canada",
                                 "catch",
                                 "target (t)")),
                     latex.mlc(c("US",
                                 "proportion",
                                 "of catch",
                                 "target",
                                 "removed")),
                     latex.mlc(c("Canada",
                                 "proportion",
                                 "of catch",
                                 "target",
                                 "removed")),
                     latex.mlc(c("Total",
                                 "proportion",
                                 "of catch",
                                 "target",
                                 "removed")))


  size.string <- latex.size.str(font.size, space.size)
  print(xtable(tab,
               caption = xcaption,
               label = xlabel,
               align = get.align(ncol(tab),
                                 first.left = FALSE,
                                 just = "c"),
               digits = c(0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1)),
        caption.placement = "top",
        include.rownames = FALSE,
        table.placement = placement,
        tabular.environment = tabular.env,
        sanitize.text.function = function(x){x},
        size = size.string)
}

years.Can.JV.catch.eq.0 <- function(catches,
                                    start.yr = 1999){
  ## Calculate the recent years that had no Canadian JV catch, for Introduction
  ##
  ## catches - output of the load.catches function above
  ## start.yr - year from which to look for no Canadian JV catch

  years.Can.JV.catch.eq.0 <-
    catches[ catches$CAN_JV == 0, ]$Year
  years.Can.JV.catch.eq.0.recent <-
    years.Can.JV.catch.eq.0[years.Can.JV.catch.eq.0 > start.yr]
  years.Can.JV.catch.eq.0.recent
}

further.tac.details <- function(fn){
  ## Reads in the further.tac.details file and returns it as a data.frame
  ##
  ## fn - the filename with relative path
  read.csv(fn, header = TRUE, sep = ",", comment.char = "#")
}

make.catches.table.us.ap <- function(catches,
                                     xcaption = "default",
                                     xlabel   = "default",
                                     font.size = 9,
                                     space.size = 10,
                                     placement = "H"){
  ## Returns an xtable for the US AP appendix
  ##
  ## catches - data frame of catches in the format of data/us-ap-catch.csv
  ## xcaption - caption to appear in the calling document
  ## xlabel - the label used to reference the table in latex
  ## font.size - size of the font for the table
  ## space.size - size of the vertical spaces for the table
  ## placement - latex code for the placement of the table in the document

  make.true.NA <- function(x){
    ## Change <NA> to NA
    if(is.character(x) || is.factor(x)){
      is.na(x) <- x %in% c("NA", "<NA>")
      x
    }else{
      as.numeric(x)
    }
  }

  colnames(catches)[1] <- ""
  nr <- nrow(catches)
  nc <- ncol(catches)
  ## Make value cells be comma-separated
  catches[!grepl("\\%", catches[, 1]), -1] <- apply(
    catches[!grepl("\\%", catches[, 1]), -1], 1:2, 
    function(x) format(as.numeric(x), big.mark = ","))

  ## Change factorized version <NA> to NA
  catches <- apply(catches, c(1,2), make.true.NA)
  catches <- apply(catches, c(1,2), function(x){gsub("NA", NA, x)})
  ## Replace NA with --
  catches[is.na(catches)] <- ""

  ## Make percent cells have percent signs
  catches <- apply(catches, 1:2, function(x) gsub("\\%", "\\\\%", x))
  ## Fix Total for Util Init. Alloc cell
  catches[
    grep("Init", catches[, 1], ignore.case = TRUE), 
    grep("total", colnames(catches), ignore.case = TRUE)] <- ""

  ## Make column and row headers bold
  catches[,1] <- latex.bold(catches[,1])
  colnames(catches) <- gsub("\\.{2}", " ", colnames(catches))
  colnames(catches) <- gsub(" (\\D+)\\.$", " \\(\\1\\)", colnames(catches))
  colnames(catches) <- gsub("US |U.S ", "U.S. ", colnames(catches))
  colnames(catches) <- gsub("([[:lower:]])\\.([[:upper:]])", "\\1 \\2", colnames(catches))
  colnames(catches)[-1] <- latex.bold(colnames(catches)[-1])
  ## Make it so that the first row heading doesn't appear.
  ##  This must be a space and not the null string
  colnames(catches)[1] <- " "

  size.string <- latex.size.str(font.size, space.size)
  print(xtable(catches,
               caption = xcaption,
               label = xlabel,
               align = get.align(ncol(catches))),
        caption.placement = "top",
        include.rownames = FALSE,
        table.placement = placement,
        sanitize.text.function = function(x){x},
        size = size.string)
}
