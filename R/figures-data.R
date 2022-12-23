make.data.overview.plot <- function(model,               ## model contains the output of SS_output
                                    show.title = FALSE, ...){ ## Show the title?
  ## Make a plot of the data used in the assessment
  SSplotData(model, subplot = 2, ...) #, both = FALSE#, datasize = TRUE) #TRUE for scaled to sample size bubbles
}

make.cumulative.catch.plot <- function(d,   ## Data as found in the xxxx.catch.by.month.csv files
                                       yrs = (year(now())-4):(year(now())-1), ## The years to show on the plot
                                       title = NULL,
                                       scale = 1000,  ## the catch data will be divided by this
                                       title.cex = 1, ## Title text size
                                       cex.axis = 1,
                                       leg.cex = 1){  ## Legend text size
  ## Cumulative catch plot for years given
  ## Remove any data with years not within limits
  if (!"month" %in% colnames(d)) {
    colnames(d) <- gsub("Year", "year", colnames(d))
    d <- reshape(d, direction = "long",
      idvar = "year", varying = 2:NCOL(d), sep = "",
      timevar = "month", v.names = "catch")
    d <- as.data.frame(d)
  }

  d <- d %>%
    dplyr::filter(year %in% yrs)
## TODO: Fix this to work on new file structure of can-ft-catch-by-month.csv.
## See hakedata package (canada) as it has this function already built in.

  catch.plot <- function(x, y, plot.type = c("default", "proportion", "cumulative")){
    ## x is period
    ## y is catch
    if(plot.type[1] == "proportion"){
        y <- cumsum(y) / sum(y)
    }
    if(plot.type[1] == "cumulative"){
        y <- cumsum(y)
    }
    xx <- 1:12
    yy <- rep(NA, 12)
    for(i in xx){
      if(any(x==i)){ #is the month present in the data
        yy[i] <- y[which(x==i)]
      }else{
        if(i == 1){
          yy[i] <- 0
        }else{
          if(plot.type[1] == "default"){
            yy[i] <- 0
          } else {
            yy[i] <- yy[i - 1]
          }
        }
      }
    }
    return(cbind(xx,yy))
  }
  d <- split(d, d$year)
  cols <- plotcolour(4)
  xx <- lapply(d, function(x) catch.plot(x$month, x$catch/scale, plot.type = "cumulative"))
  plot(1,
       1,
       xlab = "",
       ylab = "",
       ##xlab = "Month",
       ##ylab = paste0("Cumulative Catch\n(", scale," t)"),
       xlim = c(1, 12),
       ylim = c(0, max(do.call("rbind", xx)[,"yy"])),
       type = "n",
       axes = FALSE)
  lty <- 1
  lwd <- 3
  lapply(xx, function(x) lines(x[, "xx"], y = x[, "yy"], type = "b", pch = 20, cex.axis = cex.axis, lwd = lwd, lty = lty, col = cols[parent.frame()$i[]]))
  axis(1)
  axis(2, las=1)
  box()
  if(!is.null(title)){
    mtext(title, line = 0, cex = title.cex)
  }
  legend("topleft", legend = yrs, col = cols, lty = lty, lwd = lwd, cex = leg.cex, bty = "n")
}

#' A figure for age-composition data with years (x axis) and ages (y axis)
#'
#' A bubble plot of age compositions by year that
#' facilitates tracking cohorts through time.
#' The function is a wrapper for [PBSmodelling::plotBubbles] that subsets the
#' data and sets the color scheme.
#'
#' @param d The data, with years in the rows and ages in the columns.
#' @param yrs A vector of years that you want to include. This vector will be used to subset the
#' data in `d` based on rownames.
#' @param title The title to pass to [PBSmodelling::plotBubbles] `main` parameter.
#' @param title.cex A numeric value specifying the size of the text for the title.
#' @param bg.col A color for the `fill` of the bubbles.
#' @param ... Arguments to pass to [PBSmodelling::plotBubbles] such as `hide0`.
#'
#' @export
make.age.comp.plot <- function(d,
                               yrs = (year(now())-6):(year(now())-1), ## The years to show on the plot
                               title = NULL,
                               title.cex = 1, ## Title text size
                               bg.col = "blue", ...){ ## Fill color for the bubble
  ## This will make a plot of the ages comps found in the data a for the years given by yrs
  d <- d[rownames(d) %in% yrs,]
  PBSmodelling::plotBubbles(t(d),
              dnam = TRUE,
              ##xval = rownames(d),
              bg = bg.col,
              las = 1,
              powr = 0.5,
              size = 0.08,
              main = title,
              ...)
}

make.wt.at.age.plot <- function(d,
                                ages,
                                lwd = 3,
                                lty = 1,
                                xlab = "Year",
                                ylab = "Mean weight-at-age (kg)",
                                highlight = NULL){
  if (any(is.na(d$year))) d <- d[!is.na(d$year), ]
  d <- d[d$year > 0, ]
  year <- d$year
  df <- d[, names(d) %in% ages]
  col.fn <- colorRampPalette(c("darkblue", "blue", "green", "darkgreen"))
  cols <- col.fn(ncol(df))
  plot(year, df[,1],
       type = "l",
       col = cols[1],
       xlab = xlab,
       ylab = ylab,
       ylim = c(0, max(df)),
       pch = 20,
       lwd = lwd,
       lty = lty)
  for(i in 2:ncol(df)){
    lines(year, df[,i],
          ##type = "b",
          col = cols[i],
          pch = 20,
          lwd = lwd,
          lty = lty)
  }
  # Bold the highlighted age:
  if(!is.null(highlight)){
    lines(year, df[, names(df) == highlight],
          col = cols[names(df) == highlight],
          pch = 20,
          lwd = 3*lwd,
          lty = lty)
  }

  text(year[length(year)] + 1, df[nrow(df),], ages, col = cols)
}

make.age.comp.compare.plot <- function(){
}
