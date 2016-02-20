make.data.overview.plot <- function(model,               ## model contains the output of SS_output
                                    show.title = FALSE){ ## Show the title?
  ## Make a plot of the data used in the assessment
  SSplotData(model, both = FALSE, show.title = show.title)
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
  d <- d[d$year %in% yrs,]
  ## Order by year and month
  d <- d[with(d, order(year, month)),]

  d.yr <- split(d, d$year)

  ## Fill in missing months with NA's
  d.list <- lapply(d.yr, function(x) merge(data.frame(list(month = 1:12)), x, all = TRUE))
  ## Replace those NA's with zeroes
  d <- list()
  for(i in 1:length(d.list)){
    tmp.df <- d.list[[i]]
    tmp.df[is.na(tmp.df)] <- 0
    d[[i]] <- tmp.df
  }

  ylim <- c(0, max(apply(sapply(d, "[[", "catch"), 2, sum)) / scale)

  catch.plot <- function(x, y, plot.type = c("default", "proportion", "cumulative"), ...){
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
    lines(xx, yy, ...)
  }
  plot(1,
       1,
       xlab = "",
       ylab = "",
       ##xlab = "Month",
       ##ylab = paste0("Cumulative Catch\n(", scale," t)"),
       xlim = c(1, 12),
       ylim = ylim,
       type = "n",
       axes = FALSE)
  cols <- length(d):1
  lwd <- 3
  lty <- 1
  for(i in 1:length(d)){
    catch.plot(d[[i]]$month,
               d[[i]]$catch / scale,
               plot.type = "cumulative",
               lty = lty,
               lwd = lwd,
               col = cols[i],
               type = "b",
               pch = 20,
               cex.axis = cex.axis)
  }
  axis(1)
  axis(2, las=1)
  box()
  if(!is.null(title)){
    mtext(title, line = 0, cex = title.cex)
  }
  legend("topleft", legend = yrs, col = cols, lty = lty, lwd = lwd, cex = leg.cex)
}

make.age.comp.plot <- function(d,
                               yrs = (year(now())-6):(year(now())-1), ## The years to show on the plot
                               title = NULL,
                               title.cex = 1, ## Title text size
                               bg.col = "blue"){ ## Fill color for the bubble
  ## This will make a plot of the ages comps found in the data a for the years given by yrs
  d <- d[rownames(d) %in% yrs,]
  plotBubbles(t(d),
              dnam = TRUE,
              ##xval = rownames(d),
              bg = bg.col,
              las = 1,
              powr = 0.5,
              size = 0.08,
              main = title)
}

make.wt.at.age.plot <- function(d,
                                ages,
                                lwd = 3,
                                lty = 1,
                                xlab = "Year",
                                ylab = "Mean weight-at-age (kg)"){
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
  text(year[length(year)] + 1, df[nrow(df),], ages, col = cols)
}

make.age.comp.compare.plot <- function(){
}

## make.age.comp.bubble.plot(base.model,
##                           subplot = 2,
##                           show.key = TRUE,
##                           key.yrs = c(2000, 2003, 2006, 2009)+1)
## make.age.comp.bubble.plot(base.model, subplot = 1)
