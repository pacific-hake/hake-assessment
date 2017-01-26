make.can.age.data.table <- function(dat,
                                    fleet = 1,            ## 1=Can-Shoreside, 2=Can-FT, 3=Can-JV
                                    start.yr,             ## Start the table on this year
                                    xcaption = "default",
                                    xlabel = "default",
                                    font.size = 9,        ## Size of the font for the table
                                    space.size = 10,      ## Size of the spaces for the table
                                    placement = "H",
                                    decimals = 4
                                    ){
  ## Returns an xtable in the proper format for the main tables section for Canadian
  ##  age data.
  ages.df <- dat[[fleet]]
  n.trip.haul <- as.numeric(dat[[fleet + 3]])
  ages.df <- cbind(n.trip.haul, ages.df)
  dat <- cbind(as.numeric(rownames(ages.df)), ages.df)
  dat <- dat[dat[,1] >= start.yr,]

  dat[,2] <- as.numeric(f(dat[,2]))
  dat[,-c(1,2)] <- f(dat[,-c(1,2)], decimals)
  ## Add the extra header spanning multiple columns
  addtorow <- list()
  addtorow$pos <- list()
  addtorow$pos[[1]] <- -1
  addtorow$pos[[2]] <- nrow(dat)
  age.headers <- colnames(dat)[grep("^[[:digit:]].*", colnames(dat))]
  n.age <- length(age.headers)
  ages <- paste0("\\multicolumn{1}{c}{\\textbf{", 1:(n.age), "}} & ")
  ages.tex <- do.call("paste", as.list(ages))
  ## Remove last ampersand
  ages.tex <- sub("& $", "\\\\\\\\", ages.tex)

  if(fleet == 2 | fleet == 3){
    addtorow$command <- c(paste0("\\hline \\textbf{Year} & \\specialcell{\\textbf{Number}\\\\\\textbf{of hauls}} & \\multicolumn{", n.age, "}{c}{\\textbf{Age}} \\\\",
                                 "\\hline & & ", ages.tex),
                          "\\hline ")
  }else{
    addtorow$command <- c(paste0("\\hline \\textbf{Year} & \\specialcell{\\textbf{Number}\\\\\\textbf{of trips}} & \\multicolumn{", n.age, "}{c}{\\textbf{Age}} \\\\",
                                 "\\hline & & ", ages.tex),
                          "\\hline ")
  }

  ## Make the size string for font and space size
  size.string <- paste0("\\fontsize{",font.size,"}{",space.size,"}\\selectfont")

  return(print(xtable(dat,
                      caption = xcaption,
                      label = xlabel,
                      align = get.align(ncol(dat))),
               caption.placement = "top",
               include.rownames = FALSE,
               include.colnames = FALSE,
               sanitize.text.function = function(x){x},
               size = size.string,
               add.to.row = addtorow,
               table.placement = placement,
               tabular.environment = "tabular",
               hline.after = NULL))
}

make.us.age.data.table <- function(dat,
                                   fleet = 1,            ## 1=US-CP, 2=US-MS, 3=US-Shoreside
                                   xcaption = "default",
                                   xlabel = "default",
                                   font.size = 9,        ## Size of the font for the table
                                   space.size = 10,      ## Size of the spaces for the table
                                   placement = "H",
                                   decimals = 4
                                   ){

  ## Returns an xtable in the proper format for the main tables section for US
  ##  age data.

  if(fleet == 1 | fleet == 2){
    dat[,c(2,3)] <- f(dat[,c(2,3)])
    dat[,-c(1,2,3)] <- f(dat[,-c(1,2,3)], decimals)
  }else{
    dat[,2] <- f(dat[,2])
    dat[,-c(1,2)] <- f(dat[,-c(1,2)], decimals)
  }

  ## Add the extra header spanning multiple columns
  addtorow <- list()
  addtorow$pos <- list()
  addtorow$pos[[1]] <- -1
  addtorow$pos[[2]] <- nrow(dat)
  age.headers <- colnames(dat)[grep("^a.*", colnames(dat))]
  n.age <- length(age.headers)
  ages <- paste0("\\multicolumn{1}{c}{\\textbf{", 1:(n.age), "}} & ")
  ages.tex <- do.call("paste", as.list(ages))
  ## Remove last ampersand
  ages.tex <- sub("& $", "\\\\\\\\", ages.tex)
  if(fleet == 1 | fleet == 2){
    addtorow$command <- c(paste0("\\hline \\textbf{Year} & \\specialcell{\\textbf{Number}\\\\\\textbf{of fish}} & \\specialcell{\\textbf{Number}\\\\\\textbf{of hauls}} & \\multicolumn{", n.age, "}{c}{\\textbf{Age}} \\\\",
                                 "\\hline & & & ", ages.tex),
                          "\\hline ")
  }else{
    addtorow$command <- c(paste0("\\hline \\textbf{Year} & \\specialcell{\\textbf{Number}\\\\\\textbf{of trips}} & \\multicolumn{", n.age, "}{c}{\\textbf{Age}} \\\\",
                                 "\\hline & & ", ages.tex),
                          "\\hline ")
  }

  ## Make the size string for font and space size
  size.string <- paste0("\\fontsize{",font.size,"}{",space.size,"}\\selectfont")

  return(print(xtable(dat,
                      caption = xcaption,
                      label = xlabel,
                      align = get.align(ncol(dat))),
               caption.placement = "top",
               include.rownames = FALSE,
               include.colnames = FALSE,
               sanitize.text.function = function(x){x},
               size = size.string,
               add.to.row = addtorow,
               table.placement = placement,
               tabular.environment = "tabular",
               hline.after = NULL))
}

make.est.numbers.at.age.table <- function(model,                ## model is an mcmc run and is the output of the r4ss package's function SSgetMCMC
                                          start.yr,             ## start.yr is the first year to show in the table
                                          end.yr,               ## end.yr is the last year to show in the table
                                          weight.factor = 1000, ## divide catches by this factor
                                          plus.group = 15,      ## ages this and older will be grouped in table
                                          xcaption = "default", ## Caption to use
                                          xlabel   = "default", ## Latex label to use
                                          font.size = 9,        ## Size of the font for the table
                                          space.size = 10       ## Size of the spaces for the table
                                          ){
  ## Returns an xtable in the proper format for the main tables section for credibility intervals for biomass,
  ##  relative biomass, recruitment, fishing intensity, and exploitation fraction

  yrs <- start.yr:end.yr

  ## Filter the values by years
  f <- model$natage[model$natage$"Beg/Mid" == "B", -c(1:6,8:11)]
  ## If above line gets changed then change in all.r also.
  f <- f[f$Yr %in% yrs,]
  ## Group ages greater than or equal to plus.group together by summing
  max.age <- max(as.numeric(names(f[,-1])))
  plus.ages <- plus.group:max.age
  f.plus <- f[,names(f) %in% plus.ages]
  f.minus <- f[,!names(f) %in% plus.ages]
  f <- cbind(f.minus, apply(f.plus, 1, sum))
  names(f)[ncol(f)] <- paste0(plus.group, "+")
  names(f)[1] <- "Year"

  ## Apply division by weight factor and formatting
  f[,-1] <- apply(f[-1], c(1,2), function(x) x / weight.factor)
  f[,-1] <- apply(f[-1], c(1,2), f)
  f[,1] <- as.character(f[,1])

  ## Add latex headers
  colnames(f) <- sapply(colnames(f), function(x) paste0("\\textbf{",x,"}"))

  ## Make the size string for font and space size
  size.string <- paste0("\\fontsize{",font.size,"}{",space.size,"}\\selectfont")
  return(print(xtable(f, caption=xcaption, label=xlabel, align=get.align(ncol(f))),
               caption.placement = "top", table.placement="H", include.rownames=FALSE, sanitize.text.function=function(x){x},
               size=size.string))

}
