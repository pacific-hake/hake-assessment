build.doc <- function(knit.only = FALSE,
                      make.pdf  = TRUE,
                      make.bib  = TRUE,
                      doc.name  = "hake-assessment"){
  ## Use this function to build to doc entirely from within R
  ## Make sure you have created the .RData files by sourcing all.r
  ##  with the create.rdata.file variables set to TRUE.
  ## Once you have done that and run this function once within an R session,
  ##  you can go into the first knitr code chunk in hake-assessment.rnw and
  ##  set the call to load.models.into.parent.env() to FALSE,
  ##  which will save time for doing the build.
  ##
  ## knit.only - Only run knitr, not latex
  ## make.pdf - TRUE to make the pdf, if FALSE it will only go as far as
  ##  postscript.

  knit(paste0(doc.name,".rnw"))
  if(!knit.only){
    system(paste0("latex ", doc.name, ".tex"),
           invisible = FALSE,
           show.output.on.console = FALSE)
    if(make.bib){
      system(paste0("bibtex ", doc.name),
             invisible = FALSE,
             show.output.on.console = TRUE)
    }
    system(paste0("latex ", doc.name, ".tex"),
           invisible = FALSE,
           show.output.on.console = FALSE)
    system(paste0("latex ", doc.name, ".tex"),
           invisible = FALSE,
           show.output.on.console = FALSE)
    system(paste0("latex ", doc.name, ".tex"),
           invisible = FALSE,
           show.output.on.console = FALSE)
    system(paste0("dvips ", doc.name,".dvi"),
           invisible = FALSE,
           show.output.on.console = TRUE)
    if(make.pdf){
      shell(paste0("ps2pdf ", doc.name, ".ps"))
    }
  }
}

f <- function(x, dec.points = 0){
  ## Format x to have supplied number of decimal points
  ## Make thousands seperated by commas and the number of decimal points given by
  ##  dec.points
  return(format(round(x,dec.points), big.mark = ",", nsmall = dec.points))
}

export.depth <- function(x, fleet = ""){
  ## Take a data frame that has at least two columns called year and fdep
  ##  convert to meters and calculate boxplot stats on each year
  ##  write a csv file to current directory.
  ## Canada note 2019: Use the files CatchLocationsFT.dat and CatchLocationsSS.dat
  ##  in CumulativeCatch directory as data frames for input to this as x
  ## examples:
  ## export.depth(d.ft, "freezer-trawlers")
  ## export.depth(d.ss, "shoreside")

  x %>%
    transmute(year = as.factor(year), depth = fdep * 1.8288) %>%
    group_by(year) %>%
    do(as.data.frame(t(boxplot.stats(.$depth)$`stats`))) %>%
    ungroup() %>%
    transmute(year,
              lower95 = V1,
              lowerhinge = V2,
              median = V3,
              upperhinge = V4,
              upper95 = V5) %>%
    write.csv(file.path(here::here(),
                        "data",
                        paste0("depth-can-",
                               fleet,
                               ".csv")),
              row.names = FALSE)
}

## -----------------------------------------------------------------------------
## Functions to make table generation easier
## Latex newline
latex.nline <- " \\\\ "
## Horizontal line
latex.hline <- " \\hline "
latex.amp <- function(n = 1){
  ## Returns a string with n ampersands seperated by spaces. The string will
  ##  have one leading and one trailing space.
  paste0(rep(" &", n), " ", collapse = "")
}

latex.paste <- function(vec){
  ## Returns a string comprised of each element in the vector vec with an
  ##  ampersand in between. The string will have one leading and one
  ##  trailing space.
  paste(" ", vec, " ", collapse = " & ")
}

latex.bold <- function(txt){
  ## Returns the given text with the latex \\textbf{} macro around it
  paste0("\\textbf{", txt, "}")
}

latex.italics <- function(txt){
  ## Returns the given text with the latex \\emph{} macro around it
  paste0("\\emph{", txt, "}")
}

latex.under <- function(txt){
  ## Returns the given text with the latex \\underline{} macro around it
  paste0("\\underline{", txt, "}")
}

latex.mlc <- function(latex.vec, make.bold = TRUE){
  ## Returns a string which has been glued together using multi-line-cell
  ##  macro for latex. If make.bold is TRUE, the \textbf macro will be
  ##  inserted.
  if(make.bold){
    latex.vec <- sapply(latex.vec, latex.bold)
  }
  latex.str <- paste(latex.vec, collapse = latex.nline)
  paste0("\\mlc{", latex.str, "}")
}

latex.mcol <- function(ncol, just, txt){
  ## Returns the given text with the latex \\multicolumn{} macro around it
  ## ncol - the number of columns
  ## just - justification, e.g. "l", "c", or "r" for left, center, right
  paste0("\\multicolumn{", ncol, "}{", just, "}{", txt, "}")
}

latex.mrow <- function(nrow, just, txt){
  ## Returns the given text with the latex \\multicolumn{} macro around it
  ## nrow - the number of rows
  ## just - justification, e.g. "l", "c", or "r" for left, center, right
  paste0("\\multirow{", nrow, "}{", just, "}{", txt, "}")
}

latex.size.str <- function(fnt.size, spc.size){
  ## Returns a string which has the given font size and space size applied
  paste0("\\fontsize{", fnt.size, "}{", spc.size, "}\\selectfont")
}

latex.cline <- function(cols){
  ## Draw a horizontal line across the columns specified
  ## cols - a string in this format: "1-3" which means
  ##  the line should go across columns 1 to 3.
  paste0("\\cline{", cols, "}")
}

latex.cmidr <- function(cols, trim = "r"){
  ## Draw a horizontal line across the columns specified
  ## cols - a string in this format: "1-3" which means
  ##  the line should go across columns 1 to 3.
  ## trim - can be l, r, or lr and tells it to trim the
  ##  line a bit so that if there are two lines they don't
  ##  touch in the middle. (See booktabs package)
  paste0("\\cmidrule(", trim, "){", cols, "}")
}

latex.subscr <- function(main.txt, subscr.txt){
  ## Returns a latex string with main.txt subscripted by subscr.txt
  paste0(main.txt, "\\subscr{", subscr.txt, "}")
}

latex.supscr <- function(main.txt, supscr.txt){
  ## Returns a latex string with main.txt superscripted by supscr.txt
  paste0(main.txt, "\\supscr{", supscr.txt, "}")
}

## -----------------------------------------------------------------------------

install.packages.if.needed <- function(package.name,
                                       package.install.name,
                                       github = FALSE){
  if(github){
    if(!(package.name %in% rownames(installed.packages()))){
      devtools::install_github(package.install.name)
    }
  }else{
    if(!(package.name %in% rownames(installed.packages()))){
      install.packages(package.install.name)
    }
  }
}

split.prior.info <- function(prior.str,
                             dec.points = 1,
                             first.to.lower = FALSE){
  ## Get priors information from prior.str which is a string like
  ## Lognormal(2.0,1.01)
  ## Returns a vector of length 3:
  ## "Lognormal", 2.0, 1.01
  ## If first.to.lower = TRUE, makes the first letter of the name of the prior
  ##  lower case.
  p <- strsplit(prior.str, "\\(")[[1]]
  if(first.to.lower){
    ## Make the name of the prior lower case
    p[1] <- paste0(tolower(substr(p[1], 1, 1)),
                   substr(p[1],
                          2,
                          nchar(p[1])))
  }
  p.type <- p[1]
  p <- strsplit(p[2], ",")[[1]]
  p.mean <- f(as.numeric(p[1]), dec.points)
  p.sd <- f(as.numeric(gsub(")", "", p[2])), dec.points)
  return(c(p.type, p.mean, p.sd))
}

cohortCatch <- function(cohort, catage, ages = 0:20, trim.forecast=TRUE) {
  cohort.yrs <- cohort + ages
  caa <- as.matrix(catage[catage$Yr %in% cohort.yrs, as.character(ages)])
  w <- base.model$wtatage
  waa <- w[w$Fleet == 1 & w$Yr %in% cohort.yrs, ]
  waa <- waa[, names(waa) %in% ages]
  catch.waa <- as.matrix(caa * waa)

  ind <- 1:(nrow(caa) + 1)
  if(length(ind) > length(ages)){
    ind <- 1:nrow(caa)
  }
  cohort.catch <- diag(catch.waa[,ind])
  names(cohort.catch) <- cohort.yrs[1:(nrow(caa))]
  if(trim.forecast){
    cohort.catch <- cohort.catch[names(cohort.catch) < end.yr]
  }
  return(cohort.catch)
}

top.coh <- function(yr = last.data.yr,
                    num.cohorts = 3,
                    decimals = 0,
                    cap = TRUE,
                    spec.yr = NA,
                    use.catage = FALSE){
  ## Returns text describing the top N cohorts by year and percentage as a sentence.
  ## egs. top.coh(2018, 2) produces:
  ##  "The 2014 cohort was the largest (29\\%), followed by the 2010 cohort (27\\%)"
  ## top.coh(2018, 2, cap = FALSE) produces:
  ##  "the 2014 cohort was the largest (29\\%), followed by the 2010 cohort (27\\%)"
  ## top.coh(2018, spec.yr = 2010) produces:
  ##  "27"
  ##  because 27% of the catch in 2018 were of the 2010 cohort
  ## If spec.yr is a year, then the value only will be returned
  ##  as a percentage of that cohort caught in yr
  ## If use.catage is TRUE, use the base.model$catage object which are the estimates
  ## If use.catage is FALSE, use the base.model$dat$agecomp object which are the data
  if(num.cohorts < 1){
    num.cohorts = 1
  }
  if(use.catage){
    tmp <- base.model$catage[, -c(1, 3, 4, 5, 6)] %>%
      dplyr::filter(Fleet == 1) %>%
      select(-c(Fleet, Seas, XX, Era, 0))
    tmp <- tmp[-1,]
  }else{
    tmp <- base.model$dat$agecomp[, -c(2, 4, 5, 6, 7, 8, 9)] %>%
      dplyr::filter(FltSvy == 1) %>%
      select(-FltSvy) %>%
      mutate_all(funs(as.numeric))
    names(tmp) <- gsub("^a", "", names(tmp))
  }
  row.sums <- rowSums(select(tmp, -Yr))
  x <- tmp %>%
    select(-Yr) %>%
    mutate_all(~ ./row.sums)
  x <- cbind(Yr = tmp$Yr, x) %>%
    dplyr::filter(Yr == yr) %>%
    select(-Yr) %>%
    sort() %>%
    rev()
  txt <- paste0(ifelse(cap, "The ", "the "),
                yr - as.numeric(names(x)[1]),
                " cohort was the largest (",
                f(x[1] * 100, decimals),
                "\\%)")
  if(num.cohorts > 1){
    for(i in 2:num.cohorts){
      txt <- paste0(txt,
                    ", followed by the ",
                    yr - as.numeric(names(x)[i]),
                    " cohort (",
                    f(x[i] * 100, decimals),
                    "\\%)")
    }
  }
  if(!is.na(spec.yr)){
    return(f(as.numeric(x[names(x) == yr - spec.yr]) * 100, decimals))
  }
  txt
}

get.age.prop <- function(vec, place = 1){
  ## returns the age prop and the age itself for the place,
  ## where place is 1=max, 2-second highest, etc.
  prop <- rev(sort(vec))
  prop <- prop[place]
  age <- as.numeric(names(vec[vec == prop]))
  return(c(age, prop))
}

get.shade <- function(color, opacity){
  # If color is a single R color string or single number,
  #  returns an rgb string of the specified color and opacity
  # If color is a vector of cR color strings or numbers,
  #  returns a vector of rgb strings of the specified color and opacity.
  # If the opacity argument is non-integer or not between 0 and 99, NULL will be returned.
  # - opacity - 2-decimal-digit string (00-99), i.e. "20" means 20%
  # Notes: format of returned string is #RRGGBBAA
  #        where RR=red, a 2-hexadecimal-digit string
  #        GG=green, a 2-hexadecimal-digit string
  #        BB=blue, a 2-hexadecimal-digit string
  #        AA=alpha or opacity
  #
  # The opacity agrument is scalar and will be applied to all colors.
  if(!(opacity %% 1 == 0) || opacity<0 || opacity>99){
    cat0(.PROJECT_NAME,"->",currFuncName,"opacity argument must be an integer between 0 and 99.")
    return(NULL)
  }
  colorDEC <- col2rgb(color)
  if(is.matrix(colorDEC)){
    colorHEX <- matrix(nrow=3,ncol=ncol(colorDEC))
    shade <- NULL
    for(col in 1:ncol(colorDEC)){
      for(row in 1:nrow(colorDEC)){
        colorHEX[row,col] <- sprintf("%X", colorDEC[row,col])
        if(nchar(colorHEX[row,col])==1){
          colorHEX[row,col] <- paste0("0",colorHEX[row,col])
        }
      }
      shade[col] <- paste0("#",colorHEX[1,col],colorHEX[2,col],colorHEX[3,col],opacity)
    }
  }else{
    colorHEX <- sprintf("%X", colorDEC)
    for(i in 1:length(colorHEX)){
      if(nchar(colorHEX[i])==1){
        colorHEX[i] <- paste0("0",colorHEX[i])
      }
    }
    shade <- paste0("#",colorHEX[1],colorHEX[2],colorHEX[3],opacity)
  }
  return(shade)
}

remove.all.objects.except <- function(vars){
  # Removes every object in the workspace except for what is in the vars list.
  # Upon finishing, the workspace will contain whatever is in the vars list,
  #  plus the object 'remove.all.objects.except' (this function)

  vars <- c(vars, "remove.all.objects.except")
  keep <- match(x = vars, table = ls(all = TRUE, envir = .GlobalEnv))
  if(!any(is.na(keep))){
    rm(list=ls(all = TRUE, envir = .GlobalEnv)[-keep], envir = .GlobalEnv)
  }
}

pad.num <- function(num, digits = 0){
  ## Takes an integer, num and turns it into a string
  ## If the string is less than digits long, it will
  ## be prepended with zeroes
  if(digits < 1) stop("Error in pad.num - digits must be positive\n")
  sapply(num, function(x){ paste0(rep("0", digits - nchar(as.character(x))), as.character(x))})
}

t.pn <- function(){
  ## test pad.num
  cat("pad.num(0, 1) = ", pad.num(0, 1), "\n")
  cat("pad.num(1, 2) = ", pad.num(1, 2), "\n")
  cat("pad.num(10, 2) = ", pad.num(10, 2), "\n")
  cat("pad.num(10, 3) = ", pad.num(10, 3), "\n")
  cat("pad.num(10, 0) = ", pad.num(10, 0), "\n")
}

print.model.message <- function(model.dir.names, model.names, group, model.type){
  ## Print out a message stating the model directory names and pretty names,
  ##  for the group number given. If bridge is TRUE, it is a bridge model group,
  ##  if bridge is FALSE, it is a sensitivity model group.

  cat0("***")
  cat0(model.type, " model group ", group, " directories: ")
  cat(paste0("  ", model.dir.names), sep = "\n")
  cat0(model.type, " model group ", group, " pretty names: ")
  cat(paste0("  ", model.names), sep = "\n")
  cat0("***")
}

curr.fn.finder <- function(skipframes = 0,
                           skipnames = "(FUN)|(.+apply)|(replicate)",
                           ret.if.none = "Not in function",
                           ret.stack = FALSE,
                           extra.perf.per.level = "\t"){
  ## Get the current function name from within the function itself.
  ## Used to prepend the function name to all messages so that the
  ## user knows where the message came from.
  prefix <- sapply(3 + skipframes + 1:sys.nframe(), function(i){
    currv <- sys.call(sys.parent(n = i))[[1]]
    return(currv)
  })
  prefix[grep(skipnames, prefix)] <- NULL
  prefix <- gsub("function \\(.*", "do.call", prefix)
  if(length(prefix)==0){
    return(ret.if.none)
  }else if(ret.stack){
    return(paste(rev(prefix), collapse = "|"))
  }else{
    retval <- as.character(unlist(prefix[1]))
    if(length(prefix) > 1){
      retval <- paste0(paste(rep(extra.perf.per.level, length(prefix) - 1), collapse = ""), retval)
    }
    return(retval)
  }
}

get.curr.func.name <- function(){
  ## Returns the calling function's name followed by ": "
  func.name <- curr.fn.finder(skipframes = 1) # skipframes=1 is there to avoid returning getCurrFunc itself
  ## Strip extraneous whitespace
  func.name <- gsub("\t+", "", func.name)
  func.name <- gsub("\ +", "", func.name)
  func.name <- paste0(func.name,": ")
  return(func.name)
}

cat0 <- function(...){
  ## Wrapper function to make cat have no space and insert a newline at the end.
  ## Inspired by the paste0 function.
  cat(..., "\n", sep = "")
}

number.to.word <- function(x, th = FALSE, cap.first = FALSE){
  ## https://github.com/ateucher/useful_code/blob/master/R/numbers2words.r
  ## Function by John Fox found here:
  ## http://tolstoy.newcastle.edu.au/R/help/05/04/2715.html
  ## Tweaks by AJH to add commas and "and"
  ## if th is TRUE, the th version will be returned, e.g. 4 = fourth
  ## if cap.first is TRUE, the first letter will be capitalized
  helper <- function(x){
    digits <- rev(strsplit(as.character(x), "")[[1]])
    nDigits <- length(digits)
    if(nDigits == 1) as.vector(ones[digits])
    else if(nDigits == 2)
      if(x <= 19) as.vector(teens[digits[1]])
      else trim(paste(tens[digits[2]],
                      Recall(as.numeric(digits[1]))))
    else if (nDigits == 3) trim(paste(ones[digits[3]], "hundred and",
                                      Recall(makeNumber(digits[2:1]))))
    else {
      nSuffix <- ((nDigits + 2) %/% 3) - 1
      if (nSuffix > length(suffixes)) stop(paste(x, "is too large!"))
      trim(paste(Recall(makeNumber(digits[
        nDigits:(3*nSuffix + 1)])),
        suffixes[nSuffix],"," ,
        Recall(makeNumber(digits[(3*nSuffix):1]))))
    }
  }
  trim <- function(text){
    ## Tidy leading/trailing whitespace, space before comma
    text=gsub("^\ ", "", gsub("\ *$", "", gsub("\ ,",",",text)))
    ## Clear any trailing " and"
    text=gsub(" and$","",text)
    ##Clear any trailing comma
    gsub("\ *,$","",text)
  }
  makeNumber <- function(...) as.numeric(paste(..., collapse=""))
  ## Disable scientific notation
  opts <- options(scipen=100)
  on.exit(options(opts))
  ones <- c("", "one", "two", "three", "four", "five", "six", "seven",
            "eight", "nine")
  names(ones) <- 0:9
  teens <- c("ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen",
             "sixteen", " seventeen", "eighteen", "nineteen")
  names(teens) <- 0:9
  tens <- c("twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty",
            "ninety")
  names(tens) <- 2:9
  x <- round(x)
  suffixes <- c("thousand", "million", "billion", "trillion")
  if (length(x) > 1) return(trim(sapply(x, helper)))
  j <- helper(x)
  ## Cgrandin added the 'th' bit
  if(th){
    j <- strsplit(j, " ")[[1]]
    first <- j[-length(j)]
    last <- j[length(j)]
    if(last == "one"){
      last <- "first"
    }else if(last == "two"){
      last <- "second"
    }else if(last == "three"){
      last <- "third"
    }else if(last == "five"){
      last <- "fifth"
    }else if(last == "eight"){
      last <- "eighth"
    }else if(last == "nine"){
      last <- "ninth"
    }else if(last == "twelve"){
      last <- "twelfth"
    }else if(last == "twenty"){
      last <- "twentieth"
    }else if(last == "thirty"){
      last <- "thirtieth"
    }else if(last == "forty"){
      last <- "fortieth"
    }else if(last == "fifty"){
      last <- "fiftieth"
    }else if(last == "sixty"){
      last <- "sixtieth"
    }else if(last == "seventy"){
      last <- "seventieth"
    }else if(last == "eighty"){
      last <- "eightieth"
    }else if(last == "ninety"){
      last <- "ninetieth"
    }else{
      last <- paste0(last, "th")
    }
    j <- paste(c(first, last), collapse = " ")
  }
  if(cap.first){
    j <- paste0(toupper(substr(j, 1, 1)), substr(j, 2, nchar(j)))
  }
  return(j)
}

## *****************************************************************************
## The following three functions give the ability to assign more than one variable at once.
## Example Call;  Note the use of set.elems()  AND  `%=%`
## Right-hand side can be a list or vector
## set.elems(a, b, c)  %=%  list("hello", 123, list("apples, oranges"))
## set.elems(d, e, f) %=%  101:103
## # Results:
## > a
## [1] "hello"
## > b
## [1] 123
## > c
## [[1]]
## [1] "apples, oranges"
## > d
## [1] 101
## > e
## [1] 102
## > f
## [1] 103

## Generic form
"%=%" <- function(l, r, ...) UseMethod("%=%")

## Binary Operator
"%=%.lhs" <- function(l, r, ...) {
  env <- as.environment(-1)
  if (length(r) > length(l))
    warning("RHS has more args than LHS. Only first", length(l), "used.")
  if (length(l) > length(r))  {
    warning("LHS has more args than RHS. RHS will be repeated.")
    r <- extend.to.match(r, l)
  }
  for(II in 1:length(l)) {
    do.call('<-', list(l[[II]], r[[II]]), envir = env)
  }
}

## Used if LHS is larger than RHS
extend.to.match <- function(src, destin) {
  s <- length(src)
  d <- length(destin)
  # Assume that destin is a length when it is a single number and src is not
  if(d==1 && s>1 && !is.null(as.numeric(destin)))
    d <- destin
  dif <- d - s
  if (dif > 0) {
    src <- rep(src, ceiling(d/s))[1:d]
  }
  return (src)
}

set.elems <- function(...) {
  list.tmp <-  as.list(substitute(list(...)))[-1L]
  class(list.tmp) <-  "lhs"
  return(list.tmp)
}
## *****************************************************************************

cbind.fill <- function(...){
  ## equivalent of cbind(df, xx) where df is an empty data frame.
  nm <- list(...)
  nm <- lapply(nm, as.matrix)
  n <- max(sapply(nm, nrow))
  do.call(cbind, lapply(nm, function (x)
    rbind(x, matrix(, n-nrow(x), ncol(x)))))
}

strip.columns <- function(vec, names){
  ## Return a vector which is the same as the vector 'vec'
  ## but with the matching col.names removed
  return(vec[!names(vec) %in% names])
}

get.align <- function(num,
                      first.left = TRUE, ## Keep the first column left-justified
                                         ## If FALSE, it will be justified according to the 'just' argument
                      just = "r"         ## just is the justification to use for the columns, "r", "l", or "c"
                      ){
  ## Returns a character vector used in the align argument of the xtable command.
  ## e.g. posterior output tables, reference point tables. Most tables really.
  ## num is the number of columns in the table
  if(first.left){
    align <- c("l", "l")
  }else{
    align <- c(just, just)
  }
  for(i in 1:(num-1)){
    align <- c(align, just)
  }
  return(align)
}

rc <- rich.colors.short <- function(n, alpha = 1){
  x <- seq(0, 1, length = n)
  r <- 1/(1 + exp(20 - 35 * x))
  g <- pmin(pmax(0, -0.8 + 6 * x - 5 * x^2), 1)
  b <- dnorm(x, 0.25, 0.15)/max(dnorm(x, 0.25, 0.15))
  rgb.m <- matrix(c(r, g, b), ncol = 3)
  rich.vector <- apply(rgb.m, 1, function(v) rgb(v[1],v[2],v[3],alpha = alpha))
}

plotBars.fn <- function(x,y,gap=0,add=F,ciCol="black",ciLty=1,ciLwd=1,...) {
  ## x is the x axis values (i.e., years)
  ## y is a data frame with:
  ## value: estimate (point) to plot
  ## lo: lower CI
  ## hi: higher CI

  if(!add) plot(x,y$value,...)
  if(add) points(x,y$value,...)
  segments(x,y$lo,x,y$value-gap,col=ciCol,lty=ciLty,lwd=ciLwd)
  segments(x,y$hi,x,y$value+gap,col=ciCol,lty=ciLty,lwd=ciLwd)
}

plotBars.fn <- function(x,y,gap=0,scalar=1e6,add=F,ciCol="black",ciLty=1,ciLwd=1,...) {
  ## x is the x axis values (i.e., years)
  ## y is a data frame with:
  ## value: estimate (point) to plot
  ## lo: lower CI
  ## hi: higher CI

  if(!add) plot(x,y$value/scalar,...)
  if(add) points(x,y$value/scalar,...)
  segments(x,y$lo/scalar,x,y$value/scalar-gap,col=ciCol,lty=ciLty,lwd=ciLwd)
  segments(x,y$hi/scalar,x,y$value/scalar+gap,col=ciCol,lty=ciLty,lwd=ciLwd)
}

panel.letter <- function(letter){
  # adds letters to plot panels
  # letter is the letter to place on the panel
  usr <- par("usr")
  inset.x <- 0.05*(usr[2]-usr[1])
  inset.y <- 0.05*(usr[4]-usr[3])
  text(usr[1]+inset.x,usr[4]-inset.y,paste("(",letter,")",sep=""),cex=1.,font=1)
}

addpoly <- function(yrvec, lower, upper, color = 1, shade.col = NULL){
  lower[lower<0] <- 0 ## max of value or 0
  if(is.null(shade.col)){
    shade.col <- rgb(t(col2rgb(color)), alpha = 0.2 * 255, maxColorValue = 255)
  }
  polygon(x = c(yrvec, rev(yrvec)),
          y = c(lower, rev(upper)),
          border = NA,
          col = shade.col)
  lines(yrvec, lower, lty = 3, col = color)
  lines(yrvec, upper, lty = 3, col = color)
}

randWalkSelex.fn <- function(pars,devs=NULL,Phi=1.4,transform=FALSE,bounds=NULL) {
  ## calculates the selectivity from the random walk parameters in SS (option 17)
  ## -1000 means to set equal to 0
  ## assumes that this is all pars from age 0 to max age

  logS <- rep(NA,length(pars))
  logS[1] <- 0 #first value is never estimated (age 0)
  if(!is.null(devs)) {
    ## transform parameters based on bounds
    for(a in 2:length(pars)) {
      if(!is.na(devs[a])) {
        ## transformation was present in 2014-2017 models but no longer used in 2018
        if(transform){
          tmp <- log((bounds[2]-bounds[1]+0.0000002)/(pars[a]-bounds[1]+0.0000001)-1)/(-2)
          tmp <- tmp + devs[a]
          pars[a] <- bounds[1]+(bounds[2]-bounds[1])/(1+exp(-2*tmp))
        }else{
          ## in 3.30, there's no transformation, but the devs are scaled by the SE (Phi)
          pars[a] <- pars[a] + Phi*devs[a]
        }
      }
    }
  }
  for(a in 2:length(pars)) {
    ifelse(pars[a] == -1000, logS[a] <- 0, logS[a] <- logS[a-1]+pars[a])
  }

  selex <- exp(logS-max(logS))
  selex[pars== -1000] <- 0
  return(selex)
}

selexYear.fn <- function(x, yr, bnds=c(-5,9)) {
  ## get selectivity for a given year from all MCMC samples

  ## specific for hake 2013 and 2014
  ## updated 2017/01/25 to not give error when year value is outside range available

  # define mostly-empty matrix to store selectivity parameters for each mcmc sample
  selexPars <- matrix(c(-1000, 0, NA, NA, NA, NA, NA, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                      nrow = nrow(x), ncol = 16, byrow = TRUE)
  # define matrix to store deviation parameters for given year from each mcmc
  devsPars  <- matrix(NA, ncol = ncol(selexPars), nrow = nrow(x))

  ## columns of MCMC output which match names for base parameters
  tmp <- grep("AgeSel_P[1-9]_Fishery.1.", names(x))
  ## columns of MCMC output which match names for deviation parameters
  devsInd <- grep("AgeSel_P[1-9]_Fishery.1._DEVadd_[1-9]+", names(x))
  ## get all deviation parameters
  allDevsPars <- x[,devsInd]
  ## fill in matrix of selectivity parameters
  selexPars[,3:7] <- as.matrix(x[,tmp[!(tmp %in% devsInd)]])
  ## get column indices associated with deviation parameters
  devsInd <- grep(as.character(yr), names(x)[devsInd])
  if(length(devsInd)==0){
    ## if year not found in names of deviation parameters, return NULL
    return(NULL)
  }
  devsPars[,3:7] <- as.matrix(allDevsPars[,devsInd])

  ## define empty matrix to store resulting selectivity
  selex <- matrix(NA,ncol=ncol(selexPars),nrow=nrow(x))
  ## for each year, combine base selectivity parameters and deviations to get selex
  for(i in 1:nrow(selexPars)) {
    selex[i,] <- randWalkSelex.fn(selexPars[i,],devsPars[i,],bounds=bnds)
  }
  return(selex)
}

selexYear10.fn <- function(x,yr,bnds=c(-5,9)) {
  ## specific for hake 2013 and 2014
  selexPars <- matrix(c(-1000,0,NA,NA,NA,NA,NA,NA,NA,NA,NA,0,0,0,0,0),nrow=nrow(x),ncol=16,byrow=T)
  devsPars  <- matrix(NA,ncol=ncol(selexPars),nrow=nrow(x))

  tmp <- grep("AgeSel_1P_[0-9]+_Fishery",names(x))
  devsInd <- grep("AgeSel_1P_[0-9]+_Fishery_DEVadd",names(x))
  allDevsPars <- x[,devsInd]
  selexPars[,3:11] <- as.matrix(x[,tmp[!(tmp %in% devsInd)]])
  devsInd <- grep(as.character(yr),names(x)[devsInd])
  devsPars[,3:11] <- as.matrix(allDevsPars[,devsInd])

  selex <- matrix(NA,ncol=ncol(selexPars),nrow=nrow(x))
  for(i in 1:nrow(selexPars)) {
    selex[i,] <- randWalkSelex.fn(selexPars[i,],devsPars[i,],bounds=bnds)
  }

  return(selex)
}

biomass_fraction_plots <- function(replist, selected=FALSE){
  ## biomass fraction of ages 4+
  ## get weight at age
  wtatage <- replist$wtatage[replist$wtatage$fleet==1,-(2:6)]
  ## make years positive
  names(wtatage)[1] <- "Yr" # avoid annoying mix of 'yr' and 'Yr'
  wtatage$Yr <- abs(wtatage$Yr)
  ## get equilibrium value (mean across years for hake)
  wtatage.mean <- wtatage[1,]
  ## get numbers at age
  natage <- replist$natage[replist$natage$"Beg/Mid"=="B",-c(1:6,8:11)]
  ## fill in missing years in weight at age with equilibrium value
  for(y in setdiff(natage$Yr, wtatage$Yr)){
    tmp <- wtatage.mean
    tmp$Yr <- y
    wtatage <- rbind(wtatage,tmp)
  }
  wtatage <- wtatage[order(wtatage$Yr),]
  Yrs <- rownames(natage) <- natage$Yr
  ## get fishery selectivity (method would differ if it were time-varying)
  sel <- replist$ageselex[replist$ageselex$fleet==1 &
                          replist$ageselex$factor=="Asel",c("year",paste(0:20))]
  if(nrow(sel)<10){
    ## if not time-varying, repeat vector for all years
    sel <- matrix(as.numeric(sel[1,-1]),ncol=ncol(sel)-1,nrow=length(Yrs),byrow=TRUE)
  }else{
    ## if time-varying, fiddle with years to make them match the numbers at age
    sel.init <- sel[sel$year==1963,]
    sel <- sel[sel$year!=1963,]
    for(y in 1965:1964){
      sel.init$year <- y
      sel <- rbind(sel.init, sel)
    }
    sel.2014 <- sel[sel$year==2014,]
    for(y in 2015:2016){
      sel.2014$year <- y
      sel <- rbind(sel, sel.2014)
    }
    if(any(sel$year!=Yrs)){
      stop("problem with selectivity")
    }else{
      sel <- sel[,-1]
    }
  }

  ## calculate biomass at age
  batage <- natage[,-1]*wtatage[,-1]
  ## selected biomass at age
  batage.sel <- batage*sel

  if(selected){
    B0plus <- as.numeric(apply(batage.sel, 1, sum))
    B4plus <- as.numeric(apply(batage.sel[, 0:20 >= 4], 1, sum))
    B5plus <- as.numeric(apply(batage.sel[, 0:20 >= 5], 1, sum))
  }else{
    B0plus <- as.numeric(apply(batage, 1, sum))
    B4plus <- as.numeric(apply(batage[, 0:20 >= 4], 1, sum))
    B5plus <- as.numeric(apply(batage[, 0:20 >= 5], 1, sum))
  }

  ## define time-periods
  par(mfrow=c(2,1))
  main.yrs <- Yrs %in% 1966:2014
  fore.yrs <- Yrs >= 2014

  ## plot timeseries of biomass
  plot(0, type='n', xlim=range(Yrs), ylim=c(0,ceiling(max(B0plus/1e6))), yaxs='i', las=1,
       xlab='Year', ylab='Biomass (millions of mt)')
  lines(Yrs[main.yrs], B0plus[main.yrs]/1e6, lwd=3, col=1, lty=1)
  lines(Yrs[main.yrs], B4plus[main.yrs]/1e6, lwd=3, col=2, lty=1)
  lines(Yrs[main.yrs], B5plus[main.yrs]/1e6, lwd=3, col=4, lty=1)
  lines(Yrs[fore.yrs], B0plus[fore.yrs]/1e6, lwd=2, col=1, lty='12')
  lines(Yrs[fore.yrs], B4plus[fore.yrs]/1e6, lwd=2, col=2, lty='12')
  lines(Yrs[fore.yrs], B5plus[fore.yrs]/1e6, lwd=2, col=4, lty='12')
  points(Yrs[1], B0plus[1]/1e6,lwd=3)
  points(Yrs[1], B4plus[1]/1e6,col=2,lwd=3)
  points(Yrs[1], B5plus[1]/1e6,col=4,lwd=3)
  axis(1, at=1964, lab="Equilibrium", cex.axis=.8)
  axis(1, at=2014)
  abline(v=c(1964,seq(1970,2010,10),2014),lty=3,col='grey')
  abline(h=1:5,lty=3,col='grey')
  legend('bottomleft', lwd=3, col=c(1,2,4), ncol=3,
         legend=c("All ages","Ages 4+","Ages 5+"),bg='white')
  title(main=ifelse(selected, "Estimated selected biomass", "Estimated total biomass"))

  ## plot timeseries of fractions of biomass
  plot(0, type='n', xlim=range(Yrs), ylim=c(0,1), yaxs='i', las=1,
       xlab='Year', ylab='Fraction of biomass')
  lines(Yrs[main.yrs], B4plus[main.yrs]/B0plus[main.yrs], lwd=3, col=2)
  lines(Yrs[main.yrs], B5plus[main.yrs]/B0plus[main.yrs], lwd=3, col=4)
  lines(Yrs[fore.yrs], B4plus[fore.yrs]/B0plus[fore.yrs], lwd=2, col=2, lty='11')
  lines(Yrs[fore.yrs], B5plus[fore.yrs]/B0plus[fore.yrs], lwd=2, col=4, lty='11')
  points(Yrs[1], B4plus[1]/B0plus[1], lwd=3, col=2)
  points(Yrs[1], B5plus[1]/B0plus[1], lwd=3, col=4)
  axis(1, at=1964, lab="Equilibrium", cex.axis=.8)
  axis(1, at=2014)
  abline(v=c(1964,seq(1970,2010,10),2014),lty=3,col='grey')
  abline(h=seq(0,0.8,.2),lty=3,col='grey')
  legend('bottomleft', lwd=3, col=c(2,4), ncol=2,
         legend=c("Ages 4+","Ages 5+"),bg='white')
  title(main=ifelse(selected, "Estimated fractions of selected biomass", "Estimated fractions of total biomass"))
}

mcmc.out <- function (directory = "c:/mydirectory/", run = "mymodel/", file = "keyposteriors.csv",
    namefile = "postplotnames.sso", names = FALSE, headernames = TRUE,
    numparams = 1, closeall = TRUE, burn = 0, thin = 1, scatter = FALSE,
    surface = FALSE, surf1 = 1, surf2 = 2, stats = FALSE, plots = TRUE,
    header = TRUE, sep = ",", print = FALSE, new = T, colNames = NULL)
{
    if (print == TRUE) {
    }
    if (closeall == TRUE) {
    }
    filename <- file.path(directory, run, file)
    if (!file.exists(filename)) {
        stop("file doesn't exist:\n", filename)
    }
    mcmcdata <- read.table(filename, header = header, sep = sep,
        fill = TRUE)

    if (names == TRUE) {
        nameout <- file.path(directory, run, namefile)
        namedata <- read.table(nameout, header = FALSE, sep = "",
            colClasses = "character", fill = TRUE)
        numparams <- as.numeric(namedata[1, 1])
        for (j in 1:numparams) {
            names(mcmcdata)[j] <- namedata[(j + 1), 1]
        }
    }
    if (!is.null(colNames)) {
        if (length(colNames) != numparams)
            cat("numparams argument overidden by length of colNames argument\n")
        numparams <- length(colNames)

        mcmcdata <- mcmcdata[, colNames]
        if (length(colNames) == 1) {
            mcmcdata <- data.frame(mcmcdata)
            names(mcmcdata) <- colNames
        }
    }
    mcmcfirst <- mcmc(mcmcdata)
    mcmctemp <- window(mcmcfirst, thin = thin, start = (1 + burn))
    mcthinned <- as.matrix(mcmctemp)
    mcmcobject <- mcmc(mcthinned)
    draws <- length(mcmcobject[, 1])
    if (plots == TRUE) {
        if (new)
            dev.new(record = TRUE)
        if (numparams == 5 || numparams == 9 || numparams ==
            13 || numparams == 17) {
            plot(0, 0, xlab = "", ylab = "", frame.plot = FALSE,
                yaxt = "n", xaxt = "n", type = "n")
        }
        for (i in 1:numparams) {
            par(new = FALSE, mfrow = c(2, 2), ann = TRUE)
            traceplot(mcmcobject[, i], smooth = TRUE)
            mtext("Value", side = 2, line = 3, font = 1, cex = 0.8)
            if (names | headernames) {
                mtext(names(mcmcdata)[i], side = 3, adj = 0,
                  line = 2, font = 2, cex = 1)
            }
            else {
                mtext(paste("param", i), side = 3, adj = 0, line = 2,
                  font = 2, cex = 1)
            }
            lowest <- min(mcmcobject[, i])
            highest <- max(mcmcobject[, i])
            plot(c(seq(1, draws, by = 1)), c(lowest, rep(c(highest),
                (draws - 1))), xlab = "Iterations", ylab = "",
                yaxt = "n", type = "n")
            if (!exists("running")) {
                cat("skipping running average section because function 'running' is needed\n")
            }
            else {
                lines(running(mcmcobject[, i], fun = median,
                  allow.fewer = TRUE, width = draws))
                fun <- function(x, prob) quantile(x, probs = prob,
                  names = FALSE)
                lines(running(mcmcobject[, i], fun = fun, prob = 0.05,
                  allow.fewer = TRUE, width = draws), col = "GREY")
                lines(running(mcmcobject[, i], fun = fun, prob = 0.95,
                  allow.fewer = TRUE, width = draws), col = "GREY")
            }
            par(ann = FALSE)
            autocorr.plot(mcmcobject[, i], auto.layout = FALSE,
                lag.max = 20, ask = FALSE)
            mtext("Autocorrelation", side = 2, line = 3, font = 1,
                cex = 0.8)
            mtext("Lag", side = 1, line = 3, font = 1, cex = 0.8)
            lines(seq(1, 20, by = 1), rep(0.1, 20), col = "GREY")
            lines(seq(1, 20, by = 1), rep(-0.1, 20), col = "GREY")
            densplot(mcmcobject[, i], show.obs = TRUE)
            mtext("Density", side = 2, line = 3, font = 1, cex = 0.8)
            mtext("Value", side = 1, line = 3, font = 1, cex = 0.8)
        }
    }
    if (stats == TRUE) {
        dev.new()
        par(mar = c(0, 0, 3, 0))
        plot(0, ylab = "", xlab = "", type = "n", xlim = c(0,
            25), ylim = c(0, 25), main = "Summary statistics for key parameters",
            axes = FALSE)
        text(0.001, 25, "Parameter", font = 2, cex = 0.9, adj = 0)
        text(4, 25, "Median (0.05-0.95)", font = 2, cex = 0.9,
            adj = 0)
        text(13, 25, "AC Lag 1", font = 2, cex = 0.9, adj = 0)
        text(16.5, 25, "Eff. N", font = 2, cex = 0.9, adj = 0)
        text(19, 25, "Geweke-Z", font = 2, cex = 0.9, adj = 0)
        text(22.5, 25, "Heidel-W", font = 2, cex = 0.9, adj = 0)
        for (i in 1:numparams) {
            text(0, (25 - i), paste("param", i), font = 1, cex = 0.9,
                adj = 0)
            med <- quantile(mcmcobject[, i], probs = 0.5, names = FALSE)
            range <- quantile(mcmcobject[, i], probs = c(0.05,
                0.95), names = FALSE)
            text(3.2, 25 - i, paste(signif(round(med, 6), 6),
                "(", paste(signif(round(range[1], 6), 6), "-",
                  signif(round(range[2], 6), 6)), ")"), font = 1,
                cex = 0.9, adj = 0)
            l1.ac <- acf(mcmcobject[, i], lag.max = 1, type = "correlation",
                plot = F)
            acoruse <- round(l1.ac$acf[2], 6)
            text(13, 25 - i, acoruse, font = 1, cex = 0.9, adj = 0)
            effsize <- effectiveSize(mcmcobject[, i])
            text(16.5, 25 - i, round(min(effsize, draws), 0),
                font = 1, cex = 0.9, adj = 0)
            if (acoruse > 0.4) {
                gewuse <- "None"
            }
            if (acoruse <= 0.4) {
                geweke <- geweke.diag(mcmcobject[, i], frac1 = 0.1,
                  frac2 = 0.5)
                gewuse <- round(geweke$z, 3)
            }
            text(19, 25 - i, gewuse, font = 1, cex = 0.9, adj = 0)
            if (acoruse > 0.4) {
                send <- "None"
            }
            if (acoruse <= 0.4) {
                hw <- as.list(heidel.diag(mcmcobject[, i], pvalue = 0.05))
                if (hw[1] == 0) {
                  send <- "Failed"
                }
                if (hw[1] == 1) {
                  send <- "Passed"
                }
            }
            text(22.5, 25 - i, send, font = 1, cex = 0.9, adj = 0)
        }
    }
    if (scatter == TRUE) {
        dev.new()
        par(xaxt = "n", yaxt = "n")
        pairs(mcmcdata[1:numparams], cex = 0.1, gap = 0)
    }
    if (surface == TRUE) {
        dev.new()
        par(new = FALSE)
        hist2d(mcmcobject[, surf1], mcmcobject[, surf2], nbins = 100,
            na.rm = TRUE, xlab = paste("parameter", surf1), ylab = paste("parameter",
                surf2), show = TRUE, col = c("GREY", topo.colors(20)))
    }
}

# Multiply the weight-at-age by the maturity vector and reproduce output to match SS wtatage file format
# Writes output to a text file for easy insertion back into the wtatage file
fec.wtatage <- function(d = tibble::as.tibble(readClipboard()),
                        mat = c(0.000,
                                0.000,
                                0.261,
                                0.839,
                                0.961,
                                0.920,
                                0.928,
                                0.926,
                                0.957,
                                0.944,
                                0.980,
                                0.962,
                                1.000,
                                0.958,
                                0.955,
                                0.900,
                                0.900,
                                0.900,
                                0.900,
                                0.900,
                                0.900)){
    k <- apply(d, 1, function(x){j=strsplit(as.character(x),"\\s+")[[1]];j})
    pre <- t(k[1:7,])
    j <- apply(k, c(1,2), as.numeric)
    x <- j[-(1:7),]
    y <- t(x * mat)
    pre[,2] <- paste0(" ", pre[,2])
    pre[,3] <- paste0("    ", pre[,3])
    pre[,4] <- paste0("      ", pre[,4])
    pre[,5] <- paste0("  ", pre[,5])
    pre[,6] <- paste0("     ", pre[,6])
    pre[,7] <- "    -2"
    y <- f(y, 4)
    z <- cbind(pre, y)
    write.table(z, "wtatage.txt", quote = FALSE, row.names = FALSE, col.names = FALSE)
}

# Calculate the average values for the weights-at-age as pasted from an SS wtatage file.
# Writes output to a text file for easy insertion back into the wtatage file
# Written in 2019 for use in fecundity/weight-at-age changes
avg.wtatage <- function(d = tibble::as.tibble(readClipboard()),
                        year = 2018){
  k <- apply(d, 1, function(x){j=strsplit(as.character(x),"\\s+")[[1]];j})
  pre <- t(k[1:7,])[1,]
  pre[2] <- year
  pre[2] <- paste0(" ", pre[2])
  pre[3] <- paste0("    ", pre[3])
  pre[4] <- paste0("      ", pre[4])
  pre[5] <- paste0("  ", pre[5])
  pre[6] <- paste0("     ", pre[6])
  j <- apply(k, c(1,2), as.numeric)
  x <- j[-(1:7),]
  y <- t(x)
  z <- t((c(pre, f(apply(y, 2, mean), 4))))
  write.table(z, "avg-wtatage.txt", quote = FALSE, row.names = FALSE, col.names = FALSE)
}

curfnfinder <- function(skipframes=0, skipnames="(FUN)|(.+apply)|(replicate)",
    retIfNone="Not in function", retStack=FALSE, extraPrefPerLevel="\t")
{
  # Get the current function name from within the function itself.
  # Used to prepend the function name to all messages so that the
  # user knows where the message came from.
    prefix<-sapply(3 + skipframes+1:sys.nframe(), function(i){
            currv<-sys.call(sys.parent(n=i))[[1]]
            return(currv)
        })
    prefix[grep(skipnames, prefix)] <- NULL
    prefix<-gsub("function \\(.*", "do.call", prefix)
    if(length(prefix)==0)
    {
        return(retIfNone)
    }
    else if(retStack)
    {
        return(paste(rev(prefix), collapse = "|"))
    }
    else
    {
        retval<-as.character(unlist(prefix[1]))
        if(length(prefix) > 1)
        {
            retval<-paste(paste(rep(extraPrefPerLevel, length(prefix) - 1), collapse=""), retval, sep="")
        }
        return(retval)
    }
}

catw <- function(..., file = "", sep = " ", fill = FALSE, labels = NULL,
    append = FALSE, prefix=0)
{
  # writes out some innformation on the calling function to screen
    if(is.numeric(prefix))
    {
        prefix<-curfnfinder(skipframes=prefix+1) #note: the +1 is there to avoid returning catw itself
        prefix<-paste(prefix, ": ", sep="")
    }
    cat(prefix, ..., format(Sys.time(), "(%Y-%m-%d %H:%M:%S)"), "\n",
        file = file, sep = sep, fill = fill, labels = labels, append = append)
}

#' get.args
#'
#' @return a list of the argument values used in a function call
#'
#' @examples
#' eg <- function(a = 1, b = 2, c = 5){
#'   get.args()
#' }
#' eg()
#' eg(10, c = 20)
get.args <- function(){
    def.call <- sys.call(-1)
    def <- get(as.character(def.call[[1]]), mode="function", sys.frame(-2))
    act.call <- match.call(definition = def, call = def.call)
    def <- as.list(def)
    def <- def[-length(def)]
    act <- as.list(act.call)[-1]

    def.nm <- names(def)
    act.nm <- names(act)
    inds <- def.nm %in% act.nm
    out <- def
    out[inds] <- act
    out
}
