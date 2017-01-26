load.us.age.data <- function(fn ## fn is the filename with relative path
                             ){
  ## Reads in the US age data file and returns it as a data frame
  ## Assumes the age column have the format aXX where XX are numbers 0-N
  ##  and year is a column which will become the rownames
  read.csv(fn, stringsAsFactors = FALSE)
  ## yrs <- d$year
  ## age.match <- grep("a[0-9]+", names(d))
  ## age.lab <- names(d)[age.match]
  ## age.lab <- gsub("a", "", age.lab)
  ## age.df <- d[,age.match]
  ## names(age.df) <- age.lab
  ## rownames(age.df) <- yrs
  ## ## Replace any NA's with zero
  ## age.df[is.na(age.df)] <- 0
  ## return(age.df)
}

load.can.age.data <- function(fn ## fn is the filename with relative path
                                ){
  ## Reads in the canadian age data file and returns it as a list of data frames

  dat <- readLines(fn)
  header.line.nums <- grep("^[[:alpha:]].*$", dat)
  headers <- dat[header.line.nums]
  headers <- gsub("#+ +", "", headers)
  f <- function(x){
    ## takes vector x, and returns a list of length 1 less than x,
    ## where each element is a vector of x[i]+1 : x[i+1]-1
    ret <- list()
    x1 <- x[-1]
    x2 <- x[-length(x)]
    for(i in 1:length(x1)){
      ret[[i]] <- x2[i]:x1[i]
      ret[[i]] <- ret[[i]][-1]
      ret[[i]] <- ret[[i]][-length(ret[[i]])]
    }
    return(ret)
  }
  l.list <- f(header.line.nums)
  ## Add the last sections line range in
  l.list[[length(l.list) + 1]] <- (header.line.nums[length(header.line.nums)] + 1):length(dat)
  names(l.list) <- headers
  ## Now, change these line ranges to actual data frames of the data
  for(i in 1:length(l.list)){
    l.list[[i]] <- dat[l.list[[i]]]
  }
  ## Get ages from first line of the first data frame in the list and remove it from the data
  ages <- l.list[[1]][1]
  ## Get rid of the preceeding comma
  ages <- strsplit(ages, ",")[[1]][-1]
  l.list[[1]] <- l.list[[1]][-1]
  l.list <- lapply(l.list, strsplit, ",")
  ## Now l.list is a list of lists of lists. Need to combine each element into a data frame
  d.list <- list()
  for(i in 1:length(l.list)){
    d.list[[i]] <- t(as.data.frame(sapply(l.list[[i]], rbind)))
    yrs <- d.list[[i]][,1]
    rownames(d.list[[i]]) <- as.character(d.list[[i]][,1])
    d.list[[i]] <- d.list[[i]][,-1]
    if(class(d.list[[i]]) == "matrix"){
      colnames(d.list[[i]]) <- ages
      d.list[[i]] <- apply(d.list[[i]], c(1,2), as.numeric)
    }
  }
  names(d.list) <- names(l.list)
  return(d.list)
}

load.survey.history <- function(fn ## fn is the filename with relative path
                                ){
  ## Reads in the survey history file and returns it as a data frame

  dat <- read.csv(fn)
  return(dat)
}

load.sampling.history <- function(fn ## fn is the filename with relative path
                                ){
  ## Reads in the sampling history file and returns it as a data frame

  dat <- read.csv(fn)
  return(dat)
}

load.wt.at.age <- function(model,
                           wt.at.age.fn){
  ## Load in the wtatage file for the model given, and extract the weights-at-age for
  ##  fleet 2. Return data frame of wt-at-age.
  fn <- file.path(model$path, wt.at.age.fn)
  d <- readLines(fn)
  beg <- grep("Fleet = 2", d)
  header <- beg + 1
  ## Extract age column names
  header <- strsplit(d[header], " +")[[1]]
  header <- header[-(1:7)]
  header <- gsub("a", "", header)
  header <- c("year", header)

  ## Extract weight-at-age data
  beg <- beg + 3  ## Remove the two header lines and the first age row (-1940)
  end <- length(d) - 1
  k <- d[beg:end]
  k.list <- sapply(k, function(x) strsplit(x, " +"))
  df <- NULL
  for(i in 1:length(k.list)){
    df <- rbind(df, k.list[[i]])
  }
  df <- df[,-c(1,3,4,5,6,7)]
  df <- as.data.frame(apply(df, c(1,2), as.numeric))
  names(df) <- header
  return(df)
}
