make.parameters.estimated.summary.table <- function(model,
                                                    start.rec.dev.yr,
                                                    end.rec.dev.yr,
                                                    digits = 3,
                                                    xcaption = "default",
                                                    xlabel   = "default",
                                                    font.size = 9,
                                                    space.size = 10,
                                                    return.xtable = TRUE){
  ## Returns an xtable for the parameters estimated summary
  ##
  ## model - an mcmc run, output of the r4ss package's function SSgetMCMC()
  ## start.rec.dev.yr - first year of estimated recruitment devs
  ## end.rec.dev.yr - last year of estimated recruitment devs
  ## digits - number of decimal points for the estimates
  ## xcaption - caption to appear in the calling document
  ## xlabel - the label used to reference the table in latex
  ## font.size - size of the font for the table
  ## space.size - size of the vertical spaces for the table
  ## return.xtable - if TRUE, return an xtable, if FALSE return R data frame

  ## Column indices for the values as found in the control file
  lo <- 1
  hi <- 2
  init <- 3
  p.mean <- 4 ## prior mean
  p.sd <- 5   ## prior sd
  p.type <- 6 ## prior type
  phase <- 7
  start.yr.sel <- 10
  end.yr.sel <- 11
  sel.dev.sd <- 12

  prior.type <- c("0" = "Uniform",
                  "-1" = "Uniform",
                  "2" = "Beta",
                  "3" = "Lognormal")

  fetch.and.split <- function(ctl, x){
    ## Fetch the line x from the vector ctl and split it up, removing spaces.
    ## Also remove any leading spaces
    ## Return the vector of values
    j <- ctl[x]
    ## Remove inter-number spaces
    j <- strsplit(j," +")[[1]]
    ## Remove leading spaces
    j <- j[j != ""]
    return(j)
  }

  fetch.prior.info <- function(vals,
                               digits = 2){
    ## Looks at the prior type p.type and phase, and if uniform will return
    ##  "Uniform"
    ## If not uniform, it will parse the vals and build a string defining
    ##  the prior info.
    ## If Fixed, it will return the initial value
    ## If Lognormal, it will parse the vals and build a string defining the
    ##  prior info, with the exp function applied
    if(vals[p.type] < 0 & vals[phase] > 0){
      ## Uniform prior on estimated parameter
      return("Uniform")
    }
    if(vals[p.type] < 0 & vals[phase] < 0){
      ## Fixed parameter
      return(vals[init])
    }
    # if(prior.type[vals[p.type]] == "Lognormal"){
    #   return(paste0(prior.type[vals[p.type]], " (",
    #                 f(exp(as.numeric(vals[p.mean])), digits), ", ",
    #                 f(exp(as.numeric(vals[p.sd])), digits), ")"))
    # }
    paste0(prior.type[vals[p.type]], " (",
           f(as.numeric(vals[p.mean]), digits), ", ",
           f(as.numeric(vals[p.sd]), digits), ")")
  }

  ctl <- model$ctl
  # Remove preceeding and trailing whitespace on all elements,
  #  but not 'between' whitespace.
  ctl <- gsub("^[[:blank:]]+", "", ctl)
  ctl <- gsub("[[:blank:]]+$", "", ctl)
  ## Remove all lines that start with a comment
  ctl <- ctl[-grep("^#.*", ctl)]

  ## R0 is at line 46 of comment-stripped dataframe. Get it's values which can
  ##  be indexed by the variables defined above
  r0 <- fetch.and.split(ctl, 46)
  r0.vals <- c(paste0("Log (",
                      latex.subscr(latex.italics("R"),
                                   "0"),
                      ")"),
               1,
               paste0("(", r0[lo], ", ", r0[hi], ")"),
               prior.type[r0[p.type]])

  ## Steepness is at line 47 of comment-stripped dataframe
  h <- fetch.and.split(ctl, 47)
  h.vals <- c(paste0("Steepness (",
                     latex.italics("h"),
                     ")"),
              1,
              paste0("(", h[lo], ", ", h[hi], ")"),
              fetch.prior.info(h, digits))

  ## Recruitment variability (sigma_r) is at line 48 of comment-stripped dataframe
  sig.r <- fetch.and.split(ctl, 48)
  sig.r.vals <- c(paste0("Recruitment variability (",
                         latex.italics("$\\sigma_r$"),
                         ")"),
                  if(sig.r[p.type] < 0 & sig.r[phase] > 0)
                    1
                  else
                    "--",
                  if(sig.r[p.type] < 0 & sig.r[phase] > 0)
                    paste0(" (", sig.r[lo], ", ", sig.r[hi], ")")
                  else
                    "--",
                  sig.r[3])
                  ##fetch.prior.info(sig.r, digits))

  ## Recruitment devs, lower and upper bound found on lines 66 and 67 of
  ##  comment-stripped dataframe
  ## The number of them comes from the arguments to this function (for now)
  rec.dev.lb <- fetch.and.split(ctl, 66)[1]
  rec.dev.ub <- fetch.and.split(ctl, 67)[1]
  rec.dev.vals <- c(paste0("Log recruitment deviations: ",
                           start.rec.dev.yr,
                           "--",
                           end.rec.dev.yr),
                    end.rec.dev.yr - start.rec.dev.yr + 1,
                    paste0("(",
                           rec.dev.lb,
                           ", ",
                           rec.dev.ub,
                           ")"),
                    paste0("Lognormal (0, ",
                           latex.italics("$\\sigma_r$"),
                           ")"))

  ## Natural mortality is at line 25 of comment-stripped dataframe
  m <- fetch.and.split(ctl, 25)
  m.vals <- c(paste0("Natural mortality (",
                     latex.italics("M"),
                     ")"),
              if(prior.type[m[p.type]] == "Fixed")
                "--"
              else
                1,
              paste0("(", m[lo], ", ", m[hi], ")"),
              fetch.prior.info(m, digits))

  q.vals <- c(paste0("Catchability (",
                     latex.italics("q"),
                     ")"),
              1,
              "--",
              "Analytic solution")

  ## Survey additional value for SE is at line 77 of comment-stripped dataframe
  se <- fetch.and.split(ctl, 74)
  se.vals <- c("Additional variance for survey log(SE)",
               if(prior.type[se[p.type]] == "Fixed")
                 1
               else
                 "--",
               paste0("(", se[lo], ", ", se[hi], ")"),
               "Uniform")
               ##fetch.prior.info(se, digits))

  ## Number of survey selectivities is on line 81 of comment-stripped dataframe
  num.sel <- fetch.and.split(ctl, 81)
  grep.num.sel <- grep("#", num.sel)
  if(length(grep.num.sel) > 0){
    num.sel <- num.sel[1:(grep.num.sel - 1)]
  }
  num.sel <- as.numeric(num.sel[length(num.sel)])
  ## num.sel is the number of selectivity entries in the file for survey
  ## Age-0 starts on line 104 of comment-stripped dataframe
  line.num <- 104
  ages.estimated <- NULL
  for(i in line.num:(line.num + num.sel - 1)){
    age.sel <- fetch.and.split(ctl, i)
    if(age.sel[phase] > 0){
      ## This age plus one is being estimated
      ages.estimated <- c(ages.estimated, i - line.num + 1)
      ## Use the last line to get the values
      est.sel <- age.sel
    }
  }
  age.sel.vals <- c(paste0("Non-parametric age-based selectivity: ages ",
                           min(ages.estimated),
                           "--",
                           max(ages.estimated)),
                    length(ages.estimated),
                    paste0(" (", est.sel[lo], ", ", est.sel[hi], ")"),
                    "Uniform")
                    ##fetch.prior.info(est.sel, digits))

  ## Number of fishery selectivities is on line 80 of comment-stripped dataframe
  num.sel <- fetch.and.split(ctl, 80)
  grep.num.sel <- grep("#", num.sel)
  if(length(grep.num.sel) > 0){
    num.sel <- num.sel[1:(grep.num.sel - 1)]
  }
  num.sel <- as.numeric(num.sel[length(num.sel)])
  ## num.sel is the number of selectivity entries in the file for survey
  ## Age-0 starts on line 82 of comment-stripped dataframe
  line.num <- 82
  ages.estimated <- NULL
  for(i in line.num:(line.num + num.sel)){
    age.sel <- fetch.and.split(ctl, i)
    if(age.sel[phase] > 0){
      ## This age plus one is being estimated
      ages.estimated <- c(ages.estimated, i - line.num)
      ## Use the last line to get the values
      est.sel <- age.sel
    }
  }
  f.age.sel.vals <- c(paste0("Non-parametric age-based selectivity: ages ",
                             min(ages.estimated),
                             "--",
                             max(ages.estimated)),
                      length(ages.estimated),
                      paste0("(", est.sel[lo], ", ", est.sel[hi], ")"),
                      "Uniform")
                      ##fetch.prior.info(est.sel, digits))

  ## Selectivity deviations for fishery. Uses last line to get values, assumes
  ##  all are the same
  f.age.sel.dev.vals <-
    c(paste0("Selectivity deviations (",
             est.sel[start.yr.sel],
             "--",
             est.sel[end.yr.sel],
             ", ages ",
             min(ages.estimated),
             "--",
             max(ages.estimated),
             ")"),
      length(ages.estimated) * length(est.sel[start.yr.sel]:est.sel[end.yr.sel]),
      "--",
      paste0("Normal (0, ",
             ##est.sel[sel.dev.sd],
             model$parameters["AgeSel_P3_Fishery(1)_dev_se", "Value"],
             ")"))

  ## Dirichlet-Multinomial likelihood parameters
  dm <- fetch.and.split(ctl, 124)
  dm.vals <- c(paste0("Dirichlet-Multinomial likelihood (",
                       latex.italics("$\\log \\theta$"),
                      ")"),
               2,
               paste0("(", dm[lo], ", ", dm[hi], ")"),
               paste0("Normal (",
                      dm[4], ", ",
                      dm[5],
                      ")"))
               ##fetch.prior.info(dm, digits))

  tab <- rbind(r0.vals,
               h.vals,
               sig.r.vals,
               rec.dev.vals,
               m.vals,
               q.vals,
               se.vals,
               age.sel.vals,
               f.age.sel.vals,
               f.age.sel.dev.vals,
               dm.vals)

  if(!return.xtable){
    return(tab)
  }

  ## Make first row empty to make the Stock Dynamics header appear below the
  ##  horizontal line
  tab <- rbind(c("", "", "", ""), tab)

  colnames(tab) <- c(latex.bold("Parameter"),
                     latex.mlc(c("Number of",
                                 "parameters")),
                     latex.mlc(c("Bounds",
                                 "(low, high)")),
                     latex.mlc(c("Prior (Mean, SD)",
                                 "single value = fixed")))

  addtorow <- list()
  addtorow$pos <- list()
  addtorow$pos[[1]] <- 1
  addtorow$pos[[2]] <- 6
  addtorow$pos[[3]] <- 6
  addtorow$pos[[4]] <- 9
  addtorow$pos[[5]] <- 11
  addtorow$command <-
    c(paste0(latex.bold(latex.under("Stock Dynamics")),
             latex.nline),
      paste0(latex.nline,
             latex.bold(latex.under("Catchability and selectivity")),
             latex.nline),
      paste0(latex.bold(latex.italics("Acoustic Survey")),
             latex.nline),
      paste0(latex.bold(latex.italics("Fishery")),
             latex.nline),
      paste0(latex.nline,
             latex.bold(latex.under("Data weighting")),
             latex.nline))
  # Add spaces after commas and before opening parentheses
  tab <- map_df(as_tibble(tab), ~{gsub(",", ", ", .x)}) %>% as.data.frame
  tab <- map_df(as_tibble(tab), ~{gsub("\\(", " \\(", .x)}) %>% as.data.frame

  ## Make the size string for font and space size
  size.string <- latex.size.str(font.size, space.size)
  print(xtable(tab,
               caption = xcaption,
               label = xlabel,
               align = get.align(ncol(tab),
                                 just = "c")),
        caption.placement = "top",
        include.rownames = FALSE,
        sanitize.text.function = function(x){x},
        size = size.string,
        add.to.row = addtorow,
        table.placement = "H")
}


#' @param modellist A list summarized by \code{\link[r4ss]{SSsummarize}}.
#' @param catchability Should catchability for the acoustic survey be
#' included in the parameter table? Default is to not include it, i.e.,
#' \code{catchability = FALSE}.
get_keypars <- function(modellist, catchability = FALSE) {
  out <- modellist$pars[c(
      grep("NatM|SR_LN|steep|SD.+2", modellist$pars$Label),
      grep("EffN", modellist$pars$Label)
      ), 1:modellist$n]
  SD3 <- grep("SD.+3", modellist$pars$Label)
  if (length(SD3) > 0) {
    out <- rbind(out, modellist$pars[SD3, 1:modellist$n])
  }
  if (catchability) {
    out <- rbind(out, exp(
      modellist$pars[grep("LnQ.+2", modellist$pars$Label), 1:modellist$n]))
  }
  out[2, ] <- exp(out[2, ]) / 1000
  return(out)
}

#' @param modellist A list summarized by \code{\link[r4ss]{SSsummarize}}.
#' @param years.recs A vector of length three specifying the recruitment
#' years of interest.
#' @param years.b0 A vector of length two specifying the depletion years
#' of interest.
get_keydqs <- function(modellist, years.recs, years.b0) {
  recnames <- paste(paste0("Recr_", years.recs), collapse = "|")
  b0names <- paste(paste0("Bratio_", years.b0), collapse = "|")
  out <- rbind(
    modellist$recruits[grep(recnames, modellist$recruits$Label), 1:modellist$n]/1000,
    modellist$SpawnBio[modellist$SpawnBio$Label == "SSB_Virgin", 1:modellist$n]/2/1000,
    modellist$Bratio[grep(b0names, modellist$Bratio$Label), 1:modellist$n]*100
    )
  return(out)
}

#' @param modellist A list summarized by \code{\link[r4ss]{SSsummarize}}.
#' @param years.spr A vector of length one specifying the SPR year
#' of interest.
get_keyrps <- function(modellist, years.spr, target = 40) {
  sprnames <- paste(paste0("SPRratio_", years.spr), collapse = "|")
  out <- rbind(
    modellist$quants[grep(sprnames, modellist$quants$Label), 1:modellist$n]*100,
    modellist$quants[modellist$quants$Label == "SSB_SPR", 1:modellist$n]/2/1000,
    modellist$quants[modellist$quants$Label == "SPR_MSY", 1:modellist$n]*0+target,
    modellist$quants[modellist$quants$Label == "Fstd_SPR", 1:modellist$n]*100,
    modellist$quants[modellist$quants$Label == "Dead_Catch_SPR", 1:modellist$n]/1000
    )
  return(out)
}

#' @param modellist A list summarized by \code{\link[r4ss]{SSsummarize}}.
get_keylhs <- function(modellist) {
  out <- rbind(
    modellist$likelihoods[
      modellist$likelihoods$Label %in% c("TOTAL", "Survey"), 1:modellist$n],
    structure(t(
    modellist$likelihoods_by_fleet[
      modellist$likelihoods_by_fleet$Label == "Age_like",
      c("Acoustic_Survey", "Fishery")]),
    dimnames = list(NULL, paste0("model", 1:modellist$n))),
    modellist$likelihoods[
      modellist$likelihoods$Label %in% c("Recruitment", "Parm_priors", "Parm_devs"),
      1:modellist$n]
    )
  return(out)
}

#' @param getpar A vector of character strings that you desire parameters for.
#' Regex will be used to get full names, but each entry should map to a single
#' parameter. The function is not yet parameterized to work with multiple
#' matches for single entry.
#' @param mcmc The mcmc list from a list summarized by \code{\link[r4ss]{SSsummarize}}.
#' @param .fun A function, e.g., median or mean, to summarize the posterior.
  get_keymcmc <- function(getpar, mcmc, .fun) {
    out <- lapply(mcmc, function(x, y = .fun) {
      aa <- x[, grep(getpar, colnames(x)), drop = FALSE]
      if (NCOL(aa) == 0) {
        aa[, 1] <- as.numeric(NA)
      }
      return(apply(aa, 2, FUN = y))
    })
    return(setNames(unlist(out), NULL))
  }

make.short.parameter.estimates.sens.table <- function(models,
                                                      model.names,
                                                      end.yr,
                                                      age.1 = FALSE,
                                                      digits = 3,
                                                      xcaption = "default",
                                                      xlabel   = "default",
                                                      font.size = 9,
                                                      space.size = 10,
                                                      getrecs = c(2010, 2014, 2016),
                                                      show.likelihoods = TRUE){
  ## Returns an xtable in the proper format for the MLE parameter estimates for
  ##  all the models, one for each column
  ##
  ## models - a list of models which contain the MLE output from SS_output()
  ## model.names - a vector of names of the same length as the number of
  ##  models in the models list
  ## end.yr - the last year to include
  ## age.1 - if TRUE, add the age-1 index parameter to the table. Set to FALSE
  ##  if age.1 index not in any of the models being shown else the
  ##  'Additional age-1 index SD' row gets populated with recruitment estimates
  ##  (since there are no NA's to get replaced by dashes)
  ## digits - number of decimal points for the estimates
  ## xcaption - caption to appear in the calling document
  ## xlabel - the label used to reference the table in latex
  ## font.size - size of the font for the table
  ## space.size - size of the vertical spaces for the table
  ## getrecs - a vector of integers of length 3 supplying the years for which you want
  ##   estimates of recruitment. Must be of length three.
  ## show.likelihoods - if TRUE, return the negative log-likelihoods (FALSE for presentations)
  if (length(getrecs) != 3) stop("The make short function only works",
    "with three years of recruitments", call. = FALSE)

  modelssum <- r4ss::SSsummarize(models)
  tab <- rbind(
    get_keypars(modelssum),
    get_keydqs(modelssum, years.recs = getrecs, years.b0 = c(2009, end.yr)),
    get_keyrps(modelssum, years.spr = end.yr - 1, target = 40),
    get_keylhs(modelssum))

  ## Format the tables rows depending on what they are
  ## Decimal values
  if(age.1){
    tab[c(1, 3, 4, 5, 6, 7),] <- f(tab[c(1, 3, 4, 5, 6, 7),], 3)
  }else{
    tab[c(1, 3, 4, 5, 6),] <- f(tab[c(1, 3, 4, 5, 6),], 3)
  }

  ## Large numbers with no decimal points but probably commas
  tab[c(2,
        ifelse(age.1, 8, 7),
        ifelse(age.1, 9, 8),
        ifelse(age.1, 10, 9),
        ifelse(age.1, 11, 10),
        ifelse(age.1, 15, 14),
        ifelse(age.1, 18, 17)),] <-
    f(apply(tab[c(2,
                  ifelse(age.1, 8, 7),
                  ifelse(age.1, 9, 8),
                  ifelse(age.1, 10, 9),
                  ifelse(age.1, 11, 10),
                  ifelse(age.1, 15, 14),
                  ifelse(age.1, 18, 17)),],
                c(1, 2), as.numeric))
  ## Percentages
  tab[c(ifelse(age.1, 12, 11),
        ifelse(age.1, 13, 12),
        ifelse(age.1, 14, 13),
        ifelse(age.1, 17, 16)),] <-
    paste0(f(apply(tab[c(ifelse(age.1, 12, 11),
                         ifelse(age.1, 13, 12),
                         ifelse(age.1, 14, 13),
                         ifelse(age.1, 17, 16)),],
                   c(1, 2), as.numeric), 1), "\\%")
  ## SPR Percentages row (some may be NA). This is really ugly but works
  tab[ifelse(age.1, 16, 15),
      !is.na(tab[ifelse(age.1, 16, 15),])] <-
    paste0(f(as.numeric(tab[ifelse(age.1, 16, 15),
                            !is.na(tab[ifelse(age.1, 16, 15),])]), 1), "\\%")

  ## Likelihoods - Possibly commas and 2 decimal points
  if(age.1){
    tab[19:25,] <-
      f(apply(tab[19:25,],
              c(1, 2), as.numeric), 2)
  }else{
    tab[18:24,] <-
      f(apply(tab[18:24,],
              c(1, 2), as.numeric), 2)
  }

  ## Make first row empty to make the Parameter header appear below the
  ##  horizontal line
  tab <- rbind(rep("", length(models)), tab)

  ## replace "   NA" with dashes
  tab <- as.data.frame(lapply(tab,
                              function(x){
                                gsub(" +NA",
                                     paste0("\\", latex.bold("--")),
                                     x)
                              }))

  ## Set the first column to be the names
  ## The first empty string is necessary because of the
  ##  rbind(rep("", length(models)), tab) call above

  tab_labels <- cbind(c("",
                 paste0("Natural mortality (",
                        latex.italics("M"),
                        ")"),
                 paste0(latex.subscr(latex.italics("R"), "0"),
                        " (millions)"),
                 paste0("Steepness (",
                        latex.italics("h"),
                        ")"),
                 "Additional acoustic survey SD",
                 "Dirichlet-Multinomial fishery (log~$\\theta_{\\text{fish}}$)",
                 "Dirichlet-Multinomial survey (log~$\\theta_{\\text{surv}}$)",
                 paste(getrecs, "recruitment (millions)"),
                 paste0(latex.subscr(latex.italics("B"), "0"),
                        " (thousand t)"),
                 "2009 relative spawning biomass",
                 paste0(end.yr,
                        " relative spawning biomass"),
                 paste0(end.yr - 1,
                        " rel. fishing intensity: (1-SPR)/(1-",
                        latex.subscr("SPR", "40\\%"),
                        ")"),
                 paste0("Female spawning biomass (",
                        latex.italics("$B_{F_{40_{\\%}}}$"),
                        "; thousand t)"),
                 latex.subscr("SPR", "MSY-proxy"),
                 "Exploitation fraction corresponding to SPR",
                 paste0("Yield at ",
                        latex.italics("$B_{F_{40_{\\%}}}$"),
                        " (thousand t)"),
                 "Total",
                 "Survey",
                 "Survey age compositions",
                 "Fishery age compositions",
                 "Recruitment",
                 "Parameter priors",
                 "Parameter deviations"))

  if(age.1){
    tab_labels <- append(tab_labels, "Additional age-1 index SD", after = 7)
  }
  tab <- cbind(tab_labels, tab)
  ## Need to split up the headers (model names) by words and let them stack on
  ##  top of each other
  model.names.str <- unlist(lapply(gsub(" ",
                                        "\\\\\\\\",
                                        model.names),
                                   latex.mlc,
                                   make.bold = FALSE))
  colnames(tab) <- c("", model.names.str)

  addtorow <- list()
  addtorow$pos <- list()
  addtorow$pos[[1]] <- 1
  addtorow$pos[[2]] <- ifelse(age.1, 8, 7)
  addtorow$pos[[3]] <- ifelse(age.1, 14, 13)
  addtorow$pos[[4]] <- ifelse(age.1, 19, 18)
  addtorow$command <-
    c(paste0(latex.bold(latex.under("Parameters")),
             latex.nline),
      paste0(latex.nline,
             latex.bold(latex.under("Derived Quantities")),
             latex.nline),
      paste0(latex.nline,
             latex.bold(latex.under("Reference Points based on $\\Fforty$")),
             latex.nline),
      paste0(latex.nline,
             latex.bold(latex.under("Negative log likelihoods")),
             latex.nline))
  ## Remove likelihood rows (use for beamer to fit on slides)
  if(!show.likelihoods){
    tab <- tab[1:(grep("Total", tab[,1])-1),]
    addtorow$pos[[4]] <- NULL
    addtorow$command <- addtorow$command[-4]
  }

  ## Make the size string for font and space size
  size.string <- latex.size.str(font.size, space.size)
  print(xtable(tab,
               caption = xcaption,
               label = xlabel,
               align = get.align(ncol(tab),
                                 just = "c")),
        caption.placement = "top",
        include.rownames = FALSE,
        sanitize.text.function = function(x){x},
        size = size.string,
        add.to.row = addtorow,
        table.placement = "H")
}


make.short.parameter.estimates.table <- function(model,
                                                 last.yr.model,
                                                 end.yr,
                                                 getrecs = c(2010, 2014, 2016),
                                                 digits = 3,
                                                 xcaption = "default",
                                                 xlabel   = "default",
                                                 font.size = 9,
                                                 space.size = 10,
                                                 last.yr.model.name = NULL){
  ## Returns an xtable in the proper format for the parameter estimates
  ##  for MLE vs median vs median of the last year
  ##
  ## model - an mcmc run, output of the r4ss package's function SSgetMCMC()
  ## last.yr.model - last year's base model for comparison
  ## end.yr - the last year to include (req'd for spawning biomass)
  ## digits - number of decimal points for the estimates
  ## xcaption - caption to appear in the calling document
  ## xlabel - the label used to reference the table in latex
  ## font.size - size of the font for the table
  ## space.size - size of the vertical spaces for the table
  ## getrecs - a vector of integers of length 3 supplying the years for which you want
  ##   estimates of recruitment. Must be of length three.
  if (length(getrecs) != 3) stop("The make short function only works",
                                 "with three years of recruitments", call. = FALSE)
  if (is.null(last.yr.model.name)) {
    last.yr.model.name <- c("Posterior", "median from", paste(end.yr - 1, " base"), "model")
  }

  ## This year's model MLE
  modelssum <- r4ss::SSsummarize(list(model, last.yr.model))
  mle.par <- rbind(
    get_keypars(modelssum, catchability = TRUE),
    get_keydqs(modelssum, years.recs = getrecs, years.b0 = c(2009, end.yr)),
    get_keyrps(modelssum, years.spr = end.yr - 1, target = 40))[, 1]

  mcmc.par <- t(sapply(c(
    "NatM", "SR_LN", "SR_BH_steep", "Q_extraSD", "EffN.+\\)_1", "EffN.+2", "Catchability",
    paste0("Recr_", getrecs), "SSB_Initial", paste0("Bratio_", c(2009, end.yr)),
    paste(paste0("SPRratio_", end.yr-1), collapse = "|"),
    "SSB_SPR", "SPR_MSY", "Fstd_SPR", "Dead_Catch_SPR"),
    get_keymcmc, mcmc = modelssum$mcmc, .fun = median))
  mcmc.par["SR_LN", ] <- exp(mcmc.par["SR_LN", ]) / 1000
  mcmc.par["Catchability", 1] <- round(median(model$extra.mcmc$Q_vector), 3)
  mcmc.par["Catchability", 2] <- round(median(last.yr.model$extra.mcmc$Q_vector), 3)
  mcmc.par[grep("Recr_|Dead_", rownames(mcmc.par)), ] <- (
    mcmc.par[grep("Recr_|Dead_", rownames(mcmc.par)), ]) / 1000
  mcmc.par[grep("ratio_|Fstd_SPR", rownames(mcmc.par)), ] <- (
    mcmc.par[grep("ratio_|Fstd_SPR", rownames(mcmc.par)), ]) * 100
  mcmc.par[grep("SSB_[IS]", rownames(mcmc.par)), ] <- (
    mcmc.par[grep("SSB_[IS]", rownames(mcmc.par)), ]) / 1000 / 2
  mcmc.par["SPR_MSY", ] <- 40
  cbind(mle.par, mcmc.par)
  tab <- as.data.frame(cbind(mle.par, mcmc.par))
  colnames(tab) <- NULL

  ## Format the tables rows depending on what they are.
  ## Decimal valuesn
  tab[c(1, 3, 4, 5, 6, 7),] <- f(tab[c(1, 3, 4, 5, 6, 7),], 3)

  ## Large numbers with no decimal points but probably commas
  tab[c(2, 8, 9, 10, 11, 15, 18),] <-
    f(apply(tab[c(2, 8, 9, 10, 11, 15, 18),], c(1, 2), as.numeric))
  ## Percentages on non-NA elements
  paste.perc <- function(vec){
    ## Paste percentages on to all elements of vec that are not NA
    vec[!is.na(vec)] <- paste0(f(as.numeric(vec[!is.na(vec)]), 1), "\\%")
    return(vec)
  }
  tab[12,] <- paste.perc(tab[12,])
  tab[13,] <- paste.perc(tab[13,])
  tab[14,] <- paste.perc(tab[14,])
  tab[17,] <- paste.perc(tab[17,])
  ## SPR Percentages row (some may be NA). This is really ugly but works
  tab[16, !is.na(tab[16,])] <-
    paste0(f(apply(tab[16, !is.na(tab[16,])], 1, as.numeric), 1), "\\%")

  ## Make first row empty to make the Parameter header appear below the
  ##  horizontal line
  tab <- rbind(c("", "", ""), tab)

  ## Replace NAs with dashes
  tab <- apply(tab, 2, function(x) gsub("\\s*NA\\s*", "\\\\textbf{--}", x))

  ## These values are not correct and need to be removed from the table
  tab[paste0("Bratio_", end.yr), 3] <- latex.bold("--")
  tab[paste0("SPRratio_", end.yr - 1), 3] <- latex.bold("--")

  ## Set the first column to be the names
  ## Empty string below is necessary because of the rbind(c("","",""), tab)
  ##  call above
  tab <- cbind(c("",
                 paste0("Natural mortality (",
                        latex.italics("M"),
                        ")"),
                 paste0("Unfished recruitment (",
                        latex.subscr(latex.italics("R"), "0"),
                        ", millions)"),
                 paste0("Steepness (",
                        latex.italics("h"),
                        ")"),
                 "Additional acoustic survey SD",
                 "Dirichlet-Multinomial fishery (log~$\\theta_{\\text{fish}}$)",
                 "Dirichlet-Multinomial survey (log~$\\theta_{\\text{surv}}$)",
                 paste0("Catchability (",
                        latex.italics("q"),
                        ")"),
                 paste(getrecs, "recruitment (millions)"),
                 paste0("Unfished female spawning biomass (",
                        latex.subscr(latex.italics("B"), "0"),
                        ", thousand~t)"),
                 "2009 relative spawning biomass",
                 paste0(end.yr,
                        " relative spawning biomass"),
                 paste0(end.yr - 1,
                        " relative fishing intensity: (1-SPR)/(1-",
                        latex.subscr("SPR", "40\\%"),
                        ")"),
                 paste0("Female spawning biomass at ",
                        latex.subscr(latex.italics("F"), "SPR=40\\%"),
                        "(",
                        latex.subscr(latex.italics("B"), "SPR=40\\%"),
                        ", thousand t)"),
                 paste0("SPR at ",
                        latex.subscr(latex.italics("F"), "SPR=40\\%")),
                 "Exploitation fraction corresponding to SPR",
                 paste0("Yield at ",
                        latex.subscr(latex.italics("B"), "SPR=40\\%"),
                        " (thousand~t)")),
               tab)
  colnames(tab) <- c(" ",
                     latex.bold("MLE"),
                     latex.mlc(c("Posterior",
                                 "median")),
                     latex.mlc(last.yr.model.name))

  addtorow <- list()
  addtorow$pos <- list()
  addtorow$pos[[1]] <- 1
  addtorow$pos[[2]] <- 8
  addtorow$pos[[3]] <- 16
  addtorow$command <-
    c(paste0(latex.bold(latex.under("Parameters")),
             latex.nline),
      paste0(latex.nline,
             latex.bold(latex.under("Derived Quantities")),
             latex.nline),
      paste0(latex.nline,
             latex.bold(latex.under("Reference Points (equilibrium) based on ")),
             latex.subscr(latex.italics("F"), "SPR=40\\%"),
             latex.nline))
  ## Make the size string for font and space size
  size.string <- latex.size.str(font.size, space.size)
  print(xtable(tab,
               caption = xcaption,
               label = xlabel,
               align = get.align(ncol(tab),
                                 just="c")),
        caption.placement = "top",
        include.rownames = FALSE,
        sanitize.text.function = function(x){x},
        size = size.string,
        add.to.row = addtorow,
        table.placement = "H")
}

make.long.parameter.estimates.table <- function(model,
                                                posterior.regex,
                                                digits = 4,
                                                xcaption = "default",
                                                xlabel   = "default",
                                                font.size = 9,
                                                space.size = 10){
  ## Returns an xtable in the proper format for the posterior medians of the
  ##  parameter estimates
  ##
  ## model - an mcmc run, output of the r4ss package's function SSgetMCMC()
  ## posterior.regex - a vector of the posterior names to search for
  ##  (partial names will be matched)
  ## digits - number of decimal points for the estimates
  ## xcaption - caption to appear in the calling document
  ## xlabel - the label used to reference the table in latex
  ## font.size - size of the font for the table
  ## space.size - size of the vertical spaces for the table

  mc <- model$mcmc
  mc.names <- names(mc)

  ## Start with the key posteriors using the regex
  mcmc.grep <- unique(grep(paste(posterior.regex, collapse = "|"), mc.names))
  mcmc.names <- mc.names[mcmc.grep]
  mcmc.par <- mc[,mcmc.grep]
  mcmc.meds <- as.data.frame(apply(mcmc.par, 2, median))
  df <- cbind(mcmc.names, mcmc.meds)
  names(df) <- c("param", "p.med")
  rownames(df) <- NULL

  calc.meds <- function(df, x){
    ## x is a data frame of posteriors for some parameters
    ## This function will take the medians of these,
    ##  and bind them with the data frame df, and return the result
    ## Assumes df has column names param and p.med
    d <- as.data.frame(apply(x, 2, median))
    d <- cbind(rownames(d), d)
    rownames(d) <- NULL
    names(d) <- c("param", "p.med")
    df <- rbind(df, d)
    return(df)
  }

  ## Add Dirichlet-Multinomial parameter
  ## currently only 1 value so calc.meds doesn't work due to
  ## getting a vector instead of a data.frame with names in header
  # dm <- data.frame(param = "ln(EffN_mult)_1",
  #                  p.med = median(mc$`ln(EffN_mult)_1`))
  # df <- rbind(df, dm)

  ## Add all Early_InitAge parameters
  ei <- mc[,grep("Early_InitAge_[0-9]+", mc.names)]
  df <- calc.meds(df, ei)

  ## Add all Recruitment deviation parameters
  rec <- mc[,union(grep(".*_RecrDev_[0-9]+", mc.names),
                  grep("ForeRecr_[0-9]+", mc.names))]
  df <- calc.meds(df, rec)

  ## Add all AgeSel
  a.sel <- mc[,grep("AgeSel_.*", mc.names)]
  df <- calc.meds(df, a.sel)

  ## Format the values
  df[,2] <- f(df[,2], digits)

  ## Make the underscores in the names have a preceding \ so latex will like it
  param.names <- df[,1]
  df[,1] <- gsub("\\_", "\\\\_", param.names)

  ## Latex column names
  names(df) <- c(latex.bold("Parameter"), latex.bold("Posterior median"))

  addtorow          <- list()
  addtorow$pos      <- list()
  addtorow$pos[[1]] <- c(0)
  addtorow$command  <- c(paste0(latex.hline,
                                "\n",
                                "\\endhead \n",
                                latex.hline,
                                "\n",
                                "{\\footnotesize Continued on next page} \n",
                                "\\endfoot \n",
                                "\\endlastfoot \n"))
  ## Make the size string for font and space size
  size.string <- latex.size.str(font.size, space.size)
  print(xtable(df,
               caption = xcaption,
               label = xlabel,
               align = get.align(ncol(df)),
               digits = digits),
        caption.placement = "top",
        table.placement = "H",
        tabular.environment = "longtable",
        floating = FALSE,
        include.rownames = FALSE,
        sanitize.text.function = function(x){x},
        size = size.string,
        add.to.row = addtorow,
        hline.after = c(-1))
}
