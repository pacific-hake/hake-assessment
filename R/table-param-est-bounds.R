#' Creates a table summarizing lead parameters
#'
#' @param model The model to use for the table
#' @param start.rec.dev.yr First year of estimated recruitment devs
#' @param end.rec.dev.yr Last year of estimated recruitment devs
#' @param digits Number of decimal points
#' @param xcaption Caption for the table
#' @param xlabel Latex label reference in the main doc
#' @param font.size Point size of font
#' @param space.size Vertical size between rows in the table
#' @param return.xtable If `TRUE`, return the latex-formatted table code. If `FALSE`
#' return the data frame instead
#' @return Either a data frame or [xtable::xtable()] object
#' @export
table_param_est_bounds <- function(model,
                                   start.rec.dev.yr,
                                   end.rec.dev.yr,
                                   digits = 3,
                                   xcaption = "default",
                                   xlabel   = "default",
                                   font.size = 9,
                                   space.size = 10,
                                   return.xtable = TRUE){

  lo <- 1
  hi <- 2
  init <- 3
  p.mean <- 4 # prior mean
  p.sd <- 5   # prior sd
  p.type <- 6 # prior type
  phase <- 7
  start_yr.sel <- 10
  end_yr.sel <- 11
  sel.dev.sd <- 12

  prior.type <- c("0" = "Uniform",
                  "-1" = "Uniform",
                  "2" = "Beta",
                  "3" = "Lognormal")

  fetch.and.split <- function(ctl, x){
    # Fetch the line x from the vector ctl and split it up, removing spaces.
    # Also remove any leading spaces
    # Return the vector of values
    j <- ctl[x]
    # Remove inter-number spaces
    j <- strsplit(j," +")[[1]]
    # Remove leading spaces
    j <- j[j != ""]
    return(j)
  }

  fetch.prior.info <- function(vals, digits = 2){
    # Looks at the prior type p.type and phase, and if uniform will return
    #  "Uniform"
    # If not uniform, it will parse the vals and build a string defining
    #  the prior info.
    # If Fixed, it will return the initial value
    # If Lognormal, it will parse the vals and build a string defining the
    #  prior info, with the exp function applied
    if(vals[p.type] < 0 & vals[phase] > 0){
      # Uniform prior on estimated parameter
      return("Uniform")
    }
    if(vals[p.type] < 0 & vals[phase] < 0){
      # Fixed parameter
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
  # Remove all lines that start with a comment
  ctl <- ctl[-grep("^#.*", ctl)]

  # R0 is at line 43 of comment-stripped dataframe. Get it's values which can
  #  be indexed by the variables defined above
  r0 <- fetch.and.split(ctl, 43)
  r0.vals <- c(paste0("Log (",
                      latex.subscr(latex.italics("R"),
                                   "0"),
                      ")"),
               1,
               paste0("(", r0[lo], ", ", r0[hi], ")"),
               prior.type[r0[p.type]])

  ## Steepness is at line 44 of comment-stripped dataframe
  h <- fetch.and.split(ctl, 44)
  h.vals <- c(paste0("Steepness (",
                     latex.italics("h"),
                     ")"),
              1,
              paste0("(", h[lo], ", ", h[hi], ")"),
              fetch.prior.info(h, digits))

  ## Recruitment variability (sigma_r) is at line 45 of comment-stripped dataframe
  sig.r <- fetch.and.split(ctl, 45)
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

  # Recruitment devs, lower and upper bound found on lines 63 and 64 of
  #  comment-stripped dataframe
  # The number of them comes from the arguments to this function (for now)
  rec.dev.lb <- fetch.and.split(ctl, 63)[1]
  rec.dev.ub <- fetch.and.split(ctl, 64)[1]
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

  # Natural mortality
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

  # Survey additional value for SE
  se <- fetch.and.split(ctl, 75)
  se.vals <- c("Additional variance for survey log(SE)",
               1,
               paste0("(", se[lo], ", ", se[hi], ")"),
               "Uniform")

  tmp <- data.frame(do.call("rbind",
                            strsplit(grep("AgeSel_.*_Survey.*[0-9]\\)$", ctl, value = TRUE),"\\s+")))
  s.ages.estimated <- which(tmp[, 7] > 0) - 1
  age.sel.vals <- c(paste0("Non-parametric age-based selectivity: ages ",
                           min(s.ages.estimated),
                           "--",
                           max(s.ages.estimated)),
                    length(s.ages.estimated),
                    paste0(" (", min(tmp[tmp[, 7]>0, lo]), ", ", min(tmp[tmp[, 7]>0, hi]), ")"),
                    "Uniform")

  # Age-1 survey additional value for SE
  se.age1 <- fetch.and.split(ctl, 77)
  se.vals.age1 <- c("Additional variance for age-1 index log(SE)",
                    1,
                    paste0("(", se.age1[lo], ", ", se.age1[hi], ")"),
                    "Uniform")

  tmp <- data.frame(do.call("rbind",
                            strsplit(grep("AgeSel_.*_Fishery.*[0-9]\\)$", ctl, value = TRUE),"\\s+")))
  f.ages.estimated <- which(type.convert(as.is = TRUE, tmp[, 7]) > 0) - 1
  f.age.sel.vals <- c(paste0("Non-parametric age-based selectivity: ages ",
                             min(f.ages.estimated),
                             "--",
                             max(f.ages.estimated)),
                      length(f.ages.estimated),
                      paste0("(", min(tmp[tmp[, 7]>0, lo]), ", ",
                             max(tmp[tmp[, 7]>0, hi]), ")"),
                      "Uniform")
  n.yrs.sel.vals <- diff(as.numeric(tmp[tmp[,7]>0,start_yr.sel:end_yr.sel][1, 1:2])) + 1

  # Selectivity deviations for fishery. Uses last line to get values, assumes
  #  all are the same
  sel_dev_inds <- grep("AgeSel.*Fishery.*DEV", model$parameters$Label)
  sel_devs <- model$parameters[sel_dev_inds,]
  sel_dev_bounds <- c(first(sel_devs$Min), first(sel_devs$Max))
  sel_dev_bounds <- paste0("(", sel_dev_bounds[1], ", ", sel_dev_bounds[2], ")")

  f.age.sel.dev.vals <-
    c(paste0("Selectivity deviations (",
             min(tmp[tmp[,start_yr.sel] != 0,start_yr.sel]),
             "--",
             max(tmp[tmp[,end_yr.sel] != 0,end_yr.sel]),
             ", ages ",
             min(f.ages.estimated),
             "--",
             max(f.ages.estimated),
             ")"),
      length(f.ages.estimated) * n.yrs.sel.vals,
      sel_dev_bounds,
      paste0("Normal (0, ",
             model$parameters["AgeSel_P3_Fishery(1)_dev_se", "Value"],
             ")"))

  # Dirichlet-Multinomial likelihood parameters
  dm_inds <- grep("DM", ctl)
  if(!length(dm_inds)){
    stop("'DM' not found n the control file, cannot extract Dirichlet-multinomial ",
         "parameter settings",
         call. = FALSE)
  }
  dm <- map(dm_inds, ~{fetch.and.split(ctl, .x)})

  dmf <- dm[[1]]
  dms <- dm[[2]]
  dmf.vals <- c(paste0("Dirichlet-multinomial fishery likelihood, ",
                      latex.italics("$\\log(\\theta_{fish})$")),
               2,
               paste0("(", dmf[lo], ", ", dmf[hi], ")"),
               paste0("Normal (",
                      dmf[4], ", ",
                      dmf[5],
                      ")"))

  dms.vals <- c(paste0("Dirichlet-multinomial survey likelihood, ",
                      latex.italics("$\\log(\\theta_{survey})$")),
               2,
               paste0("(", dms[lo], ", ", dms[hi], ")"),
               paste0("Normal (",
                      dms[4], ", ",
                      dms[5],
                      ")"))

  tab <- rbind(r0.vals,
               h.vals,
               sig.r.vals,
               rec.dev.vals,
               m.vals,
               se.vals,
               age.sel.vals,
               se.vals.age1,
               f.age.sel.vals,
               f.age.sel.dev.vals,
               dmf.vals,
               dms.vals)

  if(!return.xtable){
    return(tab)
  }

  # Make first row empty to make the Stock Dynamics header appear below the
  #  horizontal line
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
  addtorow$pos[[1]] <- -1
  addtorow$pos[[2]] <- 1
  addtorow$pos[[3]] <- 6
  addtorow$pos[[4]] <- 6
  addtorow$pos[[5]] <- 8
  addtorow$pos[[6]] <- 9
  addtorow$pos[[7]] <- 11
  header_code <- paste0(latex.hline,
                        paste(colnames(tab), collapse = latex.amp()),
                        latex.nline,
                        latex.hline)

  header_code <- paste0(header_code,
                        latex_continue(ncol(tab), header_code))
  addtorow$command <-
    c(header_code,
      paste0(latex.bold(latex.under("Stock Dynamics")),
             latex.nline),
      paste0(latex.nline,
             latex.bold(latex.under("Data Source")),
             latex.nline),
      paste0(latex.bold(latex.italics("Acoustic Survey")),
             latex.nline),
      paste0(latex.bold(latex.italics("Age-1 Survey")),
             latex.nline),
      paste0(latex.bold(latex.italics("Fishery")),
             latex.nline),
      paste0(latex.nline,
             latex.bold(latex.under("Data weighting")),
             latex.nline))
  # Add spaces after commas and before opening parentheses
  tab <- map_df(as_tibble(tab), ~{gsub(",", ", ", .x)}) |>
    as.data.frame()
  tab <- map_df(as_tibble(tab), ~{gsub("\\(", " \\(", .x)}) |>
    as.data.frame()

  ## Make the size string for font and space size
  size.string <- latex.size.str(font.size, space.size)
  print(xtable(tab,
               caption = xcaption,
               label = xlabel,
               align = get.align(ncol(tab),
                                 just = "c")),
        caption.placement = "top",
        include.rownames = FALSE,
        include.colnames = FALSE,
        sanitize.text.function = function(x){x},
        size = size.string,
        add.to.row = addtorow,
        tabular.environment = "longtable",
        table.placement = "H",
        hline.after = NULL)
}
