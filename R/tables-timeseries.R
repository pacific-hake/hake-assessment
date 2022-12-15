#' Make a latex table of credible intervals with columns for female spawning biomass,
#' relative spawning biomass, total biomass, age 2+ biomass, age-0 recruits, relative
#' fishing intensity, and exploitation fraction. Also output a CSV file with the table contents.
#'
#' @param model A model list as output by [load_models()]
#' @param start_yr Start year in the table
#' @param end_yr End year in the table
#' @param weight.factor Factor to divide biomass values by and multiply the recruitment by
#' @param xcaption Caption for the latex table
#' @param xlabel Label for the latex table
#' @param font.size Size of font
#' @param space.size Vertical spacing between text in table
#' @param digits Number of decimal points to round to
#' @param lower_col Character string name of the column containing the lower credible interval values
#' @param upper_col Character string name of the column containing the upper credible interval values
#'
#' @return An [xtable::xtable()]
#' @export
make.ci.posterior.table <- function(model,
                                    start_yr,
                                    end_yr,
                                    weight.factor = 1000,
                                    xcaption = "default",
                                    xlabel   = "default",
                                    font.size = 9,
                                    space.size = 10,
                                    digits = 1,
                                    lower_col = NULL,
                                    upper_col = NULL){

  stopifnot(!is.null(lower_col))
  stopifnot(!is.null(upper_col))

  lower_col_sym <- sym(lower_col)
  upper_col_sym <- sym(upper_col)
  yrs <- start_yr:end_yr
  df <- map(model$mcmccalcs, ~{.x[names(.x) %in% yrs]})
  ts <- model$timeseries

  tot_bm <- model$extra.mcmc$total_biomass_quants %>%
    filter(Yr %in% yrs)
  stopifnot(lower_col %in% names(tot_bm))
  stopifnot(upper_col %in% names(tot_bm))
  tot_bm_lower <- tot_bm %>%
    pull(!!lower_col_sym)
  tot_bm_upper <- tot_bm %>%
    pull(!!upper_col_sym)

  age2plus_bm <- model$extra.mcmc$total_age2_plus_biomass_quants %>%
    filter(Yr %in% yrs)
  stopifnot(lower_col %in% names(age2plus_bm))
  stopifnot(upper_col %in% names(age2plus_bm))
  age2plus_bm_lower <- age2plus_bm %>%
    pull(!!lower_col_sym)
  age2plus_bm_upper <- age2plus_bm %>%
    pull(!!upper_col_sym)

  tab.filt <- cbind(yrs,
                    paste0(f(df$slower * weight.factor),
                           "-",
                           f(df$supper * weight.factor)),
                    paste0(f(df$dlower * 100, digits),
                           "-",
                           f(df$dupper * 100, digits), "\\%"),
                    paste0(f(tot_bm_lower / weight.factor),
                           "-",
                           f(tot_bm_upper / weight.factor)),
                    paste0(f(age2plus_bm_lower / weight.factor),
                           "-",
                           f(age2plus_bm_upper / weight.factor)),
                    paste0(f(df$rlower * weight.factor),
                           "-",
                           f(df$rupper * weight.factor)),
                    paste0(f(df$plower * 100, digits),
                           "-",
                           f(df$pupper * 100, digits),
                           "\\%"),
                    paste0(f(df$flower * 100, digits),
                           "-",
                           f(df$fupper * 100, digits),
                           "\\%"))

  ## Make current year have dashes for exploitation rate and fishing intensity
  tab.filt[nrow(tab.filt), ncol(tab.filt)] <- latex.bold("--")
  tab.filt[nrow(tab.filt), ncol(tab.filt) - 1] <- latex.bold("--")

  ## Add latex headers
  colnames(tab.filt) <- c(latex.bold("Year"),
                          latex.mlc(c("Female",
                                      "spawning",
                                      "biomass",
                                      "(thousand t)")),
                          latex.mlc(c("Relative",
                                      "spawning",
                                      "biomass")),
                          latex.mlc(c("Total",
                                      "biomass",
                                      "(thousand t)")),
                          latex.mlc(c("Age-2+",
                                      "biomass",
                                      "(thousand t)")),
                          latex.mlc(c("Age-0",
                                      "recruits",
                                      "(millions)")),
                          latex.mlc(c("(1-SPR)",
                                      "/",
                                      paste0("(1-",
                                             latex.subscr("SPR", "40\\%"),
                                             ")"))),
                          latex.mlc(c("Exploitation",
                                      "fraction")))

  size.string <- latex.size.str(font.size, space.size)
  last_row <- tab.filt[nrow(tab.filt),]
  last_row <- gsub("-", " - ", last_row)
  last_row <- gsub(" -  - ", "--", last_row)
  tab.filt <- tab.filt[1:(nrow(tab.filt) - 1),]
  tab.filt <- map_df(as_tibble(tab.filt), ~{gsub("-", " - ", .x)}) %>%
    as.data.frame %>%
    bind_rows(last_row)

  addtorow <- list()
  addtorow$pos <- list()
  addtorow$pos[[1]] <- -1
  addtorow$command <-
    paste0(latex.hline,
           paste(colnames(tab.filt), collapse = latex.amp()),
           latex.nline,
           latex.hline)

  addtorow$command <- paste0(addtorow$command,
                             latex_continue(ncol(tab.filt), addtorow$command))

  print(xtable(tab.filt,
               caption = xcaption,
               label = xlabel,
               align = get.align(ncol(tab.filt)),
               digits = digits),
        caption.placement = "top",
        include.rownames = FALSE,
        include.colnames = FALSE,
        add.to.row = addtorow,
        table.placement = "H",
        tabular.environment = "longtable",
        sanitize.text.function = function(x){x},
        size = size.string,
        hline.after = NULL)
}

#' Make a latex table of medians with columns for female spawning biomass,
#' relative spawning biomass, total biomass, age 2+ biomass, age-0 recruits, relative
#' fishing intensity, and exploitation fraction. Also output a CSV file with the table contents.
#'
#' @param model A model list as output by [load_models()]
#' @param start_yr Start year in the table
#' @param end_yr End year in the table
#' @param weight.factor Factor to divide biomass values by and multiply the recruitment by
#' @param csv.dir Directory for outputting the CSV version of the table
#' @param xcaption Caption for the latex table
#' @param xlabel Label for the latex table
#' @param font.size Size of font
#' @param space.size Vertical spacing between text in table
#' @param digits Number of decimal points to round to
#'
#' @return An [xtable::xtable()]
#' @export
make.median.posterior.table <- function(model,
                                        start_yr = NULL,
                                        end_yr = NULL,
                                        weight.factor = 1000,
                                        csv.dir = NULL,
                                        xcaption = "default",
                                        xlabel = "default",
                                        font.size = 9,
                                        space.size = 10,
                                        digits = 1){

  if(!is.null(csv.dir)){
    if(!dir.exists(csv.dir)){
      dir.create(csv.dir)
    }
  }

  yrs <- start_yr:end_yr
  df <- map(model$mcmccalcs, ~{.x[names(.x) %in% yrs]})
  ts <- model$timeseries
  tot_bm <- model$extra.mcmc$total_biomass_quants %>%
    filter(Yr %in% yrs) %>%
    select(`0.5`)
  age2plus_bm <- model$extra.mcmc$total_age2_plus_biomass_quants %>%
    filter(Yr %in% yrs) %>%
    select(`0.5`)

  tab.filt <- cbind(yrs,
                    f(df$smed * weight.factor),
                    paste0(f(df$dmed * 100, digits), "\\%"),
                    f(tot_bm / weight.factor),
                    f(age2plus_bm / weight.factor),
                    f(df$rmed * weight.factor),
                    paste0(f(df$pmed * 100, digits), "\\%"),
                    paste0(f(df$fmed * 100, digits), "\\%"))

  ## Make current year have dashes for exploitation rate and fishing intensity
  tab.filt[nrow(tab.filt), ncol(tab.filt)] <- latex.bold("--")
  tab.filt[nrow(tab.filt), ncol(tab.filt) - 1] <- latex.bold("--")

  ## Add latex headers
  colnames(tab.filt) <- c(latex.bold("Year"),
                          latex.mlc(c("Female",
                                      "spawning",
                                      "biomass",
                                      "(thousand t)")),
                          latex.mlc(c("Relative",
                                      "spawning",
                                      "biomass")),
                          latex.mlc(c("Total",
                                      "biomass",
                                      "(thousand t)")),
                          latex.mlc(c("Age-2+",
                                      "biomass",
                                      "(thousand t)")),
                          latex.mlc(c("Age-0",
                                      "recruits",
                                      "(millions)")),
                          latex.mlc(c("Relative",
                                      "fishing",
                                      "intensity")),
                          latex.mlc(c("Exploitation",
                                      "fraction")))

  size.string <- latex.size.str(font.size, space.size)

  # Write the median posteriors CSV file --------------------------------------
  if(!is.null(csv.dir)){
    dat <- cbind(yrs,
                 df$smed * weight.factor,
                 df$dmed * 100,
                 tot_bm / weight.factor,
                 age2plus_bm / weight.factor,
                 df$rmed * weight.factor,
                 df$pmed * 100,
                 df$fmed * 100)
    # Remove last year from Rel. Fishing intensity and Exploitation fraction columns
    dat[nrow(dat), (ncol(dat)-1):ncol(dat)] <- NA
    colnames(dat) <- c("Year",
                       "Female spawning biomass (thousand t)",
                       "Relative spawning biomass (%)",
                       "Total biomass (thousand t)",
                       "Age-2+ biomass (thousand t)",
                       "Age-0 recruits (millions)",
                       "Relative fishing intensity (%)",
                       "Exploitation fraction (%)")
    write.csv(dat,
              file.path(csv.dir,
                        "median-population-estimates.csv"),
              row.names = FALSE,
              na = "")
  }

  addtorow <- list()
  addtorow$pos <- list()
  addtorow$pos[[1]] <- -1
  addtorow$command <-
    paste0(latex.hline,
           paste(colnames(tab.filt), collapse = latex.amp()),
           latex.nline,
           latex.hline)

  addtorow$command <- paste0(addtorow$command,
                             latex_continue(ncol(tab.filt), addtorow$command))

  print(xtable(tab.filt,
               caption = xcaption,
               label = xlabel,
               align = get.align(ncol(tab.filt)),
               digits = digits),
        caption.placement = "top",
        include.rownames = FALSE,
        include.colnames = FALSE,
        add.to.row = addtorow,
        table.placement = "H",
        tabular.environment = "longtable",
        sanitize.text.function = function(x){x},
        size = size.string,
        hline.after = NULL)
}

make.biomass.table <- function(model,
                               start_yr,
                               end_yr,
                               weight.factor = 1000,
                               xcaption = "default",
                               xlabel   = "default",
                               font.size = 9,
                               space.size = 10,
                               digits = 1,
                               placement = "H"){
  ## Returns an xtable in the proper format for the executive summary biomass
  ##  values for the base case mcmc biomass quantiles
  ##
  ## model - an mcmc run, output of the r4ss package's function SSgetMCMC()
  ## start_yr - the first year to show in the table
  ## end_yr - the last year to show in the table
  ## weight.factor - divide catches by this factor
  ## xcaption - caption to appear in the calling document
  ## xlabel - the label used to reference the table in latex
  ## font.size - size of the font for the table
  ## space.size - size of the vertical spaces for the table
  ## digits - number of decimal points on % columns
  ## placement - latex code for placement of the table in document

  calcs <- model$mcmccalcs
  slower <- calcs$slower * weight.factor
  smed <- calcs$smed * weight.factor
  supper <- calcs$supper * weight.factor
  ## Depletion quantiles
  dlower <- calcs$dlower * 100
  dmed <- calcs$dmed * 100
  dupper <- calcs$dupper * 100

  ## Join the values and apply the formatiing
  tab <- t(rbind(f(slower, digits),
                 f(smed, digits),
                 f(supper, digits),
                 paste0(f(dlower, digits), "\\%"),
                 paste0(f(dmed, digits), "\\%"),
                 paste0(f(dupper, digits), "\\%")))

  ## Filter for correct years to show and make thousand-seperated numbers
  ##  (year assumed to be column 1)
  tab.filt <- tab[match(start_yr:end_yr, rownames(tab)),]

  ## Add year as a column
  tab.filt <- cbind.data.frame(rownames(tab.filt), tab.filt)
  names(tab.filt)[1] <- "year"

  colnames(tab.filt) <- c("",
                          latex.mlc(c(latex.supscr("2.5", "th"),
                                      "percentile")),
                          latex.bold("Median"),
                          latex.mlc(c(latex.supscr("97.5", "th"),
                                      "percentile")),
                          latex.mlc(c(latex.supscr("2.5", "th"),
                                      "percentile")),
                          latex.bold("Median"),
                          latex.mlc(c(latex.supscr("97.5", "th"),
                                      "percentile")))
  addtorow <- list()
  addtorow$pos <- list()
  addtorow$pos[[1]] <- -1
  addtorow$pos[[2]] <- nrow(tab.filt)
  addtorow$command <-
    c(paste0("\\toprule ",
             latex.mrow(3, "*", latex.bold("Year")),
             latex.amp(),
             latex.mcol(3, "c", latex.mlc(c("Spawning biomass",
                                            "(thousand t)"))),
             latex.amp(),
             latex.mcol(3, "c", latex.mlc(c("Relative spawning biomass",
                                            paste0("(",
                                                   latex.subscr("B", "t"),
                                                   "/",
                                                   latex.subscr("B", "0"),
                                                   ")")))),
             latex.nline,
             latex.cmidr("2-4", "r"),
             latex.cmidr("5-7", "l")),
      "\\bottomrule")

  size.string <- latex.size.str(font.size, space.size)
  print(xtable(tab.filt,
               caption = xcaption,
               label = xlabel,
               align = get.align(ncol(tab.filt)),
               digits = digits),
        caption.placement = "top",
        add.to.row = addtorow,
        table.placement = placement,
        include.rownames = FALSE,
        sanitize.text.function = function(x){x},
        size = size.string,
        hline.after = c(0),  ## Single line after the column headers
        booktabs = TRUE)
}

make.recruitment.table <- function(model,
                                   start_yr,
                                   end_yr,
                                   weight.factor = 1000,
                                   xcaption = "default",
                                   xlabel   = "default",
                                   font.size = 9,
                                   space.size = 10,
                                   digits = 1,
                                   digits.dev = 3,
                                   placement = "H"){
  ## Returns an xtable in the proper format for the executive summary
  ##  recruitment and deviations values for the base case mcmc
  ##
  ## model - an mcmc run, output of the r4ss package's function SSgetMCMC()
  ## start_yr - the first year to show in the table
  ## end_yr - the last year to show in the table
  ## weight.factor - divide catches by this factor
  ## xcaption - caption to appear in the calling document
  ## xlabel - the label used to reference the table in latex
  ## font.size - size of the font for the table
  ## space.size - size of the vertical spaces for the table
  ## digits - number of decimal points on recruitment
  ## digits.dev - number of decimal points on recruitment devs
  ## placement - latex code for placement of the table in document

  yrs <- start_yr:end_yr
  calcs <- model$mcmccalcs
  ## Recruitment quantiles
  rlower <- calcs$rlower * weight.factor
  rmed <- calcs$rmed * weight.factor
  rupper <- calcs$rupper * weight.factor
  ## Only include start year to end year
  rlower <- rlower[names(rlower) %in% yrs]
  rmed <- rmed[names(rmed) %in% yrs]
  rupper <- rupper[names(rupper) %in% yrs]

  ## Deviations quantiles
  devlower <- calcs$devlower
  devmed <- calcs$devmed
  devupper <- calcs$devupper

  ## Remove recruitment deviations prior to the start year
  devlower <- devlower[names(devlower) %in% yrs]
  devmed <- devmed[names(devmed) %in% yrs]
  devupper <- devupper[names(devupper) %in% yrs]

  ## Join the values and apply the formatiing
  tab <- t(rbind(f(rlower, digits),
                 f(rmed, digits),
                 f(rupper, digits),
                 f(devlower, digits.dev),
                 f(devmed, digits.dev),
                 f(devupper, digits.dev)))

  ## Filter for correct years to show and make thousand-seperated numbers
  ##  year assumed to be column 1)
  tab.filt <- tab[match(start_yr:end_yr, rownames(tab)),]

  ## Add year as a column
  tab.filt <- cbind.data.frame(rownames(tab.filt), tab.filt)
  names(tab.filt)[1] <- "year"

  colnames(tab.filt) <- c("",
                          latex.mlc(c(latex.supscr("2.5", "th"),
                                      "percentile")),
                          latex.bold("Median"),
                          latex.mlc(c(latex.supscr("97.5", "th"),
                                      "percentile")),
                          latex.mlc(c(latex.supscr("2.5", "th"),
                                      "percentile")),
                          latex.bold("Median"),
                          latex.mlc(c(latex.supscr("97.5", "th"),
                                      "percentile")))

  addtorow <- list()
  addtorow$pos <- list()
  addtorow$pos[[1]] <- -1
  addtorow$pos[[2]] <- nrow(tab.filt)
  addtorow$command <-
    c(paste0("\\toprule ",
             latex.mrow(3, "*", latex.bold("Year")),
             latex.amp(),
             latex.mcol(3, "c", latex.mlc(c("Absolute recruitment",
                                            "(millions)"))),
             latex.amp(),
             latex.mcol(3, "c", latex.bold("Recruitment deviations")),
             latex.nline,
             latex.cmidr("2-4", "r"),
             latex.cmidr("5-7", "l")),
      "\\bottomrule")

  size.string <- latex.size.str(font.size, space.size)
  print(xtable(tab.filt,
               caption = xcaption,
               label = xlabel,
               align = get.align(ncol(tab.filt)),
               digits = digits),
        caption.placement = "top",
        add.to.row = addtorow,
        table.placement = placement,
        include.rownames = FALSE,
        sanitize.text.function = function(x){x},
        size = size.string,
        hline.after = c(0),  ## Single line after the column headers
        booktabs = TRUE)
}

make.fishing.intensity.table <- function(model,
                                         start_yr,
                                         end_yr,
                                         xcaption = "default",
                                         xlabel   = "default",
                                         font.size = 9,
                                         space.size = 10,
                                         digits = 1,
                                         placement = "H"){
  ## Returns an xtable in the proper format for the executive summary fishing
  ##  intensity and exploitation fraction values for the base case mcmc
  ##  fishing intensity quantiles
  ##
  ## model - an mcmc run, output of the r4ss package's function SSgetMCMC()
  ## start_yr - the first year to show in the table
  ## end_yr - the last year to show in the table
  ## weight.factor - divide catches by this factor
  ## xcaption - caption to appear in the calling document
  ## xlabel - the label used to reference the table in latex
  ## font.size - size of the font for the table
  ## space.size - size of the vertical spaces for the table
  ## digits - number of decimal points on recruitment
  ## digits.dev - number of decimal points on recruitment devs
  ## placement - latex code for placement of the table in document

  calcs <- model$mcmccalcs
  plower <- calcs$plower
  pmed <- calcs$pmed
  pupper <- calcs$pupper
  ## Exploitation fraction quantiles
  flower <- calcs$flower
  fmed <- calcs$fmed
  fupper <- calcs$fupper

  yrs <- start_yr:end_yr

  ## remove prepended strings from year labels
  names(plower) <- gsub("SPRratio_", "", names(plower))
  names(pmed) <- gsub("SPRratio_", "", names(pmed))
  names(pupper) <- gsub("SPRratio_", "", names(pupper))

  names(flower) <- gsub("F_", "", names(flower))
  names(fmed) <- gsub("F_", "", names(fmed))
  names(fupper) <- gsub("F_", "", names(fupper))

  ## Remove any projection years from SPR tables
  plower <- plower[(names(plower) %in% yrs)]
  pmed <- pmed[(names(pmed) %in% yrs)]
  pupper <- pupper[(names(pupper) %in% yrs)]

  ## Remove any projection years from F tables
  flower <- flower[(names(flower) %in% yrs)]
  fmed <- fmed[(names(fmed) %in% yrs)]
  fupper <- fupper[(names(fupper) %in% yrs)]

  ## Join the values and apply the formatiing
  tab <- t(rbind(f(plower, digits),
                 f(pmed, digits),
                 f(pupper, digits),
                 f(flower, digits),
                 f(fmed, digits),
                 f(fupper, digits)))

  ## Filter for correct years to show
  tab.filt <- tab[match(start_yr:end_yr, rownames(tab)),]

  ## Add year as a column
  tab.filt <- cbind.data.frame(rownames(tab.filt), tab.filt)
  names(tab.filt)[1] <- "year"

  colnames(tab.filt) <- c("",
                          latex.mlc(c(latex.supscr("2.5", "th"),
                                      "percentile")),
                          latex.bold("Median"),
                          latex.mlc(c(latex.supscr("97.5", "th"),
                                      "percentile")),
                          latex.mlc(c(latex.supscr("2.5", "th"),
                                      "percentile")),
                          latex.bold("Median"),
                          latex.mlc(c(latex.supscr("97.5", "th"),
                                      "percentile")))

  addtorow <- list()
  addtorow$pos <- list()
  addtorow$pos[[1]] <- -1
  addtorow$pos[[2]] <- nrow(tab.filt)
  addtorow$command <-
    c(paste0("\\toprule ",
             latex.mrow(3, "*", latex.bold("Year")),
             latex.amp(),
             latex.mcol(3, "c", latex.bold("Relative fishing intensity")),
             latex.amp(),
             latex.mcol(3, "c", latex.bold("Exploitation fraction")),
             latex.nline,
             latex.cmidr("2-4", "r"),
             latex.cmidr("5-7", "l")),
      "\\bottomrule")

  size.string <- latex.size.str(font.size, space.size)
  print(xtable(tab.filt,
               caption = xcaption,
               label = xlabel,
               align = get.align(ncol(tab.filt)),
               digits = digits),
        caption.placement = "top",
        add.to.row = addtorow,
        table.placement = placement,
        include.rownames = FALSE,
        sanitize.text.function = function(x){x},
        size = size.string,
        hline.after = c(0),  ## Single line after the column headers
        booktabs = TRUE)
}
