#' Make the Executive Summary decision tables
#'
#' @param model An mcmc run, `model$mcmc` is the output of the [r4ss::SSgetMCMC()]
#' @param xcaption Caption to appear in the calling document
#' @param xlabel The label used to reference the table in latex
#' @param font.size Size of the font for the table
#' @param space.size Size of the vertical spaces for the table
#' @param type Type to build. `biomass` or `spr`
#' @param placement Latex code for placement of table
#'
#' @return The latex code needed to build the table
#' @export
decision_table <- function(model,
                           xcaption   = "default",
                           xlabel     = "default",
                           font.size  = 9,
                           space.size = 10,
                           type      = "biomass",
                           placement  = "H"){

  if(type != "biomass" & type != "spr"){
    stop("make.decisions.table: Error - type '",
         type,
         "' is not implemented. Stopping...\n\n")
  }
  forecast <- model$forecasts[[length(model$forecasts)]]
  if(type == "biomass"){
    num.rows <- nrow(forecast[[1]]$biomass) - 1
    table.header <- latex.bold("Resulting relative spawning biomass")
  }else{
    num.rows <- nrow(forecast[[1]]$spr) - 1
    table.header <- latex.bold("Relative fishing intensity")
  }

  # *If you add or remove a row from the table, you only need to change the next two variables*
  #
  # tab.letters are the letters in the table, one for each forecast management
  # action and a blank for all but the first year in a management action
  # additional labels are given for some rows (below letter)
  rows2Label <- c("e", "f", "h", "j", "l", "m", "n", "o")
  rowLabels <- list(c("10\\%", "reduction"),
                    c(assess.yr - 1, "catch"),
                    c("10\\%", "reduction"),
                    c("10\\%", "reduction"),
                    c(assess.yr - 1, "TAC"),
                    c("FI=", "100\\%"),
                    c("default", "HR"),
                    c(paste0("C", assess.yr, "="),
                      paste0("C", assess.yr + 1)))

  tab.letters <- NULL
  next.ind <- 1

  for(i in 1:length(forecast)){
    tab.letters[next.ind] <- paste0(letters[i], ":")
    next.ind <- next.ind + 1
    for(j in 1:(num.rows - 1)){
      if(letters[i] %in% rows2Label) {
        lab <- rowLabels[[which(letters[i] == rows2Label)]]
        tab.letters[next.ind] <- lab[j]
      } else {
        tab.letters[next.ind] <- ""
      }
      next.ind <- next.ind + 1
    }
  }
  tab.letters <- tab.letters %>%
    enframe(name = NULL, value = "labels")

  # Merge the list elements into a data frame
  forecast.tab <- map(forecast, ~{
    if(type == "biomass"){
      tmp <- .x$biomass
    }else{
      tmp <- .x$spr
    }
    tmp <- tmp %>%
      as_tibble(rownames = "Year") %>%
      select(-c("25%", "75%"))
    names(tmp) <- gsub("%", "\\\\%", names(tmp))
    first_biomass_yr <<- slice(tmp, 1)
    slice(tmp, -1)
  }) %>% map_df(~{.x})

  c.levels <- map(model$catch.levels, ~{
    tmp <- .x[[1]]
    tmp[tmp < 1] <- 0
    head(tmp, -1)
  }) %>%
    unlist %>%
    enframe(name = NULL, value = "Catch (t)")

  forecast.tab <- forecast.tab %>%
    bind_cols(c.levels, tab.letters) %>%
    mutate(Year = as.numeric(Year) - 1,
           start_yr = as.character(Year + 1)) %>%
    select(labels, Year, `Catch (t)`, start_yr, everything()) %>%
    mutate(Year = as.character(Year),
           `Catch (t)` = f(`Catch (t)`),
           `5\\%` = f(`5\\%`, 2),
           `50\\%` = f(`50\\%`, 2),
           `95\\%` = f(`95\\%`, 2))

  first_biomass_yr[ ,-1] <- as.list(f(unlist(first_biomass_yr[ ,-1]), 2))

  quant.levels <- grep("%", names(forecast.tab), value = TRUE)

  # Add the extra header spanning multiple columns
  addtorow <- list()
  addtorow$pos <- list()
  addtorow$pos[[1]] <- -1
  addtorow$pos[[2]] <- nrow(forecast.tab)

  quant.string <- ""
  quant.ampersands <- ""
  quant.cell.defs <- NULL
  for(i in 1:length(quant.levels)){
    quant.string <- paste0(quant.string,
                           latex.amp(),
                           quant.levels[i])
    quant.ampersands <- paste0(quant.ampersands,
                               latex.amp())
    quant.cell.defs <- c(quant.cell.defs, "Y")
  }
  # Add the vertical bar to the edge of the last quant cell
  quant.cell.defs[length(quant.cell.defs)] <- paste0(quant.cell.defs[length(quant.cell.defs)], "|")

  if(type == "biomass"){
    addtorow$command <- c(paste0(latex.cline("1-7"),
                                 latex.mcol(3,
                                            "|c|",
                                            ""),
                                 latex.amp(),
                                 latex.bold("Biomass at"),
                                 latex.amp(),
                                 latex.mcol(3,
                                            "c|",
                                            table.header),
                                 latex.nline,
                                 latex.cline("1-3"),
                                 latex.mcol(3,
                                            "|c|",
                                            latex.bold("Management Action")),
                                 latex.amp(),
                                 latex.bold("start of year"),
                                 latex.amp(),
                                 paste(latex.bold(quant.levels), collapse = latex.amp()),
                                 latex.nline,
                                 latex.hline,
                                 latex.amp(),
                                 latex.bold("Catch year"),
                                 latex.amp(),
                                 latex.bold("Catch (t)"),
                                 latex.amp(),
                                 paste(first_biomass_yr[1,], collapse = latex.amp()),
                                 latex.nline,
                                 latex.hline),
                          latex.hline)

  }else{
    addtorow$command <- c(paste0(latex.cline("1-6"),
                                 latex.mcol(3,
                                            "|c|",
                                            latex.bold("Management Action")),
                                 latex.amp(),
                                 latex.mcol(3,
                                            "c|",
                                            table.header),
                                 latex.nline,
                                 latex.cline("1-3"),
                                 latex.amp(),
                                 latex.bold("Catch year"),
                                 latex.amp(),
                                 latex.bold("Catch (t)"),
                                 latex.amp(),
                                 paste(latex.bold(quant.levels), collapse = latex.amp()),
                                 latex.nline,
                                 latex.hline),
                          latex.hline)
  }
  # Add the right number of horizontal lines to make the table break in the
  #  correct places
  # A line is not needed at the bottom explains (length(forecast)-1).
  for(i in 1:(length(forecast) - 1)){
    addtorow$pos[[i + 2]] <- i * num.rows
    addtorow$command <- c(addtorow$command, latex.hline)
  }

  if(type == "biomass"){
    align <- c("c",
               "|c",
               "c",
               "c|",
               "c|",
               quant.cell.defs)
  }else if(type == "spr"){
    align <- c("c",
               "|c",
               "c",
               "c|",
               quant.cell.defs)
    forecast.tab <- forecast.tab %>% select(-start_yr)
  }
  ## Make the size string for font and space size
  size.string <- latex.size.str(font.size, space.size)
  print(xtable(forecast.tab,
               caption = xcaption,
               label = xlabel,
               align = align),
        caption.placement = "top",
        include.rownames = FALSE,
        include.colnames = FALSE,
        sanitize.text.function = function(x){x},
        size = size.string,
        add.to.row = addtorow,
        table.placement = placement,
        tabular.environment = "tabularx",
        width = "\\textwidth",
        hline.after = NULL)
}

make.decision.table.pres <- function(model,
                                     model.inds,
                                     xcaption   = "default",
                                     xlabel     = "default",
                                     font.size  = 9,
                                     space.size = 10,
                                     type       = "biomass",
                                     placement  = "H"){
  ## Returns an xtable in the proper format for the management presentation in
  ##  beamer
  ## model - an mcmc run, output of the r4ss package's function SSgetMCMC()
  ## model.inds - indices of the forecast models you want to show
  ## xcaption - caption to appear in the calling document
  ## xlabel - the label used to reference the table in latex
  ## font.size - size of the font for the table
  ## space.size - size of the vertical spaces for the table
  ## type - type to build. "biomass" or "spr"
  ## placement - latex code for placement of table

  if(type != "biomass" & type != "spr"){
    stop("make.decisions.table: Error - type '",
         type,
         "' is not implemented. Stopping...\n\n")
  }
  ## The numbers below index several of the forecast catch levels
  forecast <- model$forecasts[[length(model$forecasts)]][model.inds]
  if(type == "biomass"){
    table.header1 <- latex.bold("Beginning of year")
    table.header2 <- latex.bold("relative spawning biomass")
    forecast.tab <- f(do.call("rbind",
                              lapply(forecast, "[[", "biomass")) * 100)
  }else{
    table.header1 <- latex.bold("Fishing")
    table.header2 <- latex.bold("Intensity")
    forecast.tab <- f(do.call("rbind",
                              lapply(forecast, "[[", "spr")) * 100)
  }

  ## Store years for binding later
  yrs <- rownames(forecast.tab)

  ## Append the escaped % symbols
  forecast.tab <- apply(forecast.tab,
                        2,
                        paste0,
                        "\\%")

  ## Change the quantile levels so they have correct latex escape sequence
  quant.levels <- gsub("%",
                       "\\\\%",
                       colnames(forecast.tab))

  ## Set any catch less than 1 to be 0
  c.levels <- unlist(lapply(model$catch.levels[model.inds], "[[", 1))
  c.levels[c.levels < 1] <- 0
  ## Bind the catch levels and years to the correct rows
  row.labs <- rep("", length(model.inds) * length(model$forecasts))
  row.labs[seq(1,
               length(model.inds) * length(model$forecasts),
               length(model$forecasts))] <-
    paste0(letters[model.inds], ":")

  forecast.tab <- cbind(row.labs,
                        yrs,
                        f(c.levels),
                        forecast.tab)
  colnames(forecast.tab) <- c("",
                              "Year",
                              "Catch (t)",
                              quant.levels)

  ## Add the extra header spanning multiple columns
  addtorow <- list()
  addtorow$pos <- list()
  addtorow$pos[[1]] <- -1
  addtorow$pos[[2]] <- nrow(forecast.tab)

  quant.string <- ""
  quant.cell.defs <- NULL
  for(i in 1:length(quant.levels)){
    quant.string <- paste0(quant.string,
                           latex.amp(),
                           quant.levels[i])
    quant.cell.defs <- c(quant.cell.defs,
                         "Y")
  }
  ## Add the vertical bar to the edge of the last quant cell
  quant.cell.defs[length(quant.cell.defs)] <-
    paste0(quant.cell.defs[length(quant.cell.defs)], "|")
  addtorow$command <- c(paste0(latex.hline,
                               latex.mcol(3,
                                          "|c|",
                                          "Within model quantile"),
                               quant.string,
                               latex.nline,
                               latex.hline,
                               latex.mcol(3,
                                          "|c|",
                                          "Management Action"),
                               latex.amp(),
                               latex.mcol(length(quant.levels),
                                          "c|",
                                          table.header1),
                               latex.nline,
                               latex.cline("1-3"),
                               latex.amp(),
                               "Year",
                               latex.amp(),
                               "Catch (t)",
                               latex.amp(),
                               latex.mcol(length(quant.levels),
                                          "c|",
                                          table.header2),
                               latex.nline,
                               latex.hline),
                        latex.hline)

  ## Add the right number of horizontal lines to make the table break in the
  ##  correct places
  ## A line is not needed at the bottom explains (length(forecast)-1)
  for(i in 1:(length(forecast)-1)){
    if(!is.null(forecast[[i]])){
      if(type == "biomass"){
        addtorow$pos[[i+2]] <- i * nrow(forecast[[i]]$biomass)
      }else{
        addtorow$pos[[i+2]] <- i * nrow(forecast[[i]]$spr)
      }
      addtorow$command <- c(addtorow$command, latex.hline)
    }
  }

  ## Make the size string for font and space size
  size.string <- latex.size.str(font.size, space.size)
  print(xtable(forecast.tab,
               caption = xcaption,
               label = xlabel,
               align = c("c",
                         "|c",
                         "c",
                         "c|",
                         quant.cell.defs)),
        caption.placement = "top",
        include.rownames = FALSE,
        include.colnames = FALSE,
        sanitize.text.function = function(x){x},
        size = size.string,
        add.to.row = addtorow,
        table.placement = placement,
        tabular.environment = "tabularx",
        width = "\\textwidth",
        hline.after = NULL)
}

make.risk.table <- function(model,
                            forecast.yrs,
                            index = 1,
                            xcaption   = "default",
                            xlabel     = "default",
                            font.size  = 9,
                            space.size = 10,
                            placement = "H",
                            type = 1){
  ## Returns an xtable in the proper format for the executive summary risk
  ##  tables
  ##
  ## model - an mcmc run, output of the r4ss package's function SSgetMCMC()
  ## forecast.yrs - a vector of the years which ere forecast
  ## index - Index for which risk data to use. e.g. 1 = second forecast year
  ##  compared to the first. If there were N forecast years, this can be from
  ##  1 to N-1.
  ## xcaption - caption to appear in the calling document
  ## xlabel - the label used to reference the table in latex
  ## font.size - size of the font for the table
  ## space.size - size of the vertical spaces for the table
  ## placement - latex code for placement of table
  ## type - if you want both columns of catch (i.e., type = 2),
  ## where the original doesn't and uses type = 1

  risk <- model$risks[[index]]
  ## Remove last 3 columns which are DFO values
  risk <- risk[,-((ncol(risk)-2):ncol(risk))]
  ## Fix tiny catch of less than 0.49 to zero, only for first (catch) column
  risk[risk[,1] < 0.49, 1] <- 0
  ## Format all columns except catch (1) to be zero decimal points and have a
  ##  percent sign
  risk[,-1] <- apply(apply(risk[,-1],
                           2,
                           f),
                     2,
                     paste0,
                     "\\%")
  ## Format the catch column (1) to have no decimal points and the thousands
  ##  separator
  risk[,1] <- f(as.numeric(risk[,1]))
  ## Add letters to the catch for reference with the decision tables
  risk[,1] <- paste0(letters[1:nrow(risk)],
                     ": ",
                     risk[,1])
  if (type == 2 && length(model$risks) > 2) stop("This function was",
    "not written to work with more than two projections when you want",
    "to display multiple years of catch in a single table.")
  risk2 <- cbind(
    f(sapply(model$risks, "[", 1:nrow(model$risks[[1]]))),
    apply(model$risks[[index]][, 2:(ncol(model$risks[[index]])-3)],
      1:2, function(x) paste0(f(x), "\\%")))
  colnames(risk2) <- gsub("^(\\d{4}$)$", "Catch in \\1", colnames(risk2))
  risk2[, 1] <- paste0(letters[1:nrow(risk2)], ": ", risk2[, 1])
  addtorow2 <- list("pos" = list(-1, nrow(risk2)),
    "command" = c(paste0("\\toprule \n",
    paste(sapply(
        gsub("1\\.00", "100\\\\%",
        gsub("_(\\d{4})", "\\\\subscr{\\1}",
        gsub("0\\.(\\d{2})", "B\\\\subscr{\\1\\\\%}",
        gsub("ForeCatch_(\\d{4})", "\\1 catch",
        gsub("SSB|Bratio", "B",
        gsub("^Bratio(.+)<", "Prob. B\\1 <",
        gsub("^SSB(.+)<", "Prob. B\\1 <",
        gsub("^ForeCatch_(\\d{4})", "Prob. \\1 default harvest policy catch ",
        gsub("SPRratio_(\\d{4})", "Prob. \\1 relative fishing intensity ",
        colnames(risk2)))))))))), latex.bold),
    collapse = latex.amp()), latex.nline,
    "\\midrule \n"), "\\bottomrule \n"))

  addtorow <- list()
  addtorow$pos <- list()
  addtorow$pos[[1]] <- -1
  addtorow$pos[[2]] <- nrow(risk)
  addtorow$command <-
    c(paste0("\\toprule \n",
             latex.mlc(c("Catch", paste("in",
                                        forecast.yrs[index]))),
             latex.amp(),
             latex.mlc(c("Probability",
                         paste0(latex.subscr("B", forecast.yrs[index+1]),
                                " < ",
                                latex.subscr("B", forecast.yrs[index])))),
             latex.amp(),
             latex.mlc(c("Probability",
                         paste0(latex.subscr("B", forecast.yrs[index+1]),
                                " < ",
                                latex.subscr("B", "40\\%")))),
             latex.amp(),
             latex.mlc(c("Probability",
                         paste0(latex.subscr("B", forecast.yrs[index+1]),
                                " < ",
                                latex.subscr("B", "25\\%")))),
             latex.amp(),
             latex.mlc(c("Probability",
                         paste0(latex.subscr("B", forecast.yrs[index+1]),
                                " < ",
                                latex.subscr("B", "10\\%")))),
             latex.amp(),
             latex.mlc(c("Probability",
                         paste(forecast.yrs[index], "relative"),
                         "fishing",
                         "intensity",
                         " > 100\\%")),
             latex.amp(),
             latex.mlc(c("Probability",
                         paste(forecast.yrs[index + 1], "default"),
                         "harvest policy",
                         "catch",
                         paste0(" < ", forecast.yrs[index], " catch"))),
             latex.nline,
             "\\midrule \n"),
      "\\bottomrule \n")

  ## Make the size string for font and space size
  size.string <- latex.size.str(font.size, space.size)
  align <- get.align(ncol(risk), first.left = TRUE, just = "Y")
  if (type == 2) {
    risk <- risk2
    addtorow <- addtorow2
    align <- c("l", "p{2cm}",
      rep("p{1.4cm}", ncol(risk) - 3), rep("p{2.2cm}", 2))
  }

  print(xtable(risk,
               caption = xcaption,
               label = xlabel,
               align = align),
        caption.placement = "top",
        include.rownames = FALSE,
        include.colnames = FALSE,
        sanitize.text.function = function(x){x},
        size = size.string,
        add.to.row = addtorow,
        table.placement = placement,
        hline.after = NULL,
        tabular.environment = "tabularx",
        width = "\\textwidth",
        booktabs = TRUE)
}
