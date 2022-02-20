#' Create the Executive Summary decision tables. `rows_to_label` and `row_labels`
#' need changing each year.
#'
#' @param model An mcmc run, `model$forecasts` must be populated from [run_forecasts()]
#' @param xcaption Caption to appear in the calling document
#' @param xlabel The label used to reference the table in latex
#' @param font.size Size of the font for the table
#' @param space.size Size of the vertical spaces for the table
#' @param type Type to build. `biomass` or `spr`
#' @param placement Latex code for placement of table
#' @param forecast_inds Indices of forecast catch levels to show. By default all
#' catch levels will be shown
#' @param rows_to_label A vector of letters representing rows to add custom labels to
#' @param row_labels A list of length two vectors with text to place in the empty
#' two rows under the letter for the letters given in `rows_to_label`
#' @return The latex code needed to build the table
#' @export
decision_table <- function(model,
                           xcaption = "default",
                           xlabel = "default",
                           font.size = 9,
                           space.size = 10,
                           type = "biomass",
                           placement = "H",
                           forecast_inds = seq_along(model$forecasts[[length(model$forecasts)]]),
                           rows_to_label = c("e", "f", "h", "j", "l", "m", "n", "o"),
                           row_labels = list(c("10\\%", "reduction"),
                                             c(assess.yr - 1, "catch"),
                                             c("10\\%", "reduction"),
                                             c("10\\%", "reduction"),
                                             c(assess.yr - 1, "TAC"),
                                             c("FI=", "100\\%"),
                                             c("default", "HR"),
                                             c(paste0("C", assess.yr, "="),
                                               paste0("C", assess.yr + 1)))){

  if(length(rows_to_label) != length(row_labels)){
    stop("rows_to_label vector must be the same length as row_labels list")
  }

  if(!all(rows_to_label %in% letters)){
    stop("All characters in rows_to_label must be a single letter of the alphabet")
  }

  if(type != "biomass" & type != "spr"){
    stop("type '", type, "' is not implemented")
  }
  if(any(forecast_inds > length(model$forecasts[[length(model$forecasts)]]))){
    stop("forecast_inds contains values greater than the length of the forecast catch levels list")
  }

  forecast <- model$forecasts[[length(model$forecasts)]][forecast_inds]
  if(type == "biomass"){
    num.rows <- nrow(forecast[[1]]$biomass) - 1
    table.header <- latex.bold("Resulting relative spawning biomass")
  }else{
    num.rows <- nrow(forecast[[1]]$spr) - 1
    table.header <- latex.bold("Relative fishing intensity")
  }

  tab.letters <- NULL
  next.ind <- 1

  for(i in forecast_inds){
    tab.letters[next.ind] <- paste0(letters[i], ":")
    next.ind <- next.ind + 1
    for(j in 1:(num.rows - 1)){
      if(letters[i] %in% rows_to_label) {
        lab <- row_labels[[which(letters[i] == rows_to_label)]]
        tab.letters[next.ind] <- lab[j]
      } else {
        tab.letters[next.ind] <- ""
      }
      next.ind <- next.ind + 1
    }
  }
  tab.letters <- tab.letters %>%
    enframe(name = NULL, value = "labels")

  c.levels <- map(model$catch.levels[forecast_inds], ~{
    tmp <- .x[[1]]
    tmp[tmp < 1] <- 0
    head(tmp, -1)
  }) %>%
    unlist %>%
    enframe(name = NULL, value = "Catch (t)")

  # Merge the list elements into a data frame
  if(type == "biomass"){
    forecast.tab <- map(forecast, ~{
      tmp <- .x$biomass
      tmp <- tmp %>%
        as_tibble(rownames = "Year") %>%
        select(-c("25%", "75%"))
      names(tmp) <- gsub("%", "\\\\%", names(tmp))
      first_biomass_yr <<- slice(tmp, 1)
      slice(tmp, -1)
    }) %>% map_df(~{.x})

    forecast.tab <- forecast.tab %>%
      bind_cols(c.levels, tab.letters) %>%
      mutate(Year = as.numeric(Year) - 1,
             start_yr = paste("Start of", as.character(Year + 1))) %>%
      select(labels, Year, `Catch (t)`, start_yr, everything()) %>%
      mutate(Year = as.character(Year),
             `Catch (t)` = f(`Catch (t)`),
             `5\\%` = f(`5\\%`, 2),
             `50\\%` = f(`50\\%`, 2),
             `95\\%` = f(`95\\%`, 2))

    first_biomass_yr[, -1] <- as.list(f(unlist(first_biomass_yr[ ,-1]), 2))
    first_biomass_yr[1] <- paste("Start of", first_biomass_yr[1])

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

    addtorow$command <- c(
      paste0(latex.cline("1-7"),
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
             paste(first_biomass_yr[1, ], collapse = latex.amp()),
             latex.nline,
             latex.hline),
      latex.hline)

  }else if(type == "spr"){
    forecast.tab <- map(forecast, ~{
      tmp <- .x$spr
      tmp <- tmp %>%
        as_tibble(rownames = "Year") %>%
        select(-c("25%", "75%"))
      names(tmp) <- gsub("%", "\\\\%", names(tmp))
      slice(tmp, -nrow(tmp))
    }) %>% map_df(~{.x})

    forecast.tab <- forecast.tab %>%
      bind_cols(c.levels, tab.letters) %>%
      mutate(Year = as.numeric(Year),
             start_yr = paste("Start of", as.character(Year + 1))) %>%
      select(labels, Year, `Catch (t)`, start_yr, everything()) %>%
      mutate(Year = as.character(Year),
             `Catch (t)` = f(`Catch (t)`),
             `5\\%` = f(`5\\%`, 2),
             `50\\%` = f(`50\\%`, 2),
             `95\\%` = f(`95\\%`, 2))


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

  # Add the right number of horizontal lines to make the table break in the correct places
  # A line is not needed at the bottom explains (length(forecast)-1) in the loop.
  if(length(forecast_inds) > 1){
    for(i in 1:(length(forecast_inds) - 1)){
      addtorow$pos[[i + 2]] <- i * num.rows
      addtorow$command <- c(addtorow$command, latex.hline)
    }
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

#' Creates LaTeX code to make a probability of risk table with various probabilities
#' of things happening with the stock
#'
#' @param model A model from this project
#' @param forecast.yrs A vector of forecast years
#' @param index Index for which forecast year data to use. e.g. 1 = second forecast year
#' compared to the first. If there were N forecast years, this can be from 1 to N-1.
#' @param xcaption Caption to appear in the calling document
#' @param xlabel Label used to reference the table in LaTeX
#' @param font.size Point size of font in table
#' @param space.size Vertical space between rows
#' @param placement LaTeX code for placement of the table, e.g. "H" or "tbp"
#' @param type If you want both columns of catch (i.e., type = 2),
#' where the original doesn't and uses type = 1
#'
#' @return LaTeX code to render the table
#' @export
make.risk.table <- function(model,
                            forecast.yrs,
                            index = 1,
                            xcaption   = "default",
                            xlabel     = "default",
                            font.size  = 9,
                            space.size = 10,
                            placement = "H",
                            type = 1){

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
