#' Create the Executive Summary decision tables. `rows_to_label` and
#' `row_labels` need changing each year.
#'
#' @param model An mcmc run, `model$forecasts` must be populated from
#' [run_forecasts()]
#' @param font_size Size of the font for the table
#' @param space_size Size of the vertical spaces for the table
#' @param type Type to build. `biomass` or `spr`
#' @param placement Latex code for placement of table
#' @param forecast_inds Indices of forecast catch levels to show. By
#' default all catch levels will be shown
#' @param rows_to_label A vector of letters representing rows to add
#' custom labels to
#' @param row_labels A list of length two vectors with text to place in the
#' empty
#' @param rows_to_show A vector of letters of rows to show. If `NULL`, show
#' all rows
#' @param ... arguments passed to [xtable::xtable()]
#'
#' @return The latex code needed to build the table
#' @export
table_decision_original <- function(
    model,
    font_size = 9,
    space_size = 10,
    type = "biomass",
    placement = "H",
    forecast_inds = seq_along(model$forecasts[[length(model$forecasts)]]),
    rows_to_show = NULL,
    rows_to_label = c("e", "f", "h", "j", "l", "m", "n", "o"),
    row_labels = list(c("10\\%", "reduction"),
                      c(assess_yr - 1, "catch"),
                      c("10\\%", "reduction"),
                      c("10\\%", "reduction"),
                      c(assess_yr - 1, "TAC"),
                      c("FI=", "100\\%"),
                      c("default", "HR"),
                      c(paste0("C", assess_yr, "="),
                        paste0("C", assess_yr + 1))),
    ...){

  if(length(rows_to_label) != length(row_labels)){
    stop("rows_to_label vector must be the same length as row_labels list")
  }

  if(!all(rows_to_label %in% letters)){
    stop("All characters in rows_to_label must be a single letter ",
         "of the alphabet")
  }

  if(type != "biomass" & type != "spr"){
    stop("type `", type, "` is not implemented")
  }
  if(any(forecast_inds > length(model$forecasts[[length(model$forecasts)]]))){
    stop("forecast_inds contains values greater than the length of the ",
         "forecast catch levels list")
  }

  forecast <- model$forecasts[[length(model$forecasts)]][forecast_inds]
  if(type == "biomass"){
    num_rows <- nrow(forecast[[1]]$biomass) - 1
    table_header <- latex_bold("Resulting relative spawning biomass")
  }else{
    num_rows <- nrow(forecast[[1]]$spr) - 1
    table_header <- latex_bold("Relative fishing intensity")
  }

  tab_letters <- NULL
  next_ind <- 1

  for(i in forecast_inds){
    tab_letters[next_ind] <- paste0(letters[i], ":")
    next_ind <- next_ind + 1
    for(j in 1:(num_rows - 1)){
      if(letters[i] %in% rows_to_label) {
        lab <- row_labels[[which(letters[i] == rows_to_label)]]
        tab_letters[next_ind] <- lab[j]
      } else {
        tab_letters[next_ind] <- ""
      }
      next_ind <- next_ind + 1
    }
  }
  tab_letters <- tab_letters |>
    enframe(name = NULL, value = "labels")

  c_levels <- map(model$forecasts[[3]], ~{
    .x$fore_catch$catch
  }) |>
    unlist() |>
    enframe(name = NULL, value = "Catch (t)")

  # Merge the list elements into a data frame
  if(type == "biomass"){
    forecast_tab <- map(forecast, ~{
      tmp <- .x$biomass
      tmp <- tmp |>
        as_tibble() |>
        select(-c("25%", "75%"))
      names(tmp) <- gsub("%", "\\\\%", names(tmp))
      first_biomass_yr <<- slice(tmp, 1)
      slice(tmp, -1)
    }) |>
      map_df(~{.x})

    forecast_tab <- forecast_tab |>
      bind_cols(c_levels, tab_letters) %>%
      mutate(yr = as.numeric(yr) - 1,
             start_yr = paste("Start of", as.character(yr + 1))) %>%
      select(labels, yr, `Catch (t)`, start_yr, everything()) %>%
      mutate(yr = as.character(yr),
             `Catch (t)` = f(`Catch (t)`),
             `5\\%` = f(`5\\%`, 2),
             `50\\%` = f(`50\\%`, 2),
             `95\\%` = f(`95\\%`, 2))

    first_biomass_yr[, -1] <- as.list(f(unlist(first_biomass_yr[ ,-1]), 2))
    first_biomass_yr[1] <- paste("Start of", first_biomass_yr[1])

    quant_levels <- grep("%", names(forecast_tab), value = TRUE)

    # Add the extra header spanning multiple columns
    addtorow <- list()
    addtorow$pos <- list()
    addtorow$pos[[1]] <- -1
    addtorow$pos[[2]] <- nrow(forecast_tab)

    quant_string <- ""
    quant_ampersands <- ""
    quant_cell_defs <- NULL
    for(i in 1:length(quant_levels)){
      quant_string <- paste0(quant_string,
                             latex_amp(),
                             quant_levels[i])
      quant_ampersands <- paste0(quant_ampersands,
                                 latex_amp())
      quant_cell_defs <- c(quant_cell_defs, "C{1.5cm} ")
    }
    # Add the vertical bar to the edge of the last quant cell
    quant_cell_defs[length(quant_cell_defs)] <-
      paste0(quant_cell_defs[length(quant_cell_defs)], "|")

    addtorow$command <- c(
      paste0(latex_cline("1-7"),
             latex_mcol(3,
                        "|c|",
                        ""),
             latex_amp(),
             latex_bold("Biomass at"),
             latex_amp(),
             latex_mcol(3,
                        "c|",
                        table_header),
             latex_nline,
             latex_cline("1-3"),
             latex_mcol(3,
                        "|c|",
                        latex_bold("Catch Alternative")),
             latex_amp(),
             latex_bold("start of year"),
             latex_amp(),
             paste(latex_bold(quant_levels), collapse = latex_amp()),
             latex_nline,
             latex_hline,
             latex_amp(),
             latex_bold("Catch year"),
             latex_amp(),
             latex_bold("Catch (t)"),
             latex_amp(),
             paste(first_biomass_yr[1, ], collapse = latex_amp()),
             latex_nline,
             latex_hline),
      latex_hline)

  }else if(type == "spr"){
    forecast_tab <- map(forecast, ~{
      tmp <- .x$spr
      tmp <- tmp |>
        as_tibble() |>
        select(-c("25%", "75%"))
      names(tmp) <- gsub("%", "\\\\%", names(tmp))
      slice(tmp, -nrow(tmp))
    }) |>
      map_df(~{.x})

    forecast_tab <- forecast_tab |>
      bind_cols(c_levels, tab_letters) |>
      mutate(yr = as.numeric(yr),
             start_yr = paste("Start of", as.character(yr + 1))) |>
      select(labels, yr, `Catch (t)`, start_yr, everything()) |>
      mutate(yr = as.character(yr),
             `Catch (t)` = f(`Catch (t)`),
             `5\\%` = f(`5\\%`, 2),
             `50\\%` = f(`50\\%`, 2),
             `95\\%` = f(`95\\%`, 2))

    quant_levels <- grep("%", names(forecast_tab), value = TRUE)

    # Add the extra header spanning multiple columns
    addtorow <- list()
    addtorow$pos <- list()
    addtorow$pos[[1]] <- -1
    addtorow$pos[[2]] <- nrow(forecast_tab)

    quant_string <- ""
    quant_ampersands <- ""
    quant_cell_defs <- NULL
    for(i in 1:length(quant_levels)){
      quant_string <- paste0(quant_string,
                             latex_amp(),
                             quant_levels[i])
      quant_ampersands <- paste0(quant_ampersands,
                                 latex_amp())
      quant_cell_defs <- c(quant_cell_defs, "C{1.5cm} ")
    }
    # Add the vertical bar to the edge of the last quant cell
    quant_cell_defs[length(quant_cell_defs)] <-
      paste0(quant_cell_defs[length(quant_cell_defs)], "|")

    addtorow$command <- c(paste0(latex_cline("1-6"),
                                 latex_mcol(3,
                                            "|c|",
                                            latex_bold("Catch Alternative")),
                                 latex_amp(),
                                 latex_mcol(3,
                                            "c|",
                                            table_header),
                                 latex_nline,
                                 latex_cline("1-3"),
                                 latex_amp(),
                                 latex_bold("Catch year"),
                                 latex_amp(),
                                 latex_bold("Catch (t)"),
                                 latex_amp(),
                                 paste(latex_bold(quant_levels),
                                       collapse = latex_amp()),
                                 latex_nline,
                                 latex_hline),
                          latex_hline)
  }

  if(type == "biomass"){
    align <- c("c",
               "|c",
               "c",
               "c|",
               "c|",
               quant_cell_defs)
  }else if(type == "spr"){
    align <- c("c",
               "|c",
               "c",
               "c|",
               quant_cell_defs)
    forecast_tab <- forecast_tab |>
      select(-start_yr)
  }

  if(is.null(rows_to_show[1])){
    # Add the right number of horizontal lines to make the table break in the
    # correct places. A line is not needed at the bottom explains
    # (length(forecast)-1) in the loop
    if(length(forecast_inds) > 1){
      for(i in 1:(length(forecast_inds) - 1)){
        addtorow$pos[[i + 2]] <- i * num_rows
        addtorow$command <- c(addtorow$command, latex_hline)
      }
    }
  }else{
    root_rows <- map_dbl(rows_to_show, ~{
      ind <- grep(paste0("^", .x, ":"), forecast_tab$labels)
    })
    if(!length(root_rows)){
      stop("None of the rows you selected exist in the decision table")
    }
    remove_inds <- (length(forecast_yrs) - 1):length(forecast_yrs)
    rows <- map(seq_along(forecast_yrs)[-remove_inds], ~{
      root_rows + .x
    }) |>
      unlist()
    rows <- c(rows, root_rows) |>
      sort()
    forecast_tab <- forecast_tab[rows, ]

    addtorow$pos <- list()
    addtorow$pos[[1]] <- -1
    addtorow$pos[[2]] <- nrow(forecast_tab)
    if(length(root_rows) > 1){
      for(i in 1:(length(root_rows) - 1)){
        addtorow$pos[[i + 2]] <- i * (length(forecast_yrs) - 1)
        addtorow$command <- c(addtorow$command, latex_hline)
      }
    }
  }
  # Make the size string for font and space size
  size_string <- latex_size_str(font_size, space_size)
  print(xtable(forecast_tab,
               align = align,
               ...),
        caption.placement = "top",
        include.rownames = FALSE,
        include.colnames = FALSE,
        sanitize.text.function = function(x){x},
        size = size_string,
        add.to.row = addtorow,
        table.placement = placement,
        tabular.environment = "tabular",
        hline.after = NULL)
}
