#' Create the Executive Summary decision tables. `rows_to_label` and
#' `row_labels` need changing each year.
#'
#' @param model A model, created by [create_rds_file()]
#' @param type One of `biomass` or `spr`
#' @param forecast_inds The indices to use in the `forecasts` object which
#' itself is an object of the `model` list
#' @param rows_to_show A vector of letters of rows to show. If `NULL`, show
#' @param inc_fi_stable_catch Logical. If `TRUE`, include the two rows of
#' forecasts for fishing intensity equal to 100% and stable catch for the first
#' two years
#' @param left_col_cm Number of centimeters wide to make the leftmost column.
#' This needs to be changed when the font size is changed
#' @param right_cols_cm Number of centimeters wide to make the 3 rightmost
#' columns wide. This needs to be changed when the font size is changed
#' @param bold_letters Logical. If `TRUE`, the letters labelling each row
#' will be boldface
#' @param digits The number of decimal points to show in the table
#' @param font_size The table data and header font size in points
#' @param header_font_size The font size for the headers only. If `NULL`,
#' the headers will have the same font size as the table cell data
#' @param vert_spacing The vertical spacing between newlines for this font.
#' If `NULL` this will be calculated as `header_font_size * header_vert_scale`
#' @param header_vert_scale Scale factor to create the vertical spacing value.
#' See `header_vert_spacing`
#' @param ret_df Logical. If `TRUE`, return a data frame (tibble()) instead
#' of the [kableExtra::kbl()].
#' @param ... Arguments passed to [knitr::kable()]
#'
#' @return An [knitr::kable()] object
#' @export
table_decision <- \(
  model,
  type = c("biomass", "spr"),
  forecast_inds = seq_along(model$forecasts[[length(model$forecasts)]]),
  rows_to_show = NULL,
  inc_fi_stable_catch = FALSE,
  bold_letters = TRUE,
  digits = 2,
  left_col_cm = 1,
  right_cols_cm = 1,
  font_size = 10,
  header_font_size = 10,
  header_vert_spacing = 12,
  header_vert_scale = 1.2,
  ret_df = FALSE,
  ...){

  type <- match.arg(type)

  # `letter_df` A data frame with three columns called `let`, `row1_text`,
  # and `row2_text` where `let` contains the letter shown in the table for
  # each row, `row1_text` is the first line of text directly beneath the
  # letter, and `row2_text` is the second line of text directly below the
  # first line of text. `row1_text` and `row2_text` can be empty strings, in
  # that case no text will be shown in the cell, only the letter
  if(inc_fi_stable_catch){
    letter_df = tribble(
      ~let,  ~row1_text,                        ~row2_text,
      "a",  "",                                "",
      "b",  "",                                "",
      "c",  "",                                "",
      "d",  "",                                "",
      "e",  "",                                "",
      "f",  "",                                "",
      "g",  "",                                "",
      "h",  "",                                "",
      "i",  "",                                "",
      "j",  paste0(model$endyr, " TAC"),       "",
      "k",  "Fishing intensity",               "at 100\\%",
      "l",  "Default HR",                      paste0("(",
                                                      fspr_40_10_for_latex_table,
                                                      ")"),
      "m",  "Equal catch",                     paste0("($\\mathrm{C}_{",
                                                      model$endyr + 1,
                                                      "} \\approxeq \\mathrm{C}",
                                                      "_{", model$endyr + 2,
                                                      "}$)"))
  }else{
    letter_df = tribble(
      ~let,  ~row1_text,                        ~row2_text,
      "a",  "",                                "",
      "b",  "",                                "",
      "c",  "",                                "",
      "d",  "",                                "",
      "e",  "",                                "",
      "f",  "",                                "",
      "g",  "",                                "",
      "h",  "",                                "",
      "i",  "",                                "",
      "j",  paste0(model$endyr, " TAC"),       "",
      "k",  "Default HR",                      paste0("(",
                                                      fspr_40_10_for_latex_table,
                                                      ")"))
  }
  if(is.null(nrow(letter_df)) || nrow(letter_df) == 0){
    stop("`letter_df` is not a data frame with at least one row")
  }
  if(ncol(letter_df) != 3){
    stop("`letter_df` must have three columns")
  }

  if(!is.null(rows_to_show[1])){
    forecast_inds <- match(rows_to_show, letter_df$let)
    letter_df <- letter_df |>
      dplyr::filter(let %in% rows_to_show)
  }

  if(bold_letters){
    letter_df <- letter_df |>
      mutate(let = latex_bold(paste0(let, ":")))
  }else{
    letter_df <- letter_df |>
      mutate(let = paste0(let, ":"))
  }
  num_letters <- nrow(letter_df)
  # Extract `letter_df` into a simple vector and enframe it so that it is
  # a single column data frame
  letter_col <- letter_df |>
    pmap(~{c(..1, ..2, ..3)}) |>
    unlist() |>
    enframe(name = NULL)

  fore_lst <- model$forecasts
  if(is.null(fore_lst[1]) || is.na(fore_lst[1])){
    stop("No forecasts found for input `model`. `model$forecasts` is `NULL` ",
         "or `NA`")
  }
  if(any(forecast_inds > length(fore_lst[[length(fore_lst)]]))){
    stop("`forecast_inds` contains values greater than the length of the ",
         "forecast catch levels list (`model$forecasts`)")
  }

  if(length(forecast_inds) != nrow(letter_df)){
    stop("The data frame describing the rows of the table (`letter_df`) ",
         "does not have the same number of rows as the number of forecast ",
         "streams you have provided (`forecast_inds`)")
  }

  forecast <- fore_lst[[length(fore_lst)]][forecast_inds]
  if(type == "biomass"){
    num_rows <- nrow(forecast[[1]]$depl) - 1
    table_header <- latex_bold("Resulting relative spawning biomass")
  }else{
    num_rows <- nrow(forecast[[1]]$spr) - 1
    table_header <- latex_bold("Relative fishing intensity")
  }

  # Extract the catch levels for the three years shown in the table (3)
  ct_levels <- map(model$forecasts[[3]][forecast_inds], ~{
    .x$fore_catch$catch
  }) |>
    unlist() |>
    enframe(name = NULL, value = "Catch (t)")

  # Start building the table data frame by connecting the letters column and
  # the catch forecast column
  d <- bind_cols(letter_col, ct_levels) |>
    mutate(`Catch (t)` = f(`Catch (t)`))

  # Extract the quantiles for the table values (either biomass or fishing
  # intensity). For biomass, also add a column for the biomass at the start
  # of the year
  # Also, extract the first forecast year and the quantiles of the values to
  # go along with it for later (`first_biomass_yr`). A `<<-` is used here to
  # make the variable available outside the `map()` function

  forecast_tab <- map(forecast, ~{
    if(type == "biomass"){
      tmp <- .x$depl |>
        t() |>
        as_tibble(rownames = "yr") |>
        mutate(yr = paste0("Start of ", yr)) |>
        select(-c("25%", "75%"))
      names(tmp)[1] <- "start of year"
      names(tmp) <- gsub("%", "\\\\%", names(tmp))
      first_biomass_yr <<- slice(tmp, 1)
      slice(tmp, -1)
    }else if(type == "spr"){
      tmp <- .x$spr|>
        dplyr::filter(yr %in% forecast_yrs[-length(forecast_yrs)]) |>
        select(-c("25%", "75%"))
      names(tmp) <- gsub("%", "\\\\%", names(tmp))
      tmp
    }
  }) |>
    map_df(~{.x}) |>
    mutate_at(vars(-1), ~{f(.x, digits)})

  # Continue building the table data frame by connecting the value columns to
  # the previously-constructed data frame (with letters and catch forecast
  # columns)
  d <- bind_cols(d, forecast_tab)

  # Extract a vector of the forecast years,
  forecast_yrs <- forecast[[1]]$fore_catch$year
  if(length(forecast_yrs) != 4){
    stop("The number of forecast years in the model is not 4. The decision ",
         "function is currently designed for 4 years only")
  }
  # Create a data frame column containing the forecast years (minus 1)
  # to prepare for addition to the table data frame
  catch_yrs <- rep(forecast_yrs[-length(forecast_yrs)], num_letters) |>
    enframe(name = NULL,
            value = "Catch year")

  # Continue building the table data frame by connecting the catch year
  # columns to the previously-constructed data frame (with letters, catch
  # forecast, and quantiles of the values columns)
  # Also, change all columns to character data type
  d <- bind_cols(d, catch_yrs) |>
    mutate_all(~{as.character(.x)}) |>
    select(value, `Catch year`, everything())

  # First row, with the initial year only, with rounded and formatted values
  # and with the column headers added for catch year and weight
  # `mutsel` is in this package and just combines
  # [dplyr::mutate()] and [dplyr::select()]
  if(type == "biomass"){
    first_biomass_yr <- first_biomass_yr |>
      mutate_at(vars(-1), ~{f(.x, 2)}) |>
      mutate_all(~{as.character(.x)}) |>
      mutsel(value = "",
             `Catch year` = "",
             `Catch (t)` = "")
    # `Catch year` = latex_bold("Catch year"),
    # `Catch (t)` = latex_bold("Catch (t)"))
  }
  # If the relative biomass table:
  # Continue building the table data frame by adding the first row
  # to the previously-constructed data frame (with letters, catch
  # forecast quantiles of the values columns, and catch year)
  if(type == "biomass"){
    d <- bind_rows(first_biomass_yr, d)
  }else{
    d <- d |>
      select(-yr)
  }

  if(ret_df){
    return(d)
  }

  # Remove the first three column headers, because there will be a
  # three-column spanning header above them for `Catch Alternative`
  col_names <- names(d)

  # Insert header fontsize if it wasn't supplied
  if(is.null(header_font_size)){
    header_font_size <- font_size
  }
  hdr_font_str <- create_fontsize_str(header_font_size,
                                      header_vert_spacing,
                                      header_vert_scale)

  # Insert font specs around all column headers
  col_names <- gsub("\\n", paste0("\n", hdr_font_str$dbl), col_names)
  col_names <- paste0(hdr_font_str$dbl, col_names)
  # Add \\makecell{} latex command to headers with newlines (\n)
  col_names <- linebreaker(col_names, align = "c")
  # Remove the first column name, since the letters column doesn't need
  # a description or name
  col_names[1] <- ""

  # Create extra header vector with fontsize changes to match the header font
  ca <- latex_bold(linebreaker(paste0(hdr_font_str$dbl,
                                      "Catch alternative"),
                               align = "c"))
  ba <- latex_bold(linebreaker(paste0(hdr_font_str$dbl,
                                      "Biomass at"),
                               align = "c"))
  rsb <- latex_bold(linebreaker(paste0(hdr_font_str$dbl,
                                       ifelse(type == "biomass",
                                              "Relative spawning biomass",
                                              "Relative fishing intensity")),
                                align = "c"))

  if(type == "biomass"){
    extra_header <- c(set_names(3, ca),
                      set_names(1, ba),
                      set_names(3, rsb))
  }else if(type == "spr"){
    extra_header <- c(set_names(3, ca),
                      set_names(3, rsb))
  }
  # Need to change the backslashes to quadruple-backslashes here
  #names(extra_header) <- gsub("\\\\", "\\\\\\\\", names(extra_header))

  # Create the decision table, note the column widths are changed here for
  # the first (leftmost) column and the last 3 (rightmost) columns
  k <- kbl(d,
           format = "latex",
           booktabs = TRUE,
           align = c(paste0("C{",
                            left_col_cm,
                            "cm} "),
                     rep("r", ifelse(type == "biomass",
                                     3,
                                     2)),
                     rep(paste0(" C{",
                                right_cols_cm,
                                "cm}"), 3)),
           linesep = "",
           col.names = col_names,
           escape = FALSE,
           ...) |>
    row_spec(0, bold = TRUE) |>
    # Add the extra upper header with `Catch alternative` and
    # `Relative spawning biomass` spanning three columns each, and `Biomass at`
    # spanning one column
    add_header_above(header = extra_header,
                     escape = FALSE,
                     line = FALSE) |>
    kable_styling(font_size = font_size,
                  latex_options = c("repeat_header"))

  # Horizontal line after the first row if table type is "biomass"
  if(type == "biomass"){
    k <- k |>
      row_spec(1, hline_after = TRUE)
  }
  # Horizontal line after every letter group of forecast years
  for(i in 1:(num_letters - 1)){
    # The 1 + here is to account for the first row being an extra
    # add-on to the rest of the lettered groups for the relative biomass
    # table. For the relative fishing intensity table, this does not occur and
    # every nth row has a line under it without the special case
    spec <- ifelse(type == "biomass", 1, 0)
    k <- k |>
      row_spec(spec + i * length(forecast_yrs[-length(forecast_yrs)]),
               hline_after = TRUE)
  }

  k
}
