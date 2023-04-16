#' Create the Executive Summary decision tables. `rows_to_label` and
#' `row_labels` need changing each year.
#'
#' @param model A model, created by [create_rds_file()]
#' @param type One of `biomass` or `spr`
#' @param forecast_inds The indices to use in the `forecasts` object which
#' itself is an object of the `model` list
#' @param letter_df A data frame with three columns called `let`, `row1_text`,
#' and `row2_text` where `let` contains the letter shown in the table for
#' each row, `row1_text` is the first line of text directly beneath the
#' letter, and `row2_text` is the second line of text directly below the
#' first line of text. `row1_text` and `row2_text` can be empty strings, in
#' that case no text will be shown in the cell, only the letter
#' @param rows_to_show A vector of letters of rows to show. If `NULL`, show
#' all rows
#' @param left_col_cm Number of centimeters wide to make the leftmost column.
#' This needs to be changed when the font size is changed
#' @param right_cols_cm Number of centimeters wide to make the 3 rightmost
#' columns wide. This needs to be changed when the font size is changed
#' @param digits The number of decimal points to show in the table
#' @param font_size The table data and header font size in points
#' @param header_font_size The font size for the headers only. If `NULL`,
#' the headers will have the same font size as the table cell data
#' @param vert_spacing The vertical spacing between newlines for this font.
#' If `NULL` this will be calculated as `header_font_size * header_vert_scale`
#' @param header_vert_scale Scale factor to create the vertical spacing value.
#' See `header_vert_spacing`
#' @param ... Arguments passed to [knitr::kable()]
#'
#' @return An [knitr::kable()] object
#' @export
table_decision <- \(
  model,
  type = c("biomass", "spr"),
  forecast_inds = seq_along(model$forecasts[[length(model$forecasts)]]),
  rows_to_show = NULL,
  letter_df = tribble(
    ~let, ~row1_text,                       ~row2_text,
    "a",  "",                                "",
    "b",  "",                                "",
    "c",  "",                                "",
    "d",  "",                                "",
    "e",  "10\\%",                           "reduction",
    "f",  as.character(model$endyr),         "catch",
    "g",  "",                                "",
    "h",  "10\\%",                           "reduction",
    "i",  "",                                "",
    "j",  "10\\%",                           "reduction",
    "k",  "",                                "",
    "l",  as.character(model$endyr),         "TAC",
    "m",  "FI=",                             "100\\%",
    "n",  "default",                         "HR",
    "o",  "Catch in",         paste0(model$endyr + 1, "=", model$endyr + 2)),
  #"o",  paste0("C", model$endyr + 1, "="), paste0("C", model$endyr + 2)),
digits = 2,
  left_col_cm = 1,
  right_cols_cm = 1,
  font_size = 10,
  header_font_size = 10,
  header_vert_spacing = 12,
  header_vert_scale = 1.2,
  ...){

  type <- match.arg(type)

  if(is.null(nrow(letter_df)) || nrow(letter_df) == 0){
    stop("`letter_df` is not a data frame with at least one row",
         call. = FALSE)
  }
  if(ncol(letter_df) != 3){
    stop("`letter_df` must have three columns",
         call. = FALSE)
  }
  num_letters <- nrow(letter_df)
  # Extract `letter_df` into a simple vector and enframe it so that it is
  # a single column data frame
  letter_col <- letter_df |>
    pmap(~{c(paste0(..1, ":"), ..2, ..3)
  }) |>
    unlist() |>
    enframe(name = NULL)


  fore_lst <- model$forecasts
  if(is.null(fore_lst[1]) || is.na(fore_lst[1])){
    stop("No forecasts found for input `model`. `model$forecasts` is `NULL` ",
         "or `NA`",
         call. = FALSE)
  }
  if(any(forecast_inds > length(fore_lst[[length(fore_lst)]]))){
    stop("`forecast_inds` contains values greater than the length of the ",
         "forecast catch levels list (`model$forecasts`)",
         call. = FALSE)
  }

  forecast <- fore_lst[[length(fore_lst)]][forecast_inds]
  if(type == "biomass"){
    num_rows <- nrow(forecast[[1]]$biomass) - 1
    table_header <- latex_bold("Resulting relative spawning biomass")
  }else{
    num_rows <- nrow(forecast[[1]]$spr) - 1
    table_header <- latex_bold("Relative fishing intensity")
  }

  ct_levels <- map(model$forecasts[[3]], ~{
    .x$fore_catch$catch
  }) |>
    unlist() |>
    enframe(name = NULL, value = "Catch (t)")
  d <- bind_cols(letter_col, ct_levels) |>
    mutate(`Catch (t)` = f(`Catch (t)`))

  forecast_tab <- map(forecast, ~{
    tmp <- .x$biomass |>
      mutate(yr = paste0("Start of ", yr)) |>
      select(-c("25%", "75%"))
    names(tmp)[1] <- "start of year"
    names(tmp) <- gsub("%", "\\\\%", names(tmp))
    first_biomass_yr <<- slice(tmp, 1)
    slice(tmp, -1)
  }) |>
    map_df(~{.x}) |>
    mutate_at(vars(-1), ~{f(.x, digits)})

  d <- bind_cols(d, forecast_tab)

  forecast_yrs <- forecast[[1]]$fore_catch$year
  catch_yrs <- rep(forecast_yrs[-length(forecast_yrs)], num_letters) |>
    enframe(name = NULL,
            value = "Catch year")

  d <- bind_cols(d, catch_yrs) |>
    mutate_all(~{as.character(.x)}) |>
    select(value, `Catch year`, everything())

  # First row, with the initial year only
  first_biomass_yr <- first_biomass_yr |>
    mutate_at(vars(-1), ~{f(.x, 2)}) |>
    mutate_all(~{as.character(.x)})

  #first_biomass_yr[1] <- paste("Start of", first_biomass_yr[1])
  first_biomass_yr <- first_biomass_yr |>
    mutate(value = "",
           `Catch year` = latex_bold("Catch year"),
           `Catch (t)` = latex_bold("Catch (t)")) |>
    select(value,
           `Catch year`,
           `Catch (t)`,
           `start of year`,
           everything())

  d <- bind_rows(first_biomass_yr, d)

  col_names <- names(d)
  col_names[1:3] <- ""

  # Insert custom header fontsize before linebreaker
  if(is.null(header_font_size)){
    header_font_size <- font_size
  }
  hdr_font_str <- create_fontsize_str(header_font_size,
                                      header_vert_spacing,
                                      header_vert_scale)

  col_names <- gsub("\\n", paste0("\n", hdr_font_str$quad), col_names)
  col_names <- paste0(hdr_font_str$dbl, col_names)
  # Add \\makecell{} latex command to headers with newlines
  col_names <- linebreaker(col_names, align = "c")
  col_names[1:3] <- ""

  k <- kbl(d,
           format = "latex",
           booktabs = TRUE,
           align = c(paste0("C{",
                            left_col_cm,
                            "cm} "),
                     rep("r", 3),
                     rep(paste0(" C{",
                                right_cols_cm,
                                "cm}"), 3)),
           linesep = "",
           col.names = col_names,
           escape = FALSE,
           ...) |>
    row_spec(0, bold = TRUE) |>
    add_header_above(c("Catch alternative" = 3,
                       "Biomass at",
                       "Relative spawning biomass" = 3),
                     line = FALSE,
                     bold = TRUE) |>
    kable_styling(font_size = font_size,
                  latex_options = c("repeat_header"))
    #column_spec(1, border_left = TRUE) |>
    #column_spec(ncol(d), border_right = TRUE)

  #column_spec(1:ncol(d), latex_column_spec = "|c|r|r|r|r|r|r|")

  # Horizontal line after the first row
  k <- k |>
    row_spec(1, hline_after = TRUE)
  # Horizontal line after every letter group of forecast years
  for(i in 1:(num_letters - 1)){
    # The 1 + here is to account for the first row being an extra
    # add-on to the rest of the lettered groups
    k <- k |>
      row_spec(1 + i * length(forecast_yrs[-length(forecast_yrs)]),
               hline_after = TRUE)
  }

  # Add another header line on top to contain "Catch alternative" and
  # "Relative spawning biomass"
  # k <- k |>
  #   add_header_above(c("Catch alternative" = 3,
  #                      "",
  #                      "Relative spawning biomass" = 3))

  k
}
