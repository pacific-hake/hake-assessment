#' Create a table of US vs Canadian catch, TAC, and attainment for the
#' JTC December meeting presentation
#'
#' @param yr The year to use for the table
#' @param attain_digits The number of decimal places in the attainment
#' percentages
#' @param font_size The table data and header font size in points
#' @param header_font_size The font size for the headers only. If `NULL`,
#' the headers will have the same font size as the table cell data
#' @param header_vert_spacing The vertical spacing between newlines for
#' this font. If `NULL` this will be calculated as
#' `header_font_size * header_vert_scale`
#' @param header_vert_scale Scale factor to create the vertical spacing value.
#' See `header_vert_spacing`
#'
#' @param ... Arguments passed to [knitr::kable()]
#'
#' @return A [knitr::kable()] object
#' @export
table_catch_tac_summary <- function(yr = NULL,
                                    attain_digits = 1,
                                    font_size = 8,
                                    header_font_size = 10,
                                    header_vert_spacing = 12,
                                    header_vert_scale = 1.2,
                                    ...){

  if(is.null(yr)){
    stop("You must supply a year (`yr`) to get values for")
  }

  out <- list()
  ct_df <- ct |>
    dplyr::filter(Year == yr)

  if(!nrow(ct_df)){
    stop("Year `", yr, "` not found in the data frame `ct`, which is ",
         "package data in the hake package. See the definition of `ct` in ",
         "`data-raw/pd-data-tables.R`")
  }
  if(nrow(ct_df) > 1){
    stop("Year `", yr, "` found more than once in the data frame `ct`, ",
         "which is package data in the hake package. See the definition of ",
         "`ct` in `data-raw/pd-data-tables.R`")
  }

  # Reform the year of values in to a data frame with the US values on
  # the left and the Canadian on the right

  # The proportions below (eg us_ss_prop_tac) are package data
  us_ss_tac <- pull(ct_df, `U.S. TAC`) * us_ss_prop_tac
  us_ms_tac <- pull(ct_df, `U.S. TAC`) * us_ms_prop_tac
  us_cp_tac <- pull(ct_df, `U.S. TAC`) * us_cp_prop_tac

  can_attain <- paste0(f(pull(ct_df, can_attain), attain_digits), "\\%")
  us_attain <- paste0(f(pull(ct_df, us_attain), attain_digits), "\\%")
  us_ss_attain <- paste0(f(pull(ct_df, `U.S. Shoreside`) / us_ss_tac * 100,
                           attain_digits), "\\%")
  us_ms_attain <- paste0(f(pull(ct_df, `U.S. Mothership`) / us_ms_tac * 100,
                           attain_digits), "\\%")
  us_cp_attain <- paste0(f(pull(ct_df, `U.S. Catcher-processor`) /
                             us_cp_tac * 100,
                           attain_digits), "\\%")

  us_ss_perc_catch <- paste0(f(pull(ct_df, `U.S. Shoreside`) /
                           pull(ct_df, `U.S. Total`) * 100,
                         attain_digits), "\\%")
  us_ms_perc_catch <- paste0(f(pull(ct_df, `U.S. Mothership`) /
                           pull(ct_df, `U.S. Total`) * 100,
                         attain_digits), "\\%")
  us_cp_perc_catch <- paste0(f(pull(ct_df, `U.S. Catcher-processor`) /
                           pull(ct_df, `U.S. Total`) * 100,
                         attain_digits), "\\%")
  can_ss_perc_catch <- paste0(f(pull(ct_df, `Canada Shoreside`) /
                           pull(ct_df, `Canada Total`) * 100,
                         attain_digits), "\\%")
  can_ft_perc_catch <-paste0(f(pull(ct_df, `Canada Freezer-trawler`) /
                           pull(ct_df, `Canada Total`) * 100,
                         attain_digits), "\\%")

  lst <- list(
    c("Total", "", "Total", ""),
    c("Catch (t)", f(pull(ct_df, `U.S. Total`)),
      "Catch (t)", f(pull(ct_df, `Canada Total`))),
    c("TAC", f(pull(ct_df, `U.S. TAC`)),
      "TAC", f(pull(ct_df, `Canada TAC`))),
    c("Attainment", us_attain,
      "Attainment", can_attain),
    c("Shoreside", "", "Shoreside", ""),
    c("Catch (t)", f(pull(ct_df, `U.S. Shoreside`)),
      "Catch (t)", f(pull(ct_df, `Canada Shoreside`))),
    c("TAC", f(us_ss_tac),
      "--", "--"),
    c("Attainment", us_ss_attain,
      "--", "--"),
    c("\\% of U.S. Catch", us_ss_perc_catch,
      "\\% of Canadian Catch", can_ss_perc_catch),
    c("Catcher-Processor", "", "Freezer-Trawler", ""),
    c("Catch (t)", f(pull(ct_df, `U.S. Catcher-processor`)),
      "Catch (t)", f(pull(ct_df, `Canada Freezer-trawler`))),
    c("TAC", f(us_cp_tac),
      "--", "--"),
    c("Attainment", us_cp_attain,
      "--", "--"),
    c("\\% of U.S. Catch", us_cp_perc_catch,
      "\\% of Canadian Catch", can_ft_perc_catch),
    c("Mothership", "", "", ""),
    c("Catch (t)", f(pull(ct_df, `U.S. Mothership`)),
      "--", "--"),
    c("TAC", f(us_ms_tac),
      "--", "--"),
    c("Attainment", us_ms_attain,
      "--", "--"),
    c("\\% of U.S. Catch", us_ms_perc_catch,
      "--", "--"))

  df <- map_dfr(lst, ~{
    names(.x) <- seq_along(.x)
    .x
    })
  #names(df) <- c("U.S.", "U.S. Values", "Canada", "Canada Values")

  # Insert custom header fontsize before linebreaker
  if(is.null(header_font_size)){
    header_font_size <- font_size
  }
  hdr_font_str <- create_fontsize_str(header_font_size,
                                      header_vert_spacing,
                                      header_vert_scale)

  col_names <- names(df)
  col_names <- gsub("\\n", paste0("\n", hdr_font_str$quad), col_names)
  col_names <- paste0(hdr_font_str$dbl, col_names)
  # Add \\makecell{} latex command to headers with newlines
  col_names <- linebreak(col_names, align = "c")

  kbl(df,
      format = "latex",
      booktabs = TRUE,
      align = "r",
      linesep = "",
      col.names = NULL,
      escape = FALSE,
      ...) |>
    row_spec(c(0, 1, 5, 10, 15), bold = TRUE) |>
    row_spec(c(1, 5, 10, 15), italic = TRUE) |>
    row_spec(c(4, 9, 14), hline_after = TRUE) |>
    kable_styling(font_size = font_size) |>
    add_header_above(c("U.S." = 2, "Canada" = 2), bold = TRUE)
}
