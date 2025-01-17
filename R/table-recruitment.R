#' Create a table of absolute recruitment and relative spawning biomass
#' medians and quantiles
#'
#' @param model A model, created by [create_rds_file()]
#' @param start_yr Start year in table
#' @param end_yr End year in table
#' @param digits Number of decimal places to round recruitment values to
#' @param digits_dev Number of decimal places to round recruitment deviation
#' values to
#' @param scale A scale factor to divide the recruitment by
#' @param font_size The table data and header font size in points
#' @param header_font_size The font size for the headers only. If `NULL`,
#' the headers will have the same font size as the table cell data
#' @param header_vert_spacing The vertical spacing between newlines for
#' this font. If `NULL` this will be calculated as
#' `header_font_size * header_vert_scale`
#' @param header_vert_scale Scale factor to create the vertical spacing value.
#' See `header_vert_spacing`
#' @param ... Arguments passed to [knitr::kable()]
#'
#' @return An [knitr::kable()] object
#' @export
table_recruitment <- function(model,
                              start_yr,
                              end_yr,
                              digits = 1,
                              digits_dev = 3,
                              scale = 1000,
                              font_size = 8,
                              header_font_size = 10,
                              header_vert_spacing = 12,
                              header_vert_scale = 1.2,
                              ...){

  calcs <- model$mcmccalcs
  yrs <- start_yr:end_yr

  # Recruitment quantiles
  inds <- grep("^20[0-9]{2}$", names(calcs$rlower))
  rlower <- calcs$rlower[inds] * scale
  rlower <- rlower[names(rlower) %in% yrs]
  rmed <- calcs$rmed[inds] * scale
  rmed <- rmed[names(rmed) %in% yrs]
  rupper <- calcs$rupper[inds] * scale
  rupper<- rupper[names(rupper) %in% yrs]

  # Recruitment deviation quantiles
  inds <- grep("^20[0-9]{2}$", names(calcs$devlower))
  devlower <- calcs$devlower[inds]
  devlower <- devlower[names(devlower) %in% yrs]
  devmed <- calcs$devmed[inds]
  devmed <- devmed[names(devmed) %in% yrs]
  devupper <- calcs$devupper[inds]
  devupper<- devupper[names(devupper) %in% yrs]

  # Check to see if devlower, devmed, and devupper have values. Fill in
  # missing years if they do not. This will occur if late devs are turnee
  # off in the SS3 control file by using a negative number. In that case, the
  # last two years will not exist in the devs
  devs <- map(list(devlower, devmed, devupper), \(dev){

    dev_nms <- as.numeric(names(dev))
    missing_yrs <- setdiff(yrs, dev_nms)
    if(length(missing_yrs)){
      missing_vec <- rep(NA, length(missing_yrs))
      names(missing_vec) <- missing_yrs
      return(c(dev, missing_vec))
    }
    dev
  })
  devlower <- devs[[1]]
  devmed <- devs[[2]]
  devupper <- devs[[3]]

  df <- tibble(yrs,
               f(rlower, digits),
               f(rmed, digits),
               f(rupper, digits),
               f(devlower, digits_dev),
               f(devmed, digits_dev),
               f(devupper, digits_dev)) |>
    dplyr::filter(yrs %in% start_yr:end_yr)

  # Replace NAs with double dashes in all cells in the table
  df <- df |>
    mutate(across(everything(), ~{gsub("^ *NA$", "--", .x)}))

  names(df) <- c("Year",
                 "Recruit-\nment\n2.5\\textsuperscript{th}\npercentile",
                 "Recruit-\nment\nMedian",
                 "Recruit-\nment\n97.5\\textsuperscript{th}\npercentile",
                 "Rec.\nDeviations\n2.5\\textsuperscript{th}\npercentile",
                 "Rec.\nDeviations\nMedian",
                 "Rec.\nDeviations\n97.5\\textsuperscript{th}\npercentile")

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
      col.names = col_names,
      escape = FALSE,
      ...) |>
    row_spec(0, bold = TRUE) |>
    kable_styling(font_size = font_size,
                  latex_options = c("repeat_header"))
}
