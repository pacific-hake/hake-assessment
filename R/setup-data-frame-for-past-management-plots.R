#' Create a data frame of ggplot-format containing a time series of the
#' management-chosen TAC, assessment estimated TAC, and realized catch, and if
#' requested the estimated spawning and total biomasses
#'
#' @details
#' if `diffs` is `TRUE` then proportions from year to year based on 1
#' being set to the maximum value in each time series will be returned instead
#' of actual values
#'
#' @param d The data as read in using [readr::read_csv()] from the file
#' "catch-targets.csv"
#' @param yrs A vector of years to show on the plot. If `NA`, the `Year`
#' column in `d` will be used
#' @param inc_biomass_ests If `TRUE`, include a line representing the estimated
#' biomass (medians)
#' @param inc_survey If `TRUE`, include the survey biomass estimates as
#' provided by the survey team (not fits from the model)
#' @param survey_index_df A data frame with the columns `year` and `total`
#' containing the estimated biomass values for the survey years in the `total`
#' column`
#' @param diffs If `TRUE`, show proportion change from year to year, normalized
#' by the maximum value in `yrs`
#' @param ret_tbl Logical. If `TRUE`, return a [kableExtra::kbl()] containing
#' the outputs instead of the data frame
#' @param font_size The table data and header font size in points. Only used
#' if `ret_tbl` is `TRUE`
#' @param right_cols_cm The number of centimeters wide to make all of the
#' rightmost columns (all the value columns). Only used if `ret_tbl` is `TRUE`
#' @param ... Absorbs arguments not expected to be passed to this function
#'
#' @return A [data.frame]
#' @export
setup_data_frame_for_past_management_plots <- \(model,
                                                d,
                                                inc_biomass_ests = FALSE,
                                                inc_survey = FALSE,
                                                survey_index_df = NA,
                                                diffs = FALSE,
                                                yrs = NA,
                                                ret_tbl = FALSE,
                                                font_size = 10,
                                                right_cols_cm = 1.8,
                                                ...){

  # Remove any old default HCR tac values that don't exist (2002 and 2003
  # for example for hake)
  d <- d |>
    dplyr::filter(!is.na(`Default HCR TAC`))

  # Add the current assessment year's Default HCR TAC to the data frame
  assess_yr <- max(d$Year) + 1
  col_nm <- paste0("ForeCatch_", assess_yr)
  med_dhcr_final <- as.numeric(quantile(model$mcmc[[col_nm]], probs = 0.5))
  new_row <- c(assess_yr, NA, NA, med_dhcr_final)
  names(new_row) <- names(d)
  d <- d |>
    bind_rows(new_row)

  d <- d |>
    rename(`TAC implemented by management` = `Total TAC`)

  if(!is.na(yrs[1]) &&!all(yrs %in% d$Year)){
    stop("Not all `yrs` are in the data. The data contains the following years:\n",
         paste(sort(unique(d$Year)), collapse = " "), "\n")
  }

  if(is.na(yrs[1])){
    yrs <- d$Year |> unique() |> sort()
  }else{
    d <- d |>
      dplyr::filter(Year %in% yrs)
  }

  group_ord <- c("Default HCR TAC", "TAC implemented by management", "Realized catch")

  # Reformat data frame to be in ggplot format (long)
  d <- d |>
    pivot_longer(-Year) |>
    mutate(name = factor(name, levels = group_ord)) |>
    mutate(value = value / 1e3)

  if(inc_biomass_ests){

    # Add estimated age 2+ spawning biomass
    bio <- base_model$mcmccalcs$smed |>
      enframe() |>
      rename(Year = name) |>
      mutate(Year = as.numeric(Year)) |>
      dplyr::filter(Year %in% yrs) |>
      mutate(name = "Age 2+ Spawning Biomass") |>
      select(Year, name, value) |>
      mutate(value = value * 1e3)
    d <- d |>
      bind_rows(bio) |>
      mutate(name = factor(name))

    # Add estimated age 2+ total biomass
    bio <- base_model$extra_mcmc$total_age2_plus_biomass_quants |>
      select(yr, `50%`) |>
      rename(Year = yr, value = `50%`) |>
      mutate(value = value / 1e3) |>
      dplyr::filter(Year %in% yrs) %>%
      bind_cols(tibble(name = "Age 2+ Total Biomass", .rows = nrow(.))) |>
      select(Year, name, value)

    d <- d |>
      bind_rows(bio) |>
      mutate(name = factor(name))
  }

  # Remove TAC implemented by management and realized catch for current year
  # as it hasn't happened yet (they are NAs)
  d <- d |> dplyr::filter(!is.na(value))

  if(inc_survey){

    if(is.null(nrow(survey_index_df)) || nrow(survey_index_df) < 0){
      stop("You provided `inc_survey` but did not supply a correct data frame ",
           "for `survey_index_df`")
    }
    if(!"year" %in% names(survey_index_df)){
      stop("The `survey_index_df` data frame must contain a column named `year`")
    }
    if(!"total" %in% names(survey_index_df)){
      stop("The `survey_index_df` data frame must contain a column named `total`")
    }
    j <- survey_index_df |>
      rename(Year = year, value = total) |>
      mutate(name = "Survey index") |>
      dplyr::filter(Year %in% yrs) |>
      select(Year, name, value)

    d <- d |>
      bind_rows(j)
  }

  if(diffs){

    # Calculate proportion changed from year to year, for each data group
    d <- d |>
      # split the data frame into a list of data frames, by the values found
      # in the `name` column
      split(~ name) |>
      # normalize each data frame in the list
      map(~{.x |> mutate(value = value / max(value, na.rm = TRUE))}) |>
      # shift normalized values by one and subtract from original values
      # (one year lag)
      map(~{
        orig_vec <- .x$value
        # add NA at the beginning to make the vector the same length
        shift_vec <- c(NA, .x$value[-length(.x$value)])
        diff_vec <- orig_vec - shift_vec
        # replace NA placed at the beginning with zero
        diff_vec[is.na(diff_vec)] <- 0
        .x$value <- diff_vec
        .x
      }) |>
      # bind all the data frames in the list back into a single data frame
      bind_rows()
  }

  if(ret_tbl){

    d <- d |>
      pivot_wider(names_from = "name", values_from = "value") |>
      mutate(across(-Year, ~{f(.x)}))
    k <- kbl(d,
             format = "latex",
             booktabs = TRUE,
             align = c("l",
                       rep(paste0("R{",
                                  right_cols_cm,
                                  "cm}"),
                           ncol(d) - 1)),
             linesep = "",
             escape = FALSE,
             ...) |>
      row_spec(0, bold = TRUE) |>
      kable_styling(font_size = font_size,
                    latex_options = c("repeat_header"))

    return(k)
  }

  d
}