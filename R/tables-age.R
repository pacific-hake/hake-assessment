#' Makes a table of the estimated -at-age values for 5 different values:
#' Numbers-at-age, Catch-at-age, Biomass-at-age, Exploitation-at-age,
#' and Catch-at-age-biomass
#'
#' @param model A model in this project
#' @param start_yr Start year for the table
#' @param end_yr End year for the table
#' @param table_type 1 = Numbers-at-age, 2 = Exploitation-rate-at-age, 3 = Catch-at-age-number
#' 4 = Catch-at-age-biomass, 5 = Biomass-at-age
#' @param digits Number of decimal points
#' @param csv_dir Directory for CSV output
#' @param xcaption Table caption
#' @param xlabel The label used to reference the table in latex
#' @param font_size Size of the font for the table
#' @param space_size Size of the vertical spaces for the table
#'
#' @return An [xtable::xtable()]
#' @export
atage_table <- function(model,
                        start_yr = NA,
                        end_yr = NA,
                        table_type = 1,
                        digits = 0,
                        csv_dir = "out-csv",
                        xcaption = "default",
                        xlabel   = "default",
                        font_size = 9,
                        space_size = 10){

  csv_dir_full <- here::here("doc", csv_dir)
  if(!dir.exists(csv_dir_full)){
    dir.create(csv_dir_full)
  }

  tbl <- switch (table_type,
                 model$extra_mcmc$natage_median,
                 model$extra_mcmc$expatage_median,
                 model$extra_mcmc$catage_median,
                 model$extra_mcmc$catage_biomass_median,
                 model$extra_mcmc$batage_median)
  fn <- switch (table_type,
                file.path(csv_dir_full, out_est_naa_file),
                file.path(csv_dir_full, out_est_eaa_file),
                file.path(csv_dir_full, out_est_caa_file),
                file.path(csv_dir_full, out_est_caa_bio_file),
                file.path(csv_dir_full, out_est_baa_file))

  yrs_in_table <- sort(unique(tbl$Yr))
  min_yr <- min(yrs_in_table)
  max_yr <- max(yrs_in_table)
  start_yr <- ifelse(is.na(start_yr), min_yr, start_yr)
  end_yr <- ifelse(is.na(end_yr), max_yr, end_yr)
  if(start_yr > end_yr){
    start_yr <- min_yr
    end_yr <- max_yr
  }
  start_yr <- ifelse(start_yr < min_yr, min_yr, start_yr)
  end_yr <- ifelse(end_yr > max_yr, max_yr, end_yr)
  yrs <- start_yr:end_yr

  dat <- tbl %>%
    filter(Yr %in% yrs) %>%
    rename(Year = Yr) %>%
    mutate(Year = as.character(Year))
  write_csv(dat, fn)
  dat <- dat %>%
    mutate_at(.vars = vars(-Year), ~{f(.x, digits)})
  names(dat)[length(names(dat))] <- paste0(names(dat)[length(names(dat))], "+")

  # Add latex headers
  ages <- colnames(dat)[-1]
  ages_tex <- map_chr(ages, latex_bold)
  ages_tex <- paste0(latex_paste(ages_tex), latex_nline)

  # Add the extra header spanning multiple columns
  addtorow <- list()
  addtorow$pos <- list()
  addtorow$pos[[1]] <- -1

  addtorow$command <-
    paste0(latex_hline,
           latex_bold("Year"),
           latex_amp(),
           latex_mcol(ncol(dat) - 1,
                      "c",
                      latex_bold("Age")),
           latex_nline,
           latex_amp(),
           ages_tex,
           latex_hline)

  addtorow$command <- paste0(addtorow$command,
                             latex_continue(ncol(dat), addtorow$command))

  # Make the size string for font and space size
  size_string <- latex_size_str(font_size, space_size)
  return(print(xtable(dat,
                      caption = xcaption,
                      label = xlabel,
                      align = get.align(ncol(dat))),
               caption.placement = "top",
               include.rownames = FALSE,
               include.colnames = FALSE,
               sanitize.text.function = function(x){x},
               size = size_string,
               add.to.row = addtorow,
               table.placement = "H",
               tabular.environment = "longtable",
               #latex.environments = "center",
               hline.after = NULL))

}
