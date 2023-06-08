#' Inject LaTeX float placement codes for figure and tables, based on their
#' knitr chunk label names
#'
#' @param x Tex code, as a vector of lines read in from a TeX file by
#' [readLines()]
#' @param ... Absorbs arguments meant for other functions
#'
#' @return The modified Tex code, as a vector
#' @export
post_process_set_object_placement <- function(x, ...){

  settings_fn <- "object-placement.csv"
  doc_dr <- here("doc")
  if(!dir.exists(doc_dr)){
    stop("The `doc` directory which contains the file `", settings_fn, " ",
         "does not exist",
         call. = FALSE)
  }
  settings_fn <- here("doc", settings_fn)
  if(!file.exists(settings_fn)){
    stop("The figure/table placement CSV file does not exist. This file ",
         "should be here:\n", settings_fn,
         call. = FALSE)
  }

  settings_df <- read_csv(settings_fn,
                          col_types = cols(),
                          comment = "#",
                          show_col_types = FALSE)

  if(nrow(settings_df) < 1){
    return(invisible())
  }

  pwalk(settings_df, ~{
    row <- list(...)
    x <<- post_process_set_tab_fig_placement(
      x,
      type = row$type,
      knitr_label = ifelse(is.na(row$knitr_label),
                           row$file_name,
                           row$knitr_label),
      place = row$placement)
  })

  x
}