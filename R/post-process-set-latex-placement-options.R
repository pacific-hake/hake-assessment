#' Inject LaTeX float placement codes for figure and tables, based on their
#' knitr chunk label names
#'
#' @param x Tex code, as a vector of lines read in from a TeX file by
#' [readLines()]
#' @param ... Absorbs arguments meant for other functions
#'
#' @return The modified Tex code, as a vector
#' @export
post_process_set_latex_placement_options <- function(x, ...){

  settings_fn <- object_placement_fn
  doc_dr <- here("doc")
  if(!dir.exists(doc_dr)){
    stop("The `doc` directory which contains the file `", settings_fn, " ",
         "does not exist")
  }
  settings_fn <- here("doc", settings_fn)
  if(!file.exists(settings_fn)){
    stop("The figure/table placement CSV file does not exist. This file ",
         "should be here:\n", settings_fn)
  }

  settings_df <- read_csv(settings_fn,
                          col_types = cols(),
                          comment = "#",
                          show_col_types = FALSE)

  if(nrow(settings_df) < 1){
    return(x)
  }

  for(i in seq_len(nrow(settings_df))){
    row_df <- settings_df |> slice(i)
    x <- post_process_object_placement(x, row_df, row_num = i, ...)
  }

  x
}