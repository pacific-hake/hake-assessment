#' Move certain table captions, usually to left-justify. There is no way
#' to center kable tables and left-justify the captions to the left side
#' of the page
#'
#' @param x Tex code, as a vector of lines read in from a TeX file by
#' [readLines()]
#' @param ... Absorbs arguments meant for other functions
#'
#' @return The modified Tex code, as a vector
#' @export
post_process_move_table_captions <- function(x,
                                             ...){

  # Move a single caption horizontally
  # @param x Tex code, as a vector of lines read in from a TeX file by
  # `readLines()`
  # @param tag The chunk tag found in the knitr code chunk header in which
  # the table is created. Includes leading `tab:` part
  # @param pts The number of `pts` to move the caption. Can be positive or
  # negative
  move_cap <- \(x, tag, pts){

    pat <- paste0("\\\\caption\\{\\\\label\\{",
                  tag,
                  "\\}")
    ind <- grep(pat, x)
    if(length(ind)){
      lst <- post_process_extract_chunks(x, ind, ind)
      lst$inbetween[[1]] <- c(lst$inbetween[[1]],
                              paste0("\\captionsetup{margin=", pts, "pt}"))
      post_process_interlace_chunks(lst)
    }else{
      warning("Did not match the table tag `", tag, "` in the TeX file. ",
              "The table caption could not be adjusted since it doesn't exist")
      x
    }
  }

  settings_fn <- caption_adjustments_fn
  doc_dr <- here("doc")
  if(!dir.exists(doc_dr)){
    stop("The `doc` directory which contains the file `", settings_fn, " ",
         "does not exist")
  }
  settings_fn <- here("doc", settings_fn)
  if(!file.exists(settings_fn)){
    stop("The caption adjustments CSV file does not exist. This file ",
         "should be here:\n", settings_fn)
  }

  settings_df <- read_csv(settings_fn,
                          col_types = cols(),
                          comment = "#",
                          show_col_types = FALSE)

  for(i in seq_len(nrow(settings_df))){
    x <- move_cap(x,
                  settings_df[i, ]$chunk_label,
                  settings_df[i, ]$adjustment)
  }

  x
}