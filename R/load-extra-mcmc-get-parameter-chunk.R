#' Extracts a vector of lines of data or output (a chunk) from each
#' element of a list of identically-formatted vectors, typically read
#' in from files with identical formats (SS3 Report files).
#'
#' @details
#' This function is different from the [load_extra_mcmc_get_chunk()] function
#' in the output it reads in. The former reads in outputs that each have
#' their own header associated with it, this function reads in output found
#' in the PARAMETERS section of the Report_mce_xxx.sso files which have a
#' shared header at the top and are each listed below that. The header must
#' be found and read in and attached to the outputs requested through the
#' `beg_pat` and `end_pat` regular expressions.
#'
#'
#' @param lst A list of identically-formatted vectors, typically from a
#' call to [readLines()] where each file is identically-formatted
#' @param beg_pat A regular expression that marks the beginning of the chunk
#' of data (the first line to include) in the PARAMETERS section
#' @param end_pat A regular expression that marks the last line of data to
#' include in the PARAMETERS section
#'
#' @return A list of [tibble::tibble()] of output or data, each tibble will
#' have the exact same dimensions and column names
#'
#' @export
load_extra_mcmc_get_parameter_chunk <- function(lst,
                                                beg_pat,
                                                end_pat,
                                                ...){

  # Use this as the output to match regular expressions for
  x <- lst[[1]]

  header_ind <- grep("PARAMETERS", x)
  # Remove the one before 200 because it is just a definition line for
  # PARAMETERS
  header_ind <- header_ind[header_ind > 200]
  if(!length(header_ind)){
    warning("Could not find a PARAMETERS header line in the input matching
            the regular expression `", beg_pat, "`")
    return(NA)
  }
  if(length(header_ind) > 1){
    warning("More than one match in the input for the regular expression `",
            beg_pat, "`. This is supposed to math the PARAMETERS line You ",
            "should fix the regular expression so that only the line you ",
            "want matches")
    return(NA)
  }
  # Skip any blank lines between the PARAMETERS line and the header line
  header_ind <- header_ind + 1
  while(x[header_ind] == "" && header_ind < length(x)){
    header_ind <- header_ind - 1
  }

  header <- str_split(x[header_ind], " +")[[1]]

  start_ind <- grep(beg_pat, x)

  if(!length(start_ind)){
    warning("Could not find a line in the input matching the regular ",
            "expression `", beg_pat, "`")
    return(NA)
  }
  if(length(start_ind) > 1){
    warning("More than one match in the input for the regular expression `",
            beg_pat, "`. Using the first one. You should fix the regular ",
            "expression so that only the line you want matches")
    start_ind <- start_ind[1]
  }

  end_ind <- grep(end_pat, x)
  if(!length(end_ind)){
    warning("Could not find a line in the report file matching the regular ",
            "expression `", end_pat, "`")
    return(NA)
  }
  if(length(end_ind) > 1){
    warning("More than one match in the input for the regular expression `",
            end_pat, "`. Using the first one. You should fix the regular ",
            "expression so that only the line you want matches")
    end_ind <- end_ind[1]
  }
  if(end_ind < start_ind){
    warning("The ending regular expression matched a line before the beginning ",
            "regular expression match. Check your regular expressions and try ",
            "again")
    return(NA)
  }

  # Multiple columns with the name XX appear in some output (catage in SS3)
  # This line doesn't affect anything if XX is not a column name
  map(lst, ~{
    .x[start_ind:end_ind] |>
      map_dfr(\(line){
        xx <- str_split(line, " +")[[1]]
        xx <- xx[xx != ""]
        names(xx) <- header[seq_along(xx)]
        as_tibble_row(xx)
      })
  })
}
