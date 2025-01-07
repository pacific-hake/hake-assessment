#' Extracts a vector of lines of data or output (a chunk) from each
#' element of a list of identically-formatted vectors, typically read
#' in from files with identical formats (SS3 Report files)
#'
#' @param lst A list of identically-formatted vectors, typically from a
#' call to [readLines()] where each file is identically-formatted
#' @param beg_pat A regular expression that marks the beginning of the chunk
#' of data
#' @param end_pat A regular expression that marks the end of the chunk of data
#'
#' @return A list of vectors of output or data, each list element will have
#' the exact same number of lines and the same structure within those lines
#' @export
load_extra_mcmc_get_chunk <- function(lst,
                                      beg_pat,
                                      end_pat,
                                      ...){

  # Use this as the output to match regular expressions for
  x <- lst[[1]]

  headmark_ind <- grep(beg_pat, x)

  if(!length(headmark_ind)){
    warning("Could not find a line in the input matching the regular ",
         "expression `", beg_pat, "`")
    return(NA)
  }
  if(length(headmark_ind) > 1){
    warning("More than one match in the input for the regular expression `",
            beg_pat, "`. Using the first one. You should fix the regular ",
            "expression so that only the line you want matches")
    headmark_ind <- headmark_ind[1]
  }

  tailmark_ind <- grep(end_pat, x)
  if(!length(tailmark_ind)){
    warning("Could not find a line in the report file matching the regular ",
         "expression `", end_pat, "`")
    return(NA)
  }
  if(length(tailmark_ind) > 1){
    warning("More than one match in the input for the regular expression `",
            end_pat, "`. Using the first one. You should fix the regular ",
            "expression so that only the line you want matches")
    tailmark_ind <- tailmark_ind[1]
  }
  if(tailmark_ind < headmark_ind){
    warning("The ending regular expression matched a line before the beginning ",
         "regular expression match. Check your regular expressions and try ",
         "again")
    return(NA)
  }else if(tailmark_ind > headmark_ind){
    header_ind <- headmark_ind + 1
    # Skipe NOTES after the maker line
    while(grepl("NOTE", x[header_ind])) {
      header_ind <- header_ind + 1
    }
    # Skip any blank lines between marker and header line
    while(x[header_ind] == "" && header_ind < length(x)){
      header_ind <- header_ind + 1
    }
    if(header_ind == length(x)){
      warning("The beginning marker `beg_pat` = `", beg_pat, "` had only blank ",
           "lines after it, all the way to the end of the input. Check your ",
           "regular expression for errors")
      return(NA)
    }

    # Multiple columns with the name XX appear in catage output (SS3)
    header <- gsub("XX", "", str_split(x[header_ind], " +")[[1]])
    # The above call creates empty strings in the headers if XX exists,
    # so we need to get rid of those
    header <- header[header != ""]
    start_ind <- header_ind + 1

    tail_ind <- tailmark_ind - 1
    # Skip any blank lines between marker and end data line
    while(x[tail_ind] == "" && tail_ind < length(x)){
      tail_ind <- tail_ind - 1
    }
    if(tail_ind == length(x)){
      warning("The ending marker `end_pat` = `", end_pat, "` had only blank ",
           "lines after it, all the way to the end of the input. Check your ",
           "regular expression for errors")
      return(NA)
    }
  }else{
    header <- gsub("XX", "", str_split(x[headmark_ind], " +")[[1]])
    start_ind <- tail_ind <- headmark_ind
  }

  # Multiple columns with the name XX appear in some output (catage in SS3)
  # This line doesn't affect anything if XX is not a column name
  out <- map(lst, ~{gsub("XX", "", .x[start_ind:tail_ind])})

  # Remove any comment lines (lines starting with optional whitespace
  # and then one or more # symbols)
  out <- out |>
    map(~{
      inds <- grep("^\\s*#+.*$", .x)
      if(length(inds)){
        .x <- .x[-inds]
      }
      .x
    })

  list(header = header, lst = out)
}
