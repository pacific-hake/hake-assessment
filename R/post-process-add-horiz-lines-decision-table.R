#' Adds two horizontal lines to the decision tables
#'
#' @param x Tex code, as a vector of lines read in from a TeX file by
#' [readLines()]
#' @param ... Absorbs arguments meant for other functions
#'
#' @return The modified Tex code, as a vector
#' @export
post_process_add_horiz_lines_decision_table <- function(x,
                                                        ...){
  ind <- grep("\\\\selectfont Relative spawning biomass\\}\\}\\}", x)
  if(length(ind)){
    for(i in seq_along(x[ind])){
      x[ind[i]] <- paste0(x[ind[i]], " \\cline{1-3} \\cline{5-7}")
    }
  }

  ind2 <- grep("\\\\selectfont Relative fishing intensity\\}\\}\\}", x)
  if(length(ind2)){
    for(i in seq_along(x[ind2])){
      x[ind2[i]] <- paste0(x[ind2[i]], " \\cline{1-3} \\cline{4-6}")
    }
  }

  # Make the two horizontal lines in the fishing intensity decision table
  # that underline 'Catch alternative' and 'Fishing intensity' have a gap
  # between them. Without this, they blend into one line and the two groups
  # cannot be distinguished
  ind3 <- grep("\\\\caption\\{\\\\label\\{tab:es-decisions-spr-tab\\}", x)
  if(length(ind3)){
    # Is the line above the caption with label the beginning of the table?
    # If so, make the modification
    is_begin_table <- grep(paste0("\\{C\\{[0-9\\.]+cm\\} rr ",
                                  "C\\{[0-9\\.]+cm\\} C\\{[0-9\\.]+cm\\} ",
                                  "C\\{[0-9\\.]+cm\\}\\}"), x[ind3 - 1])
    if(is_begin_table){
      begin_tab_ind <- ind3 - 1
      # Use look behind regular expression to match '{C' that is preceded
      # by a '}' or ']'
      x[begin_tab_ind] <- gsub("(?<=(\\}|\\]))\\{C",
                               "{@{\\\\extracolsep{10pt}}C",
                               x[begin_tab_ind],
                               perl = TRUE)
      x[begin_tab_ind] <- gsub("}$",
                               "@{}}",
                               x[begin_tab_ind],
                               perl = TRUE)
    }
  }

  x
}