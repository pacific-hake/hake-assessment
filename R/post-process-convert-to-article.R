#' Replace book references with article and chapter with section, and
#' add a "sub" to each section, subsection etc to lower each by one level
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
post_process_convert_to_article <- function(x){

  x <- gsub("\\{book\\}", "\\{article\\}", x)

  section_inds <- grep("\\\\section", x)
  section_curly_inds <- grep("\\{section\\}", x)

  subsection_inds <- grep("\\\\subsection", x)
  subsection_curly_inds <- grep("\\{subsection\\}", x)

  subsubsection_inds <- grep("\\\\subsubsection", x)
  subsubsection_curly_inds <- grep("\\{subsubsection\\}", x)

  subsubsubsection_inds <- grep("\\\\subsubsubsection", x)
  subsubsubsection_curly_inds <- grep("\\{subsubsubsection\\}", x)

  # Change sections to subsections etc
  if(length(section_inds))
    x[section_inds] <- gsub("section",
                            "subsection",
                            x[section_inds])
  if(length(section_curly_inds))
    x[section_curly_inds] <- gsub("section",
                                  "subsection",
                                  x[section_curly_inds])

  if(length(subsection_inds))
    x[subsection_inds] <- gsub("subsection",
                               "subsubsection",
                               x[subsection_inds])
  if(length(subsection_curly_inds))
    x[subsection_curly_inds] <- gsub("subsection",
                                     "subsubsection",
                                     x[subsection_curly_inds])

  if(length(subsubsection_inds))
    x[subsubsection_inds] <- gsub("subsubsection",
                                  "subsubsubsection",
                                  x[subsubsection_inds])
  if(length(subsubsection_curly_inds))
    x[subsubsection_curly_inds] <- gsub("subsubsection",
                                        "subsubsubsection",
                                        x[subsubsection_curly_inds])

  if(length(subsubsubsection_inds))
    x[subsubsubsection_inds] <- gsub("subsubsubsection",
                                     "subsubsubsubsection",
                                     x[subsubsubsection_inds])
  if(length(subsubsubsection_curly_inds))
    x[subsubsubsection_curly_inds] <- gsub("subsubsubsection",
                                           "subsubsubsubsection",
                                           x[subsubsubsection_curly_inds])


  # Change chapters to sections
  x <- gsub("\\\\chapter", "\\\\section", x)
  x <- gsub("\\{chapter\\}", "{section}", x)

  x
}