#' Replace chapter with section for all sections, and prepend a "sub" to
#' each section, subsection etc to lower each by one level
#'
#' @param x Tex code, as a vector of lines read in from a TeX file by
#' [readLines()]
#'
#' @return The modified Tex code, as a vector
#' @export
post_process_convert_to_article <- function(x){

  section_inds <- grep("\\\\section\\{", x)
  subsection_inds <- grep("\\\\subsection\\{", x)
  subsubsection_inds <- grep("\\\\subsubsection\\{", x)
  subsubsubsection_inds <- grep("\\\\subsubsubsection\\{", x)

  # Change sections to subsections etc
  if(length(section_inds))
    x[section_inds] <- gsub("section",
                            "subsection",
                            x[section_inds])
  if(length(subsection_inds))
    x[subsection_inds] <- gsub("subsection",
                               "subsubsection",
                               x[subsection_inds])
  if(length(subsubsection_inds))
    x[subsubsection_inds] <- gsub("subsubsection",
                                  "subsubsubsection",
                                  x[subsubsection_inds])
  if(length(subsubsubsection_inds))
    x[subsubsubsection_inds] <- gsub("subsubsubsection",
                                     "subsubsubsubsection",
                                     x[subsubsubsection_inds])
  x <- gsub("\\\\chapter", "\\\\section", x)
  x <- gsub("\\{chapter\\}", "{section}", x)

  x
}