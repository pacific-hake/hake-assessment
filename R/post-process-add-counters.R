#' Add LaTeX counter code so that the Executive summary has letters for
#' tables and figures, and the rest of the document has numbers
#'
#' @param x Tex code, as a vector of lines read in from a TeX file by
#' [readLines()]
#' @param ... Absorbs arguments meant for other functions
#'
#' @return The modified Tex code, as a vector
#' @export
post_process_add_counters <- function(x,
                                      ...){

  # Insert commands to change numbering to alpha (letters) for the
  # Executive summary
  pat <- "\\\\rfoot\\{Executive summary\\}"
  ind <- grep(pat, x)
  if(!length(ind)){
   warning("Could not find the Executive summary right footer line to insert ",
        "the letter numbering information for tables and figures")
    return(x)
  }
  if(length(ind) > 1){
    stop("Multiple lines matched the Executive summary right footer regular ",
         "expression `", pat, "`")
  }

  lst <- post_process_extract_chunks(x, ind, ind)
  lst$between[[1]] <- c(lst$between[[1]],
                        "\\renewcommand{\\thetable}{\\alph{table}}",
                        "\\renewcommand{\\thefigure}{\\alph{figure}}")

  x <- post_process_interlace_chunks(lst)

  # Insert commands to change numbering to arabic (regular numbers) for the
  # rest of the document

  pat <- "EXECUTIVE SUMMARY EOF"
  ind <- grep(pat, x)
  if(!length(ind)){
    stop("Could not find the `EXECUTIVE SUMMARY EOF` marker which is used ",
         "to insert the numbering information for tables and figures for ",
         "the non-Executive Summary part of the document")
  }
  if(length(ind) > 1){
    stop("Multiple lines matched the `EXECUTIVE SUMMARY EOF` regular ",
         "expression `", pat, "`")
  }

  lst <- post_process_extract_chunks(x, ind, ind)
  lst$between[[1]] <- c("%",
                        "% The following code was injected by",
                        "% hake::post_process_add_counters()",
                        "%",
                        "\\renewcommand{\\thetable}{\\arabic{table}}",
                        "\\setcounter{table}{0}",
                        "\\renewcommand{\\thefigure}{\\arabic{figure}}",
                        "\\setcounter{figure}{0}",
                        "%",
                        "% End of injected code",
                        "%")

  post_process_interlace_chunks(lst)
}