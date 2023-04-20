#' Add LaTeX vertical space code before all sections with a `hyperref`
#' declared before it
#'
#' @param x Tex code, as a vector of lines read in from a TeX file by
#' [readLines()]
#'
#' @return The modified Tex code, as a vector
#' @export
post_process_subtract_section_space <- function(x){

  inds <- grep("^\\\\hypertarget\\{", x)

  if(!length(inds)){
    warning("No hypertargets were found in the TeX code. These should ",
            "immediately preceed the section/subsection definitions and are ",
            " required to add code to subtract vertical space before the ",
            "section headers")
    return(x)
  }

  for(i in seq_along(inds)){
    inds <- grep("^\\\\hypertarget\\{", x)
    # Special cases - some sections start overlapping if tnegative space
    # is added
    stock_struct_sec <- length(grep("intro-stock-structure",
                                    tolower(x[inds[i]])))
    total_catch_sec <- length(grep("data-total-catch",
                                   tolower(x[inds[i]])))
    acoustic_survey_sec <- length(grep("data-acoustic-survey",
                                   tolower(x[inds[i]])))
    maturity_sec <-  length(grep("data-maturity",
                                 tolower(x[inds[i]])))
    if(stock_struct_sec ||
       total_catch_sec ||
       acoustic_survey_sec ||
       maturity_sec){
      next
    }

    lst <- post_process_extract_chunks(x, inds[i], inds[i])
    lst$between[[1]] <- c("\\vspace{-4mm}",
                          lst$between[[1]])
    x <- post_process_interlace_chunks(lst)
  }

  x
}