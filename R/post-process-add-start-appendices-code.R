#' Add start appendices LaTeX code
#'
#' @details
#' Add LaTeX code for the start of the appendices section. This is needed
#' to tell the LaTeX compiler to start numbering the sections using letters
#' and set u-p tables, figures, and equations to be of the format A.2 for
#' example. It also adds TOC information so this has to come before the TOC
#' post processing step
#'
#' @param x Tex code, as a vector of lines read in from a TeX file by
#' [readLines()]
#' @param ... Absorbs arguments meant for other functions
#'
#' @return The modified Tex code, as a vector
#' @export
post_process_add_start_appendices_code <- function(x, ...){

  ind <- grep("APPENDICES START HERE", x)
  # Get the lines of the appendix starts
  app_inds <- grep("\\\\label\\{sec:app", x)

  if(!length(ind)){
    if(length(app_inds)){
      warning("Appendix marker text `APPENDICES START HERE` not found. ",
              "You have appendix sections declared so those appendix ",
              "figures, tables, and equations will not be numbered ",
              "correctly (eg. Figure A.2, Table C.4)\n\nDid you forget to ",
              "uncomment the `100-start-appendices.rmd` file in ",
              "`_bookdown.yml`?")
    }
    return(x)
  }
  if(length(ind) > 1){
    stop("You have more than one start appendices tag in your code. ",
         "Search for `APPENDICES START HERE` and make sure there is ",
         "only one")
  }

  pre <- x[1:(ind - 1)]
  post <- x[(ind + 1):length(x)]

  x <- c(pre,
         "%",
         "% The following code was injected by",
         "% hake::post_process_add_start_appendices_code()",
         "%",
         "\\newpage",
         "\\appendix",
         "\\renewcommand{\\theequation}{\\thesection.\\arabic{equation}}",
         "\\renewcommand\\thefigure{\\thesection.\\arabic{figure}}",
         "\\renewcommand\\thetable{\\thesection.\\arabic{table}}",
         "\\addtocontents{toc}{\\bigskip\\medskip\\noindent",
         "    \\textbf{Appendices}\\par}",
         "\\vspace{5mm}",
         "%",
         "% End of injected code",
         "%",
         post)

  x
}