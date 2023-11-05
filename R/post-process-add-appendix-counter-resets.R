#' Add appendix counter rests LaTeX code for equations, figures, and tables
#' so that each lettered appendix starts at 1 again for those things
#'
#' @param x Tex code, as a vector of lines read in from a TeX file by
#' [readLines()]
#' @param ... Absorbs arguments meant for other functions
#'
#' @return The modified Tex code, as a vector
#' @export
post_process_add_appendix_counter_resets <- function(x,
                                                     ...){

  # Get the lines of the appendix starts
  app_inds <- grep("\\\\label\\{sec:app", x)

  if(!length(app_inds)){
    # No appendices found
    return(x)
  }

  for(ind in seq_along(app_inds)){
    # Need to re-acquire the indices every loop iteration because x is changing
    app_inds <- grep("\\\\label\\{sec:app", x)
    pre <- x[1:app_inds[ind]]
    post <- x[(app_inds[ind] + 1):length(x)]
    x <- c(pre,
           "%",
           "% The following code was injected by",
           "% hake::post_process_add_appendix_counter_resets()",
           "%",
           "\\setcounter{equation}{0}",
           "\\setcounter{figure}{0}",
           "\\setcounter{table}{0}",
           "%",
           "% End of injected code",
           "%",
           post)
  }

  x
}