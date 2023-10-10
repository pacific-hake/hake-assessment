#' Inject the `tagpdf` code around the figure chunk given by the knitr
#' chunk `label`
#'
#' @param x Tex code, as a vector of lines read in from a TeX file by
#' [readLines()]
#' @param label The label used in the knitr chunk
#' @param alt_text The alternative text to inject
#'
#' @return The modified Tex code, as a vector
#' @export
alt_text_inject_tags <- function(x, label, alt_text){

  includegraphics_ind <- grep(paste0(label, "\\-[0-9]+"), x)
  caption_ind <- grep(paste0(label, "\\}"), x)

  pre <- x[1:(includegraphics_ind - 1)]
  post <- x[(caption_ind + 1):length(x)]
  chunk <- x[includegraphics_ind:(caption_ind - 1)]

  ret <- c(pre,
           paste0("\\tagstructbegin{tag=Figure,alttext=",
                  alt_text,
                  "}"),
           "\\tagmcbegin{tag=Figure}",
           chunk,
           "\\tagmcend",
           "\\tagstructend",
           "\\tagpdfparaOn",
           "\\tagstructbegin{tag=Caption}",
           "\\tagmcbegin{tag=Caption}",
           x[caption_ind],
           "\\tagmcend",
           "\\tagstructend",
           post)

  ret
}
