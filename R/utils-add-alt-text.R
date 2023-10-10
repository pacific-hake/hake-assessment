#' Add alternative text to figures in the document via the `tagpdf` LaTeX
#' package
#'
#' @details Searches the given TEX file for the `includegraphics{}`,
#' `\\begin{caption}`, `\\end{caption}`, `\\begin{knitrout}`,
#' `\\end{knitrout}`, and `\\label{fig.*}` and manipulates the
#' structure of those commands with the figure tagging commands
#' injected. Alternative text comes from a data frame created
#' during the knitting process via `knitr` hooks that read in the
#' text from the `alt.text` knitr chunk tags in the figures. Those
#' have to be manually changed each year
#'
#' @param x Tex code, as a vector of lines read in from a TeX file by
#' [readLines()]
#'
#' @return The modified Tex code, as a vector
#' @export
add_alt_text <- function(x){

  # Strip whitespace from the beginning of all lines
  x <- gsub("^[[:space:]]+", "\\1", x)
  # Strip out lines that begin with comment character (after removing
  # leading whitespace)
  x <- x[!grepl("^%", x)]

  inc_graphics_all_pat <- "\\\\includegraphics"
  all_figure_inds <- grep(inc_graphics_all_pat, x)

  if(!length(all_figure_inds)){
    # No figures in document
    return(x)
  }

  # Get indices of file-based figures
  is_knitr <- map_lgl(x[all_figure_inds], ~{
    png_pat <- knitr_figures_dir
    length(grep(png_pat, .x))
  })
  knitr_inds <- all_figure_inds[is_knitr]
  # Get chunk names for the knitr-based figures
  labels <- gsub(paste0(".*\\{",
                        knitr_figures_dir,
                        "(.*?)-fig-[0-9]+\\} *$"), "\\1", x[knitr_inds])

  alt_text_labels <- paste0("(ref:", labels, "-alt)")
  alt_text <- map(alt_text_labels, ~{
    extract_alt_text(.x)
  })

  figure_labels <- paste0(labels, "-fig")
  for(label in seq_along(figure_labels)){
    x <- alt_text_inject_tags(x, figure_labels[label], alt_text[[label]])
  }

  x
}
