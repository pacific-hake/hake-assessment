#' Add alternative text to figures in the document via the `tagpdf` LaTeX
#' package
#'
#' @details
#' Searches the given TEX file for the `includegraphics{}`,
#' finds the correct alternative text for those calls by searching through
#' the project RMD files for the matching `(ref:variable-alt)` definitions
#' and injects the text and the `tagpdf` structural LaTeX lines for
#' web accessibility
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
  # Title page image is totally different in the way it is included so we
  # remove it from the process right at the start
  title_page_hake_pic_ind <- grep("hake\\-picture", x[all_figure_inds])
  all_figure_inds <- all_figure_inds[-title_page_hake_pic_ind]

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
  file_inds <- all_figure_inds[!is_knitr]
  # Get chunk names for the knitr-based figures
  knitr_labels <- gsub(paste0(".*\\{",
                              knitr_figures_dir,
                              "(.*?)-fig-[0-9]+\\} *$"),
                       "\\1",
                       x[knitr_inds])
  file_fns_labels <- gsub(paste0(".*\\{main-figures/",
                                 "(.*?)\\} *$"),
                          "\\1",
                          x[file_inds])

  file_based_ref_labels <- map_chr(file_fns_labels, ~{
    extract_ref_label_from_figure_filename(.x)
  })

  alt_text_files <- map_chr(file_based_ref_labels, ~{
    extract_alt_text(.x)
  })

  knitr_ref_labels <- paste0("(ref:", knitr_labels, "-alt)")
  alt_text_knitr <- map_chr(knitr_ref_labels, ~{
    extract_alt_text(.x)
  })

  # Put the alt text in the correct order
  file_which <- which(all_figure_inds %in% file_inds)
  knitr_which <- which(all_figure_inds %in% knitr_inds)

  all_alt_text <- NULL
  all_alt_text[knitr_which] <- alt_text_knitr
  all_alt_text[file_which] <- alt_text_files

  file_based_fig_labels <- gsub("\\(ref:", "", file_based_ref_labels)
  file_based_fig_labels <- gsub("\\-alt\\)$", "", file_based_fig_labels)
  all_fig_labels <- NULL
  all_fig_labels[knitr_which] <- knitr_labels
  all_fig_labels[file_which] <- file_based_fig_labels
  all_fig_labels <- paste0(all_fig_labels, "-fig")

  # Inject the alt text and accompanying accessibility tags
  for(label in seq_along(all_fig_labels)){
    x <- alt_text_inject_tags(x, all_fig_labels[label], all_alt_text[label])
  }

  x
}
