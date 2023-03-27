#' Modify the tex code `x` by changing the figure label placement code for the
#' knitr_label` to the value of `place`
#'
#' @param x Tex code, as a vector of lines read in from a TeX file by
#' [readLines()]
#' @param figs_dir The name of the directory in which the knitr-built
#' figures reside. Used for matching figure import locations inside the tex
#' file
#' @param knitr_label The knitr chunk label for the chunk which creates the
#' figure
#' @param place One of the following LaTeX placement strings (or a
#' combination of them):
#' - h: Place the float here, i.e., approximately at the same point it occurs
#'      in the source text.
#' - t: Position at the top of the page.
#' - b: Position at the bottom of the page.
#' - p: Put on a special page for floats only.
#' - !: Override internal parameters LaTeX uses for determining “good” float
#'      positions.
#' - H: Place the float at precisely the location in the LaTeX code. This
#'      requires the float package
#' @return The modified Tex code, as a vector
#' @export
set_figure_placement <- function(x,
                                 figs_dir,
                                 knitr_label,
                                 place){

  get_beg_fig_ind <- function(fig_ind, n_lines = 5){

    # Search previous `n-lines` tex lines for \begin{figure} to match the line at
    # `fig_ind`
    srch_lines <- x[(fig_ind - n_lines):(fig_ind - 1)]
    beg_fig_ind <- grep("\\\\begin\\{figure\\}", srch_lines)
    srch_line <- gsub("\\{", "\\\\{", srch_lines[beg_fig_ind])
    srch_line <- gsub("\\}", "\\\\}", srch_line)
    srch_line <- gsub("\\[", "\\\\[", srch_line)
    srch_line <- gsub("\\]", "\\\\]", srch_line)
    srch_line <- gsub("\\\\begin", "\\\\\\\\begin", srch_line)
    if(!length(srch_line)){
      stop("Did not find the line \begin{figure} associated with the ",
           "figure inclusion line `", x[fig_ind], "`. Consider increasing ",
           "the number of lines searched above it",
           call. = FALSE)
    }
    grep(srch_line, x)
  }

  # Replace the figure line in the tex code.
  replace_beg_figure_line <- function(ind, place){
    gsub("(\\[[a-zA-Z\\!]+\\])(.*)$", paste0("[", place, "]\\2"), x[ind])
  }

  fig_label <- paste0(figs_dir, knitr_label)
  fig_ind <- grep(fig_label, x)
  if(!length(fig_ind)){
    warning("Figure label `", knitr_label, "` not found. Bypassing...")
    return(x)
  }

  if(!length(fig_ind)){
    stop("Figure label `", fig_label, "`, was not found in the tex file.",
         "It is needed to set the plot placement in post-processing",
         call. = FALSE)
  }
  if(length(fig_ind) > 1){
    stop("There was more than 1 figure label `", fig_label, "`, found in ",
         "the tex file. Only one can exist to correctly set the plot ",
         "placement in post-processing",
         call. = FALSE)
  }

  beg_fig_ind <- get_beg_fig_ind(fig_ind)
  # Replace any placement values
  x[beg_fig_ind] <- gsub("(\\[[a-zA-Z\\!]+\\])(.*)$",
                         paste0("[", place, "]\\2"), x[beg_fig_ind])

  x
}
