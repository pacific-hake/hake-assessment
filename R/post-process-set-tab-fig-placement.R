#' Modify the tex code `x` by changing the table or figure label placement
#' code for the `knitr_label` to the value of `place`
#'
#' @param x Tex code, as a vector of lines read in from a TeX file by
#' @param type One of `table` or `figure`
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
post_process_set_tab_fig_placement <- function(x,
                                               type = c("table", "figure"),
                                               knitr_label,
                                               place,
                                               n_lines = 5){

  type <- match.arg(type)

  if(type == "table"){
    label <- paste0("\\\\caption\\{\\\\label\\{tab:", knitr_label)
  }else if(type == "figure"){
    label <- paste0("\\/", knitr_label)
  }

  ind <- grep(label, x)
  if(!length(ind)){
    warning(type, " label `", label, "`, was not found in the tex file.",
            "It is needed to set the plot placement in post-processing")
    return(x)
  }
  if(length(ind) > 1){
    stop("There was more than 1 ", type," label `", label, "`, found in ",
         "the tex file. Only one can exist to correctly set the plot ",
         "placement in post-processing",
         call. = FALSE)
  }

  srch_lines <- x[(ind - n_lines):(ind - 1)]
  if(type == "table"){
    beg_ind <- grep("\\\\begin\\{.*?table\\}", srch_lines)
  }else if(type == "figure"){
    beg_ind <- grep("\\\\begin\\{figure\\}", srch_lines)
  }

  if(!length(beg_ind)){
    type_txt <- ifelse(type == "table", ".*?table", "figure")
    stop("Did not find the line \\begin{", type_txt, "} associated with the ",
         type, " inclusion line `", x[ind], "`. Consider increasing ",
         "the number of lines searched above it",
         call. = FALSE)
  }
  beg_ind <- ind - n_lines + beg_ind - 1

  # Replace any placement values
  if(type == "table"){
    x[beg_ind] <- gsub("\\\\begin", "\\\\begin", x[beg_ind])
    has_sq_brac <- length(grep("\\[", x[beg_ind]))
    if(has_sq_brac){
      x[beg_ind] <- gsub("(\\[[a-zA-Z\\!]+\\])(.*)$",
                         paste0("[", place, "]\\2"), x[beg_ind])
    }else{
      x[beg_ind] <- paste0(x[beg_ind], "[", place, "]")
    }
  }else if(type == "figure"){
    x[beg_ind] <- gsub("(\\[[a-zA-Z\\!]+\\])(.*)$",
                       paste0("[", place, "]\\2"), x[beg_ind])
  }

  x
}
