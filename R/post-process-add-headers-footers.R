#' Add left and right footer text to the pages
#'
#' @param x Tex code, as a vector of lines read in from a TeX file by
#' [readLines()]
#' @param headrule_width_pt Width of the primary headrule line in pts
#' @param headrule_double_lines Logical. If `TRUE`, make headrule double lines
#' @param headrule_double_width_pt Width of the secondary (top) headrule line
#' in pts if `headrule_double_lines` is `TRUE`
#' @param headrule_double_space_mm Space between the two headrule lines in mm
#' if `headrule_double_lines` is `TRUE`
#' @param footrule_width_pt Width of the primary footrule line in pts
#' @param footrule_double_lines Logical. If `TRUE`, make footrule double lines
#' @param footrule_double_width_pt Width of the secondary (top) footrule line
#' in pts if `footrule_double_lines` is `TRUE`
#' @param footrule_double_space_mm Space between the two footrule lines in mm
#' if `footrule_double_lines` is `TRUE`
#' @param ... Absorbs arguments meant for other functions
#'
#' @return The modified Tex code, as a vector
#' @export
post_process_add_headers_footers <- function(x,
                                             headrule_width_pt = NULL,
                                             headrule_double_lines = NULL,
                                             headrule_double_width_pt = NULL,
                                             headrule_double_space_mm = NULL,
                                             footrule_width_pt = NULL,
                                             footrule_double_lines = NULL,
                                             footrule_double_width_pt = NULL,
                                             footrule_double_space_mm = NULL,
                                             ...){

  headrule_width_pt <- headrule_width_pt %||% 0
  headrule_double_lines <- headrule_double_lines %||% FALSE
  headrule_double_width_pt <- headrule_double_width_pt %||% 2
  headrule_double_space_mm <- headrule_double_space_mm %||% 1
  footrule_width_pt <- footrule_width_pt %||% 0
  footrule_double_lines <- footrule_double_lines %||% FALSE
  footrule_double_width_pt <- footrule_double_width_pt %||% 2
  footrule_double_space_mm <- footrule_double_space_mm %||% 1

  left_ind <- grep("fancyfoot\\[L\\]\\{\\}", x)
  if(!length(left_ind)){
    stop("`\\fancyfoot[L]{}` not found in the preamble.tex code")
  }
  right_ind <- grep("fancyfoot\\[R\\]\\{\\}", x)
  if(!length(right_ind)){
    stop("`\\fancyfoot[R]{}` not found in the preamble.tex code")
  }

  x[left_ind] <- paste0("\\fancyfoot[L]{", footer_left, "}")
  x[right_ind] <- paste0("\\fancyfoot[R]{", footer_right, "}")

  # Header and Footer lines
  hdr_tag <- paste0("INSERT headrule and footrule here - do not remove ",
                    "this comment")
  ind <- grep(hdr_tag, x)
  if(!length(ind)){
    stop("Preprocessing tag '", hdr_tag, "' not found in the preamble. ",
         "It should be immediately after the fancyhead and fancyfoot ",
         "definitions")
  }
  pre <- x[1:ind]
  post <- x[(ind + 1):length(x)]
  dat <- NULL

  if(headrule_double_lines){
    dat <- c(dat,
             "\\renewcommand\\headrule{\\begin{minipage}{1\\textwidth}",
             paste0("\\hrule width \\hsize height ", headrule_double_width_pt,
                    "pt \\kern ", headrule_double_space_mm,
                    "mm \\hrule width \\hsize"),
             "\\end{minipage}\\par}")
  }else{
    dat <- c(dat,
             "\\renewcommand\\headrule{\\begin{minipage}{1\\textwidth}",
             paste0("\\hrule width \\hsize height ", headrule_width_pt, "pt"),
             "\\end{minipage}\\par}")
  }

  if(footrule_double_lines){
    dat <- c(dat,
             "\\renewcommand\\footrule{\\begin{minipage}{1\\textwidth}",
             paste0("\\hrule width \\hsize height ", footrule_double_width_pt,
                    "pt \\kern ", footrule_double_space_mm,
                    "mm \\hrule width \\hsize"),
             "\\end{minipage}\\par}")
  }else{
    dat <- c(dat,
             "\\renewcommand\\footrule{\\begin{minipage}{1\\textwidth}",
             paste0("\\hrule width \\hsize height ", footrule_width_pt, "pt"),
             "\\end{minipage}\\par}")
  }

  x <- c(pre, dat, post)

  x
}