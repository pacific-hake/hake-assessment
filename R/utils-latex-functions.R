# Newline
latex_nline <- " \\\\ "

# Horizontal line
latex_hline <- " \\hline "

#' Create a string with `n` ampersands separated by spaces
#'
#' @details
#' The string will have one leading and one trailing space
#'
#' @param n The number of space-ampersands to return
#'
#' @return A string with `n` ampersands separated by spaces
#' @export
latex_amp <- function(n = 1){
  paste0(rep(" &", n), " ", collapse = "")
}

#' Wrap the given text with the latex \\textbf{} macro around it
#'
#' @param txt The text
#'
#' @return The given text with the latex \\textbf{} macro around it
#' @export
latex_bold <- function(txt){
  paste0("\\textbf{", txt, "}")
}

#' Wrap the given text with the latex \\emph{} macro around it
#'
#' @param txt The text
#'
#' @return The given text with the latex \\emph{} macro around it
#' @export
latex_italics <- function(txt){
  paste0("\\emph{", txt, "}")
}

#' Wrap the given text with the latex \\underline{} macro around it
#'
#' @param txt The text
#'
#' @return The given text with the latex \\underline{} macro around it
#' @export
latex_under <- function(txt){
  paste0("\\underline{", txt, "}")
}

#' Wrap the given text with the latex \\multicolumnn{} macro around it
#'
#' @param ncol The number of columns
#' @param just Justification, e.g. "l", "c", or "r" for left, center, right
#' @param txt The text
#'
#' @return The given text with the latex \\multicolumn{} macro around it
#' @export
latex_mcol <- function(ncol, just, txt){
  paste0("\\multicolumn{", ncol, "}{", just, "}{", txt, "}")
}

#' Creates a string which has the given font size and space size applied
#'
#' @param fnt.size The font size
#' @param spc.size The space size (between text size)
#'
#' @return A string which has the given font size and space size applied
#' @export
latex_size_str <- function(fnt_size, spc_size){
  paste0("\\fontsize{", fnt_size, "}{", spc_size, "}\\selectfont")
}

#' Provide latex code to draw a horizontal line across the columns specified
#'
#' @param cols A string in this format: "1-3" which means the line should go
#' across columns 1 to 3
#'
#' @return A string of latex code to draw a horizontal line across the
#' columns specified
#' @export
latex_cline <- function(cols){
  paste0("\\cline{", cols, "}")
}

#' Creates a latex string with `main_txt` subscripted by `subscr_txt`
#'
#' @param main_txt The main text to subscript
#' @param subscr_txt The subscript text
#'
#' @return A latex string with `main_txt` subscripted by `subscr_txt`
#' @export
latex_subscr <- function(main_txt, subscr_txt){
  paste0(main_txt, "\\textsubscript{", subscr_txt, "}")
}
