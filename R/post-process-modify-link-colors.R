#' Modify reference link colors in the document
#'
#' @param x Tex code, as a vector of lines read in from a TeX file by
#' [readLines()]
#' @param underline_links Logical. If `TRUE` make all the links in the
#' TOC and the section links in the document text be underlined
#' @param color_links Logical. If `TRUE` make all the links in the
#' TOC and the section links in the document text be colored
#' @param toc_underline_link_color If `underline_links` is `TRUE`, this
#' color will be the underline color. See LaTeX package `xcolor` for allowable
#' colors
#' @param toc_link_text_color If `underline_links` is `FALSE`, this color
#' will be the color of the link text (without underlines). See LaTeX package
#' `xcolor` for allowable colors
#' @param cite_underline_link_color If `underline_links` is `TRUE`, this
#' color will be the underline color for citations
#' @param cite_link_text_color Text color for citation links. See
#' `toc_link_text_color`
#' @param url_underline_link_color If `underline_links` is `TRUE`, this
#' color will be the underline color for URLs
#' @param url_link_text_color Text color for URL links. See
#' `toc_link_text_color`
#' @param ... Absorb arguments meant for other functions
#'
#' @return The modified Tex code, as a vector
#' @export
post_process_modify_link_colors <- function(
    x,
    underline_links = NULL,
    color_links = NULL,
    toc_underline_link_color = NULL,
    toc_link_text_color = NULL,
    cite_underline_link_color = NULL,
    cite_link_text_color = NULL,
    url_underline_link_color = NULL,
    url_link_text_color = NULL,
    ...){

  underline_links <- underline_links %||% FALSE
  toc_underline_link_color <- toc_underline_link_color %||% "blue"
  toc_link_text_color <- toc_link_text_color %||% "blue"
  cite_underline_link_color <- cite_underline_link_color %||% "blue"
  cite_link_text_color <- cite_link_text_color %||% "blue"
  url_underline_link_color <- url_underline_link_color %||% "blue"
  url_link_text_color <- url_link_text_color %||% "blue"

  # Find the \hypersetup LaTeX command
  start_ind <- grep("\\\\hypersetup", x)
  if(!length(start_ind)){
    stop("\\hypersetup not found in the LaTeX code. This should have been ",
         "injected by Pandoc so this is a more serious error")
    }
  if(length(start_ind) > 1){
    stop("\\hypersetup was found more than once in the LaTeX code")
  }
  # Find all lines that encompass the \hypersetup command by brace matching
  # Look for closing brace, keeping a count of open ones
  # Assumes closing brace is not on the same line as opening one
  end_ind <- start_ind + 1
  ob_count <- 1
  repeat{
    if(end_ind == length(x)){
      stop("Could not find matching end curly brace for the ",
           "\\hypersetup call")
    }
    if(!ob_count){
      break
    }
    for(i in seq_len(nchar(x[end_ind]))){
      ch <- substr(x[end_ind], i, i)
      if(ch == "{"){
        ob_count <- ob_count + 1
      }else if(ch == "}"){
        ob_count <- ob_count - 1
      }
    }
    end_ind <- end_ind + 1
  }

  # Remove the hypersetup chunk and add replacement
  x <- c(x[1:(start_ind - 1)],
         "%",
         "% The following code was injected by",
         "% hake::post_process_table_of_contents()",
         "%",
         "\\hypersetup{",
         ifelse(underline_links,
                "pdfborderstyle = {/S/U/W 1},",
                "pdfborderstyle = {},"),
         paste0("colorlinks = ", ifelse(color_links,
                                        "true",
                                        "false"),
                ","),
         "plainpages = false,",
         paste0("linkcolor = ",
                toc_link_text_color,
                ","),
         paste0("linkbordercolor = ",
                toc_underline_link_color,
                ","),
         paste0("citecolor = ",
                cite_link_text_color,
                ","),
         ifelse(underline_links,
                paste0("citebordercolor = ",
                       cite_underline_link_color,
                       ","),
                paste0("%citebordercolor = ",
                       cite_underline_link_color,
                       ",")),
         paste0("urlcolor = ",
                url_link_text_color,
                ","),
         ifelse(underline_links,
                paste0("urlbordercolor = ",
                       url_underline_link_color,
                       ","),
                paste0("%urlbordercolor = ",
                       url_underline_link_color,
                       ",")),
         "pdflang = {en-US},",
         paste0("pdftitle = {",
                # `doc_title` created in 002-load-globals.rmd as a global
                doc_title,
                "},"),
         paste0("pdfauthor = {",
                # `doc_author` created in 002-load-globals.rmd as a global
                doc_author,
                "}"),
         "}",
         "%",
         "% End of injected code",
         "%",
         "",
         x[(end_ind + 1):length(x)])

  x
}