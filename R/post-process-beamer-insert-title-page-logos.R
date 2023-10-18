#' Insert the NOAA and DFO logos and a hake picture on the title page of a
#' beamer presentation
#'
#' @param x Tex code, as a vector of lines read in from a TeX file by
#' [readLines()]
#' @param ... Arguments passed to all the post-processing functions
#'
#' @return The modified Tex code, as a vector
#' @export
post_process_beamer_insert_title_page_logos <- function(x, ...){

  # Insert title page logos
  ind <- grep("\\usepackage\\{pgfpages\\}", x)

  if(!length(ind)){
    stop("The \\usepackage{pgfpages} tag was not found. It must be present ",
         " to insert the custom beamer title page",
         call. = FALSE)
  }
  if(length(ind) > 1){
    stop("The \\usepackage{pgfpages} tag was found more than once in the ",
         "document. It must appear only once to insert the custom beamer ",
         "title page",
         call. = FALSE)
  }

  pre <- x[1:ind]
  post <- x[(ind + 1):length(x)]
  dat <- c(
    "",
    "\\setbeamertemplate{title page}",
    "{",
    "  \\includegraphics[height=0.5in]{../../images/NOAA.eps}",
    "  \\hfill",
    "  \\includegraphics[height=0.5in]{../../images/DFO.eps}",
    "",
    "  \\vskip0pt plus 1fill",
    "  \\begin{center}",
    "  {\\usebeamerfont{title}\\usebeamercolor[fg]{title}\\inserttitle}\\\\",
    "  \\vskip22pt")

  if(is.null(title_page_image)){
    dat <- c(dat,
             paste0("  \\includegraphics[height=1in, width=4in]{",
                    "../../images/blank.png}\\\\"))
  }else{
    dat <- c(dat,
             paste0("  \\includegraphics[height=",
                    title_page_image_height_in,
                    "in, width=",
                    title_page_image_width_in,
                    "in]{",
                    title_page_image,
                    "}\\\\"))
  }

  dat <- c(dat,
           "  \\insertauthor",
           "  \\vskip5pt",
           "  \\insertdate",
           "  \\end{center}",
           "  \\usebeamerfont{subtitle}\\usebeamercolor[fg]{subtitle}\\insertsubtitle\\par",
           "  \\vskip0pt plus 1filll",
           "}",
           "")

  x <- c(pre, dat, post)

  x
}
