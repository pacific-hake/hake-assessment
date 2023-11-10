#' Insert an image on the title page
#'
#' @param x Tex code, as a vector of lines read in from a TeX file by
#' [readLines()]
#' @param title_page_image The file to include as the title page image
#' @param title_page_image_width_cm The width in cm to use for the picture
#' @param figures_dir The subdirectory of the "doc" directory containing
#' images that have previously been made such as pictures and logos, and all
#' other figures made outside the scope of this project
#' @param ... Absorbs arguments meant for other functions
#'
#' @return The modified Tex code, as a vector
#' @export
post_process_add_picture_to_title_page <- function(x,
                                                   title_page_image = NULL,
                                                   title_page_image_width_cm = NULL,
                                                   figures_dir = NULL,
                                                   ...){

  figures_dir <- figures_dir %||% "image-files"
  title_page_image <- title_page_image %||% file.path(figures_dir,
                                                      "hake-line-drawing.png")
  title_page_image_width_cm <- title_page_image_width_cm %||% 12

  if(!file.exists(title_page_image)){
    stop("The file `", title_page_image, "` does not exist")
  }

  ind <- grep("PREAMBLE EOF", x)
  if(!length(ind)){
    stop("The `PREAMBLE EOF` tag was not found in the preamble LaTeX code. ",
         "Insert it and run again")
  }
  if(length(ind) > 1){
    stop("The `PREAMBLE EOF` tag was found more than once in the preamble ",
         "LaTeX code.")
  }
  pre <- x[1:(ind - 1)]
  post <- x[(ind + 1):length(x)]
  dat <- c("\\pretitle{\\begin{center}}",
           paste0("\\posttitle{\\end{center}\\begin{center}\\LARGE",
                  "\\includegraphics[width=", title_page_image_width_cm,
                  "cm]{", title_page_image, "}\\\\[\\bigskipamount]",
                  "\\end{center}}"))
  x <- c(pre, dat, post)

  x
}