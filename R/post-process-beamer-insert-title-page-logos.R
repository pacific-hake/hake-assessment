#' Insert the NOAA and DFO logos and a hake picture on the title page of a
#' beamer presentation
#'
#' @param x Tex code, as a vector of lines read in from a TeX file by
#' [readLines()]
#' @param test Logical. If `TRUE`, this is a test compilation in a temporary
#' directory. If `FALSE`, it is the real compilation of the full presentation,
#' in the hake directory structure
#' @param images_dir The directory where the title picture and logos are
#' found for beamer presentations
#' @param title_page_image The filename of the image to place on the title page
#' of the beamer presentation. If `NULL`, a blank image will appear
#' @param title_page_image_height_in The height in inches for the title page
#' image. Only applies if `title_page_image` is not `NULL`
#' @param title_page_image_width_in  The width in inches for the title page
#' image. Only applies if `title_page_image` is not `NULL`
#' @param ... Arguments passed to all the post-processing functions
#'
#' @return The modified Tex code, as a vector
#' @export
post_process_beamer_insert_title_page_logos <- function(
    x,
    test,
    images_dir,
    title_page_image,
    title_page_image_height_in,
    title_page_image_width_in,
    ...){

  # Insert title page logos
  ind <- grep("\\usepackage\\{pgfpages\\}", x)

  if(!length(ind)){
    stop("The \\usepackage{pgfpages} tag was not found. It must be present ",
         " to insert the custom beamer title page")
  }
  if(length(ind) > 1){
    stop("The \\usepackage{pgfpages} tag was found more than once in the ",
         "document. It must appear only once to insert the custom beamer ",
         "title page")
  }

  # If NULL, set default values
  test <- test %||% FALSE
  title_page_image_height_in <- title_page_image_height_in %||% 1
  title_page_image_width_in <- title_page_image_width_in %||% 2
  images_dir <- images_dir %||% "../../images"

  images_path <- ifelse(test,
                        "images",
                        images_dir)
  # Add a backslash at the end if one is not present
  if(!length(grep("\\/$", images_path))){
    images_path <- gsub("\\/$", "", images_path)
  }

  pre <- x[1:ind]
  post <- x[(ind + 1):length(x)]

  dat <- c(
    "",
    "\\setbeamertemplate{title page}",
    "{",
    paste0("  \\includegraphics[height=0.5in]{", images_path, "/NOAA.eps}"),
    "  \\hfill",
    paste0("  \\includegraphics[height=0.5in]{", images_path, "/DFO.eps}"),
    "",
    "  \\vskip0pt plus 1fill",
    "  \\begin{center}",
    "  {\\usebeamerfont{title}\\usebeamercolor[fg]{title}\\inserttitle}\\\\",
    "  \\vskip22pt")

  if(is.null(title_page_image)){
    dat <- c(dat,
             paste0("  \\includegraphics[height=1in, width=4in]{",
                    images_path, "/blank.png}\\\\"))
  }else{
    title_page_image <- file.path(images_path, title_page_image)
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
