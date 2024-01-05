#' Modify the font type and size in the LaTeX code
#'
#' @param x Tex code, as a vector of lines read in from a TeX file by
#' [readLines()]
#' @param font The name of the font, assumed to be present on the machine
#' @param font_size_pt The font size to use, in pt. Some fonts do not support
#' all sizes so this may have no effect depending on font
#' @param ... Absorbs arguments meant for other functions
#'
#' @return The modified Tex code, as a vector
#' @export
post_process_modify_font_info <- function(x,
                                          font = NULL,
                                          font_size_pt = NULL,
                                          ...){

  font <- font %||% "IBM Plex Serif"
  font_size_pt <- font_size_pt %||% "12pt"

  font_size_has_pt <- grep("^[0-9]+pt$", font_size_pt)
  if(!length(font_size_has_pt)){
    if(is.numeric(font_size_pt)){
      font_size_pt <- paste0(font_size_pt, "pt")
    }else{
      warning("`font_size_pt` in the YAML header is not a numeric value. ",
              "It will be set to 12pt")
      font_size_pt <- "12pt"
    }
  }

  # Set font size ----
  start_ind <- grep("\\\\documentclass\\[?", x)
  if(!length(start_ind)){
    stop("\\documentclass not found in the preamble LaTex code")
  }
  if(length(start_ind) > 1){
    stop("\\documentclass found more than once in the preamble LaTex code")
  }

  # \documentclass code will always look like this coming from Pandoc:
  #
  # \documentclass[
  # 11pt,
  # ]{article}
  #
  # or for no font size:
  #
  # \documentclass[
  # ]{article}

  # Match the last line (closing square brace)
  end_ind <- grep("\\]\\{article}$", x)
  if(!length(end_ind)){
    stop("Could not find `]{article}` in the LaTeX code produced by Pandoc ",
         "Make sure this regular expression is still correct in the case ",
         "Pandoc writes its documentclass outpu differently in new realeases")
  }
  if(length(end_ind) > 1){
    stop("Found `]{article}` in the LaTeX code produced by Pandoc ")
  }

  pre <- x[1:start_ind]
  post <- x[end_ind:length(x)]
  size <- paste0(font_size_pt, ",")

  x <- c(pre, size, post)

  # Set font ----

  mainfont_ind <- grep("\\\\setmainfont\\{IBM Plex Serif\\}", x)
  if(!length(mainfont_ind)){
    stop("\\setmainfont{IBM Plex Serif} not found in the preamble LaTeX ",
         "code. This must be present as the default font in the preamble ",
         "or the font selection in the YAML will have no effect")
  }
  if(length(mainfont_ind) > 1){
    stop("\\setmainfont{IBM Plex Serif} found more then once in the ",
         "preamble LaTeX code. This must be present only once as the ",
         "default font in the preamble or the font selection in the YAML ",
         "will have no effect")
  }
  font <- tolower(font)
  pre <- x[1:(mainfont_ind - 1)]
  post <- x[(mainfont_ind + 1):length(x)]

 if(font == "baskervillef"){
  new_font_dat <-
    c("\\setmainfont{BaskervilleF-Regular.otf}[",
      "              BoldFont = BaskervilleF-Bold.otf,",
      "              ItalicFont = BaskervilleF-Italic.otf,",
      "              BoldItalicFont = BaskervilleF-BoldItalic.otf]")
  }else if(font == "ibm plex sans"){
    new_font_dat <- "\\setmainfont{IBM Plex Sans}"
  }else if(font == "ibm plex serif"){
    new_font_dat <- "\\setmainfont{IBM Plex Serif}"
  }else if(font == "roboto"){
    new_font_dat <-
      c("\\setmainfont{Roboto-Regular.otf}[",
        "              BoldFont = Roboto-Bold.otf,",
        "              ItalicFont = Roboto-Italic.otf,",
        "              BoldItalicFont = Roboto-BoldItalic.otf]")
  }else if(font == "robotoserif"){
    new_font_dat <-
      c("\\setmainfont{RobotoSerif-Regular.otf}[",
        "              BoldFont = RobotoSerif-Bold.otf,",
        "              ItalicFont = RobotoSerif-Italic.otf,",
        "              BoldItalicFont = RobotoSerif-BoldItalic.otf]")
  }else if(font == "texgyrepagella"){
    new_font_dat <-
      c("\\setmainfont{texgyrepagella-regular.otf}[",
        "              BoldFont = texgyrepagella-bold.otf,",
        "              ItalicFont = texgyrepagella-italic.otf,",
        "              BoldItalicFont = texgyrepagella-bolditalic.otf]")
  }else if(font == "qtpalatine"){
    new_font_dat <- "\\setmainfont{QTPalatine}"
  }else{
    stop("Font not available (not coded into post-processor): `", font, "`")
  }

  x <- c(pre, new_font_dat, post)

  x
}