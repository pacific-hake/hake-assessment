post_process_add_xcolor_package <- function(x, ...){

  ind <- grep("\\\\PassOptionsToPackage\\{hyphens\\}\\{url\\}", x)
  pre <- x[1:ind]
  post <- x[(ind + 1):length(x)]

  x <- c(pre,
    "\\PassOptionsToPackage{dvipsnames,svgnames,x11names}{xcolor}",
    post)

  x
}
