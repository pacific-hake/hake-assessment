#' Render the Hake assessment document PDF file
#'
#' @param input The name of the index `rmd` file. This should be the
#' same as the first `rmd` file named in the `_bookdown.yml` file.
#' See `input` argument for [bookdown::render_book()]
#' @param output_dir Which directory to build the PDF in. See `output_dir`
#' argument for [bookdown::render_book()]
#' @param ... Arguments to pass to the [bookdown::render_book()] and
#' [post_process()] or [post_process_beamer()] functions
#'
#' @return Nothing
#' @export
render <- function(input = "000-launcher.rmd",
                   output_dir = ".",
                   ...){

  if(!file.exists(input)){
    stop("`render()`: The input file `", input, "` does not exist")
  }

  x <- readLines(input)
  test_line <- grep("test:", x, value = TRUE)
  if(!length(test_line)){
    stop("`render()`: The YAML line containing tag `test:` was not found ",
         "in the input file `", input, "`")
  }
  if(length(test_line) > 1){
    stop("`render()`: The YAML line containing tag `test:` was found ",
         "more than once in the input file `", input, "`")
  }

  test <- gsub(".*test:\\s*(true|false).*", "\\1", test_line)
  if(!test %in% c("true", "false")){
   stop("`render()`: The YAML line containing tag `test:` in the ",
        "input file `", input, "` had a value other than `true` or ",
        "`false`")
  }
  test <- as.logical(test)
  if(test){
    bookdown::render_book(input = input,
                          output_dir = output_dir,
                          envir = globalenv(),
                          ...) |>
      suppressWarnings()
    message("Suppressed warnings for render() of test project")
  }else{
    bookdown::render_book(input = input,
                          output_dir = output_dir,
                          envir = globalenv(),
                          ...)
  }
}
