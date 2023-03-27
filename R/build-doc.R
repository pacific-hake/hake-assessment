#' Build the Hake assessment document
#'
#' @param tag Logical. If `TRUE`, add tagging commands to the
#' tex file so that a tagged, web-accessible document is built.
#' If `TRUE`, it takes longer for the `lualatex` step so typically this
#' is only use for the final build (or testing along the way)
#'
#' @return Nothing
#' @export
build_doc <- function(tag = FALSE, ...){

  setwd(here::here("doc"))

  bookdown::render_book("000-launcher.rmd",
                        output_dir = here::here("doc"),
                        output_format = "bookdown::pdf_document2",
                        envir = parent.frame(),
                        ...)
  # Get filename from _bookdown file
  if(!file.exists("_bookdown.yml")){
    stop("File `_bookdown.yml` does not exist in the doc directory",
         call. = FALSE)
  }

  # Construct tex filename
  bd <- readLines("_bookdown.yml")
  pat <- '^ *book_filename: +\\"(\\w+)\\"'
  gr <- grep(pat, bd)
  fn <- gsub('^ *book_filename: +\\"(\\w+)\\"', "\\1", bd[gr])
  fn <- paste0(fn, ".tex")
  if(!file.exists(fn)){
    stop("File `", fn, "` does not exist. This filename was built using the ",
         "value for `book_filename:` in the `_bookdown.yml` file with ",
         "`.tex` appended",
         call. = FALSE)
  }
  # Add table of contents, center section headers, modify figure placements
  # and many other things to make the document presentable
  post_process(fn, tag = tag)
  # Run lualatex twice to ensure references are populated correctly
  system(paste0("lualatex -interaction=nonstopmode ", fn),
         intern = TRUE,
         wait = TRUE)
  system(paste0("lualatex -interaction=nonstopmode ", fn),
         intern = TRUE,
         wait = TRUE)
}