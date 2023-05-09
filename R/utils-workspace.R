#' Create and change to a temporary directory where the loading files have
#' been copied to
#'
#' @details
#' Run `bookdown::render_book("000-launcher.rmd", output_dir = ".")` once
#' in the temporary directory to load everything necessary. Once that has
#' completed you can run any table or figure code in the document for
#' debugging purposes. You can also see what a plot or table will look like
#' in the doc by adding it to the blank "006-test.rmd" file and then running
#' bookdown as above.
#'
#' @return Nothing
workspace <- function(){

  cdr <<- getwd()

  doc_dr <- here("doc")
  raw_fns <- c("000-launcher.rmd",
               "001-load-packages.rmd",
               "002-load-globals.rmd",
               "003-load-data-tables.rmd",
               "004-load-models.rmd",
               "004-load-models.R",
               "005-load-knitr-variables.rmd",
               "preamble.tex",
               "clean")

  src_fns <- file.path(doc_dr, raw_fns)

  work_dr <- tempdir()
  setwd(work_dr)
  dir.create("doc")

  # Needed for `here::here()` to rowk right
  writeLines("", ".here")

  dest_fns <- file.path("doc", raw_fns)
  walk2(src_fns, dest_fns, ~{
    file.copy(.x, .y, overwrite = FALSE)
  })
  # Needed to set `here:here()` correctly
  i_am("./doc/clean")

  setwd("doc")
  # Make the bookdown YAML file
  bd_lines <- c(
    'book_filename: "hake"',
    'rmd_files: ["000-launcher.rmd",',
    '            "001-load-packages.rmd",',
    '            "002-load-globals.rmd",',
    '            "003-load-data-tables.rmd",',
    '            "004-load-models.rmd",',
    '            "005-load-knitr-variables.rmd",',
    '            "006-test.rmd"]',
    '',
    'delete_merged_file: false')
  writeLines(bd_lines, "_bookdown.yml")
  writeLines("", "006-test.rmd")

  message("\nAll variables and models loaded, in a temporary directory.",
          "\n\nRun bookdown::render_book('000-launcher.rmd', output_dir = '.')",
          "\nWhen finished, run setwd(cdr) to go back to the directory ",
          "you came from.")

  invisible()
}