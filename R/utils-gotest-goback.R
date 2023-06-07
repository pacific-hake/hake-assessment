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
#' @param doc_dr The directory that holds all the RMD files for building the
#' document. Note: due to `tempdir()` issues, we cannot use `here()` for this,
#' it must be a full path
#'
#' @return Nothing
gotest <- function(repo_dr = "~/github/pacific-hake/hake"){

  # Set global dir name to return back to with `goback()`
  cdr <<- repo_dr

  raw_fns <- c("000-launcher.rmd",
               "001-load-packages.rmd",
               "002-load-globals.rmd",
               "003-load-models.rmd",
               "003-load-models.R",
               "004-load-knitr-variables.rmd",
               "preamble.tex",
               "bib/refs.bib",
               "main-figures/age1hake_03-21_sA_squareroot_bin_narrow-panel_grayblue.png",
               "main-figures/fishCatchRatesUS.png",
               "main-figures/hake-picture.eps",
               "main-figures/hake-picture.png",
               "main-figures/hake_survey_1995-21_NASCTimeSeries_BiomassAtAgeHistograms_grayblue.png",
               "clean")

  doc_dr <- file.path(repo_dr, "doc")

  src_fns <- file.path(doc_dr, raw_fns)

  work_dr <- tempdir()
  setwd(work_dr)
  unlink("*", recursive = TRUE, force = TRUE)
  dir.create("doc")
  dir.create("doc/bib")
  dir.create("doc/main-figures")

  # Needed for `here::here()` to work right
  writeLines("", ".here")

  dest_fns <- file.path("doc", raw_fns)
  map2(src_fns, dest_fns, ~{
    file.copy(.x, .y, overwrite = TRUE, copy.mode = TRUE)
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
    '            "003-load-models.rmd",',
    '            "004-load-knitr-variables.rmd",',
    '            "005-test.rmd"]',
    '',
    'delete_merged_file: false')
  writeLines(bd_lines, "_bookdown.yml")
  writeLines("", "005-test.rmd")

  message("\nAll variables and models loaded, in a temporary directory.",
          "\n\nRun bookdown::render_book('000-launcher.rmd', ",
          "output_dir = '.')\nWhen finished, run goback() to go back to ",
          "the directory you came from.")

  invisible()
}

#' Navigate back to the directory you were in before calling [gotest()]
#'
#' @details
#' After running [gotest()], you will be in a temporary directory,
#' and you'll be able to build a test document with arbitrary
#' knitr code chunks. Once you're done testing and checking your output,
#' run this function to return to the real document directory, and
#' to reset the [here::here()] command so that it references the real
#' project again.
#'
#' Assumes you're going back to the hake `doc` directory and that the file
#' `.here` exists in the directory above that (the hake repository root
#' directory)
#'
#' @return Nothing
goback <- function(){

  if(!exists("cdr")){
    stop("The variable `cdr` does not exist. You need to set this to the ",
         "directory name you want to go back to, then re-run this ",
         "function. In future, make sure you enter a temporary directory ",
         "for testing using the `hake::gotest()` function, which ",
         "automatically sets `cdr` for you",
         call. = FALSE)
  }

  setwd(cdr)
  #set_here(".")
  i_am(".here")
  setwd("doc")
}