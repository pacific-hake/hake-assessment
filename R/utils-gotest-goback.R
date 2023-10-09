#' Create and change to a temporary directory and copy a skeleton
#' version of the document files for testing
#'
#' @details
#' See the *Debugging a figure or table, or any other Rmarkdown code*
#' section of the README.md file for detailed explanation.
#' See the complement function [goback()].
#'
#' @param copy_tmpdir Logical. If `TRUE`, copy the temporary path that
#' you were just placed in to the OS clipboard
#'
#' @return Nothing
gotest <- function(copy_tmpdir = FALSE){

  curr_dir <- getwd()
  if(!length(grep("hake", curr_dir)) && length(grep("tmp", curr_dir))){
    message("You appear to already be in a temporary directory. You ",
            "must `goback()` before trying to `gotest()` again or just ",
            "continue to test here")
    return(invisible())
  }

  # Set global directory name to return back to with `goback()`
  goback_dr <<- curr_dir

  raw_fns <- c(
    "000-launcher.rmd",
    "001-load-packages.rmd",
    "002-load-globals.rmd",
    "003-load-models.rmd",
    "003-load-models.R",
    "004-load-project-variables.rmd",
    "101-appendix-base-mcmc-diagnostics.rmd",
    "caption-adjustments.csv",
    "forecast-descriptions.csv",
    "object-placement.csv",
    "preamble.tex",
    "bib/refs.bib",
    "csl/csas.csl",
    "main-figures/age1hake_03-21_sA_squareroot_bin_narrow-panel_grayblue.png",
    "main-figures/fishCatchRatesUS.png",
    "main-figures/hake-picture.eps",
    "main-figures/hake-picture.png",
    "main-figures/hake_survey_1995-21_NASCTimeSeries_BiomassAtAgeHistograms_grayblue.png",
    "clean")

  doc_dr <- file.path(here::here("doc"))

  src_fns <- file.path(doc_dr, raw_fns)

  work_dr <- tempdir()
  setwd(work_dr)
  unlink("*", recursive = TRUE, force = TRUE)
  dir.create("doc")
  dir.create("doc/bib")
  dir.create("doc/csl")
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
    '            "004-load-project-variables.rmd",',
    '            "005-test.rmd"]',
    '',
    'delete_merged_file: false')
  writeLines(bd_lines, "_bookdown.yml")
  writeLines("", "005-test.rmd")

  # Edit the launcher to be in test mode
  launcher <- readLines("000-launcher.rmd")
  ind <- grep("test:", launcher)
  launcher[ind] <- "test: true"
  writeLines(launcher, "000-launcher.rmd")

  message("\nAll variables and models loaded, in a temporary directory.",
          "\n\nTo build the document, run render()\nWhen finished, ",
          "run goback() to go back to the directory you came from.")

  if(copy_tmpdir){
    if(dirclip()){
      message("The temporary directory path has been copied to ",
              "the clipboard for easy pasting into the OS terminal when ",
              "you run lualatex... ")
    }else{
      message("The temporary directory path could not be copied to the ",
              "clipboard")
    }
  }
  message("Now in directory ", getwd())

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
#' Assumes you're going back to a directory in the hake repository and that
#' the file `.here` exists in the hake repository root directory.
#'
#' @return Nothing
goback <- function(){

  curr_dir <- getwd()
  if(length(grep("hake", curr_dir)) && !length(grep("tmp", curr_dir))){
    message("You appear to already be in the hake repository. You ",
            "must `gotest()` before trying to `goback()`")
    return(invisible())
  }

  if(!exists("goback_dr")){
    stop("The variable `goback_dr` does not exist. You need to set ",
         "this to the directory name you want to go back to, then ",
         "run goback() again. In future, make sure you enter a temporary ",
         "directory for testing using the `hake::gotest()` function, which ",
         "automatically sets `goback_dr` for you",
         call. = FALSE)
  }

  setwd(goback_dr)
  #set_here(".")
  i_am(".here")
  message("Now in directory ", getwd())
}