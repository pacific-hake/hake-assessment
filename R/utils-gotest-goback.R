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

  # Read the bookdown file to determine which files need to be copied
  fns <- list.files()
  if(!"_bookdown.yml" %in% fns){
    stop("You can only call `gotest()` from a directory containing ",
         " `_bookdown.yml` as it is used to determine which type of project ",
         "you are wanting to test. e.g. main document or one of the beamer ",
         "presentations ",
         call. = FALSE)
  }
  # Set global directory name to return back to with `goback()`
  goback_dr <<- curr_dir

  bookdown_lst <- read_bookdown_file("_bookdown.yml")
  # Search the index file (first RMD file) to find out which type of doc this
  # is, beamer or main doc
  type <- get_doc_type(bookdown_lst$rmd_fns[1])

  if(type == "beamer"){
    raw_fns <- c(bookdown_lst$rmd_fns,
                 "clean")

    src_fns <- replace_dotted_paths(raw_fns)
    dest_fns <- file.path("doc", basename(src_fns))

    # Add the beamer images (title picture and logos)
    # Read the image directory from the 000-launcher.rmd file
    x <- readLines("000-launcher.rmd")
    images_src_dir <- grep("images_dir:", x, value = TRUE)
    if(!length(images_src_dir)){
      stop("`images_dir:` not found in 000-launcher.rmd",
           call. = FALSE)
    }
    images_src_dir <- gsub('\\"', "", images_src_dir)
    images_src_dir <- gsub("\\s*images_dir:\\s*", "", images_src_dir)
    images_src_dir <- gsub("\\s*#.*", "", images_src_dir)
    images_src_fns <- list.files(images_src_dir, full.names = TRUE)
    images_basename_fns <- list.files(images_src_dir)
    images_dest_fns <- file.path("doc/images", images_basename_fns)
    images_src_fns <- replace_dotted_paths(images_src_fns)
    src_fns <- c(src_fns, images_src_fns)
    dest_fns <- c(dest_fns, images_dest_fns)

    # Add the 003-load-models.R file to the list, setting the source path
    # the same as the 003-load-models.rmd file's path
    src_003 <- grep("003", raw_fns, value = TRUE)
    src_003_dir <- dirname(src_003)
    src_003_name <- basename(src_003)
    src_003_name <- gsub("rmd", "R", src_003_name)
    src_003_fn <- file.path(src_003_dir, src_003_name)
    src_003_fn <- replace_dotted_paths(src_003_fn)
    dest_003_fn <- file.path("doc", src_003_name)
    src_fns <- c(src_fns, src_003_fn)
    dest_fns <- c(dest_fns, dest_003_fn)

    # Add the main figures (prebuilt figures)
    main_figs_src_dir <- here::here("doc/main-figures")
    main_figs_src_fns <- list.files(main_figs_src_dir, full.names = TRUE)
    main_figs_basename_fns <- list.files(main_figs_src_dir)
    main_figs_dest_fns <- file.path("doc/main-figures", main_figs_basename_fns)
    src_fns <- c(src_fns, main_figs_src_fns)
    dest_fns <- c(dest_fns, main_figs_dest_fns)

    # Fetch the knitr figures directory name for creation later
    x <- readLines("000-launcher.rmd")
    knitr_figures_dir <- grep("knitr_figures_dir:", x, value = TRUE)
    if(!length(knitr_figures_dir)){
      stop("`knitr_figures_dir:` not found in 000-launcher.rmd",
           call. = FALSE)
    }
    knitr_figures_dir <- gsub('\\"',
                              "",
                              knitr_figures_dir)
    knitr_figures_dir <- gsub("\\s*knitr_figures_dir:\\s*",
                              "",
                              knitr_figures_dir)
  }else{
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
      "clean")

    # Add the main figures (prebuilt figures and logos in files)
    main_figs_src_dir <- here::here("doc/main-figures")
    main_figs_basename_fns <- list.files(main_figs_src_dir)
    main_figs_fns <- file.path("main-figures", main_figs_basename_fns)
    raw_fns <- c(raw_fns, main_figs_fns)

    doc_dr <- file.path(here::here("doc"))
    src_fns <- file.path(doc_dr, raw_fns)
    dest_fns <- file.path("doc", raw_fns)
  }

  work_dr <- tempdir()
  setwd(work_dr)
  unlink("*", recursive = TRUE, force = TRUE)
  dir.create("doc")
  dir.create("doc/main-figures")
  if(type == "beamer"){
    # Create knitr-figs directory in case there are no figures built in chunks,
    # the logos and title image will have somewhere to be converted to pdf
    # format
    dir.create(file.path("doc", knitr_figures_dir))
    dir.create("doc/images")
  }
  if(type == "pdf"){
    dir.create("doc/bib")
    dir.create("doc/csl")
  }

  # Needed for `here::here()` to work right
  writeLines("", ".here")

  map2(src_fns, dest_fns, ~{
    file.copy(.x, .y, overwrite = TRUE, copy.mode = TRUE)
  })

  # Needed to set `here:here()` correctly
  i_am("./doc/clean")

  setwd("doc")
  # Create the bookdown configuration file _bookdown.yml
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

  # Create the test file 005-test.rmd
  if(type == "beamer"){
    writeLines(c("# Test Slide", ""), "005-test.rmd")
  }else{
    writeLines("", "005-test.rmd")
  }

  # Edit the launcher to be in test mode. Need to keep leading spaces due to
  # YAML formatting specs
  launcher <- readLines("000-launcher.rmd")
  ind <- grep("test:", launcher)
  if(!length(ind)){
    stop("Could not find `test:` in the `000-launcher.rmd` file., It must ",
         "be in the `hake_beamer:` or `hake_pdf:` section of YAML in that file",
         call. = FALSE)
  }
  if(length(ind) > 1){
    stop("`test:` occurs more than once in the `000-launcher.rmd` file., ",
         "It must only occur in the `hake_beamer:` or `hake_pdf:` section ",
         "of YAML in that file",
         call. = FALSE)
  }
  val <- launcher[ind]
  # Capture leading spaces
  leading_spaces <- gsub("^(\\s*)test.*", "\\1", val)
  launcher[ind] <- paste0(leading_spaces, "test: true")
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