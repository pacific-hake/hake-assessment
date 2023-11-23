#' Create and change to a temporary directory and copy a skeleton
#' version of the document or beamer presentation files for testing
#'
#' @details
#' Extracts the source and destination filenames for copying into the
#' temporary directory with one of the two functions
#' [gotest_doc_get_src_dest_filenames()] and
#' [gotest_beamer_get_src_dest_filenames()]
#' See the *Debugging a figure or table, or any other Rmarkdown code*
#' section of the README.md file for detailed explanation.
#' Use the function [goback()] to return from the testing directory to the
#' directory you were in before calling [gotest()]
#'
#' @param config_fn The name of the bookdown YAML file. The default is
#' `_bookdown.yml`
#'
#' @return Nothing, but the global variable `goback_dr` is set for the
#' function [goback()] to use to return to the original directory
#' @export
gotest <- function(config_fn = "_bookdown.yml",
                   figures_dir = NULL,
                   knitr_figures_dir = NULL){

  figures_dir <- figures_dir %||% "image-files"
  knitr_figures_dir <- knitr_figures_dir %||% "knitr-figs"

  if(!file.exists(config_fn)){
    stop("The `bookdown` config file `", config_fn, "` does not exist")
  }

  curr_dir <- getwd()
  if(!length(grep("hake", curr_dir)) && length(grep("tmp", curr_dir))){
    message("You appear to already be in a temporary directory. You ",
            "must `goback()` before trying to `gotest()` again or just ",
            "continue to test here")
    return(invisible())
  }

  # Read the bookdown file to determine which files need to be copied
  fns <- list.files()
  if(!config_fn %in% fns){
    stop("You can only call `gotest()` from a directory containing a ",
         "`bookdown` config file (`", config_fn, "`) as it is used to ",
         "determine which type of project you are wanting to test. ",
         "e.g. the main assessment document or one of the beamer ",
         "presentations ")
  }
  # Set global directory name to return back to with `goback()`
  goback_dr <<- curr_dir

  # Search the index file (first RMD file in the bookdown config file ) to
  # find out which type of doc this is, beamer or main doc, and to get the
  # name of the index file
  bookdown_lst <- read_bookdown_file(config_fn)
  index_fn <- bookdown_lst$rmd_fns[1]
  if(!file.exists(index_fn)){
    stop("The `bookdown` index file `", index_fn, "` does not exist")
  }
  type <- get_doc_type(index_fn)

  if(type == "doc"){
    fns_lst <- gotest_doc_get_src_dest_filenames()
  }else if(type == "beamer"){
    fns_lst <- gotest_beamer_get_src_dest_filenames(bookdown_lst,
                                                    my_figures_dir = figures_dir)
  }else{
    stop("Document type `type` not implemented yet")
  }

  src_fns <- fns_lst$src_fns
  dest_fns <- fns_lst$dest_fns

  work_dr <- tempdir()
  setwd(work_dr)
  unlink("*", recursive = TRUE, force = TRUE)

  dir.create("doc")
  dir.create(file.path("doc", figures_dir))
  if(type == "beamer"){
    dir.create("doc/images")
  }else if(type == "doc"){
    dir.create("doc/bib")
    dir.create("doc/csl")
  }

  # Needed for `here::here()` to work right
  writeLines("", ".here")

  map2(src_fns, dest_fns, ~{
    file.copy(.x, .y, overwrite = TRUE, copy.mode = TRUE)
  })

  # Needed to set `here:here()` correctly
  i_am(paste0("./doc/", index_fn))

  setwd("doc")
  # Create the bookdown configuration file _bookdown.yml
  bd_lines <- c(
    'book_filename: "test"',
    'rmd_files: ["000-launcher.rmd",',
    '            "001-load-packages.rmd",',
    '            "002-load-globals.rmd",',
    '            "003-load-models.rmd",',
    '            "004-load-project-variables.rmd",',
    '            "005-test.rmd",',
    '            "999-blank.rmd"]',
    '',
    'delete_merged_file: false')
  writeLines(bd_lines, config_fn)

  if(type == "doc"){
    writeLines(c("# Test Section", ""), "005-test.rmd")
  }else{
    writeLines(c("# Test Slide", ""), "005-test.rmd")
  }

  # Fetch the knitr figures directory name and create the directory
  # in the temporary doc directory. This is required in case there are
  # no figures built in the testing project but there is a pre-made image
  # included as an EPS file. `knitr` converts this to a PNG and attempts to
  # put it in the knitr figures directory, but fails with error if that
  # directory does not exist. This is an edge case
  launcher <- readLines(index_fn)

  knitr_figures_dir <- grep("knitr_figures_dir:", launcher, value = TRUE)
  if(!length(knitr_figures_dir)){
    stop("`knitr_figures_dir:` not found in file ",
         "`", index_fn, "`")
  }
  knitr_figures_dir <- gsub('\\"',
                            "",
                            knitr_figures_dir)
  knitr_figures_dir <- gsub("\\s*knitr_figures_dir:\\s*",
                            "",
                            knitr_figures_dir)
  # Already in "doc" at this point so this will be created in "doc" dir
  dir.create(here::here("doc", knitr_figures_dir))

  # Edit the launcher to be in test mode. Need to keep leading spaces due to
  # YAML formatting specs
  ind <- grep("test:", launcher)
  if(!length(ind)){
    stop("Could not find `test:` in the `000-launcher.rmd` file., It must ",
         "be in the `hake_beamer:` or `hake_pdf:` section of YAML in ",
         "that file")
  }
  if(length(ind) > 1){
    stop("`test:` occurs more than once in the `000-launcher.rmd` file., ",
         "It must only occur in the `hake_beamer:` or `hake_pdf:` section ",
         "of YAML in that file")
  }
  val <- launcher[ind]
  # Capture leading spaces
  leading_spaces <- gsub("^(\\s*)test.*", "\\1", val)
  launcher[ind] <- paste0(leading_spaces, "test: true")
  writeLines(launcher, index_fn)

  message("\nAll variables and models loaded, in a temporary directory.",
          "\n\nTo build the document, run render().\nWhen finished, ",
          "run goback() to go back to the directory you came from.")

  message("Now in directory ", getwd())

  invisible()
}
