#' Create the lists of source and destination filenames needed for copying
#' for the beamer presentations testing
#'
#' @details
#' Meant to be called by the wrapper function [gotest()]
#'
#' @return Nothing
gotest_beamer_get_src_dest_filenames <- function(bookdown_lst){

  raw_fns <- bookdown_lst$rmd_fns
  # Add 003-load-models.R at the right depth if dotted
  r_fn <- grep("003-load-models.rmd", raw_fns, value = TRUE)
  if(!length(r_fn)){
    stop("File `", r_fn, "` not found in the bookdown config file. See ",
         "gotest_beamer() function",
         call. = FALSE)
  }

  dotted_path_to_docs <- gsub("((../)*doc)/*.*", "\\1", r_fn)
  raw_fns <- c(raw_fns, file.path(dotted_path_to_docs, "003-load-models.R"))

  src_fns <- replace_dotted_paths(raw_fns)
  # Only copy files that match these ones
  pat <- paste0("000-launcher.rmd|",
                "001-load-packages.rmd|",
                "002-load-globals.rmd|",
                "003-load-models.rmd|",
                "003-load-models.R|",
                "004-load-project-variables.rmd|",
                "999-blank.rmd")

  src_fns <- grep(pat, src_fns, value = TRUE)
  if(!length(src_fns)){
    stop("No files in the bookdown config file matched the files to be ",
         "copied. See the `gotest_beamer()` function",
    call. = FALSE)
  }
  dest_fns <- file.path("doc", basename(src_fns))

  # Add the beamer images (title picture and logos)
  # Read the image directory from the 000-launcher.rmd file
  index_fn <- bookdown_lst$rmd_fns[1]
  x <- readLines(index_fn)
  images_src_dir <- grep("images_dir:", x, value = TRUE)
  if(!length(images_src_dir)){
    stop("`images_dir:` not found in 000-launcher.rmd",
         call. = FALSE)
  }
  images_src_dir <- gsub('\\"', "", images_src_dir)
  images_src_dir <- gsub("\\s*images_dir:\\s*", "", images_src_dir)
  images_src_dir <- gsub("\\s*#.*", "", images_src_dir)
  images_src_fns <- list.files(images_src_dir, full.names = TRUE)
  if(!length(images_src_fns)){
    stop("Could not get a list of the images needed for the beamer ",
         "presentations (location of logos). See gotest_beamer() function",
         call. = FALSE)
  }
  images_src_fns <- replace_dotted_paths(images_src_fns)
  images_dest_fns <- file.path("doc/images", basename(images_src_fns))
  src_fns <- c(src_fns, images_src_fns)
  dest_fns <- c(dest_fns, images_dest_fns)

  # Add the main figures (prebuilt figures)
  main_figs_src_dir <- here::here("doc/main-figures")
  main_figs_src_fns <- list.files(main_figs_src_dir, full.names = TRUE)
  main_figs_dest_fns <- file.path("doc/main-figures", basename(main_figs_src_fns))
  src_fns <- c(src_fns, main_figs_src_fns)
  dest_fns <- c(dest_fns, main_figs_dest_fns)

  list(src_fns = src_fns,
       dest_fns = dest_fns)
}