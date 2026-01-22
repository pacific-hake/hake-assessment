#' Create the lists of source and destination filenames needed for copying
#' for the main assessment document testing
#'
#' @details
#' Meant to be called by the wrapper function [gotest()]
#'
#' @return Nothing
gotest_doc_get_src_dest_filenames <- function(bookdown_lst = NULL,
                                              figures_dir = NULL){

  figures_dir <- figures_dir %||% "image-files"

  # Read in the loaded file to check whether csl, bib, and image-files are
  # relative or not
  launcher_fn <- bookdown_lst$rmd_fns[1]
  if(!file.exists(launcher_fn)){
    stop("From gotest_doc_get_src_dest_filenames(): File `", launcher_fn, "` does not exist")
  }
  launcher_lines <- readLines(here(doc_path, launcher_fn))
  # csl
  csl_inds <- grep("csl:", launcher_lines)
  if(length(csl_inds)){
    if(length(csl_inds) > 1){
      stop("The text `csl:` was found more than one in the launcher file `", launcher_fn, "`")
    }
  }else{
    stop("The text `csl:` was not found in the launcher file `", launcher_fn, "`")
  }

  csl_line <- launcher_lines[csl_inds]
  csl_fn <- gsub("csl: *", "", csl_line)
  csl_has_prespaces <- grep("^([[:space:]]+)csl:.*$", csl_fn)
  if(length(csl_has_prespaces)){
    csl_prespaces <- gsub("^([[:space:]]+)csl:.*$", csl_fn)
  }else{
    csl_prespaces <- ""
  }
  csl_src_fn <- gsub("\\.csl.*", ".csl", csl_fn)
  csl_dest_fn <- gsub(".*?([a-zA-Z0-9]+\\.csl).*", "\\1", csl_line)
  csl_dest_fn <- file.path("csl", csl_dest_fn)
  launcher_lines[csl_inds] <- paste0(csl_prespaces, "csl: ", csl_dest_fn)

  # # bib
  bib_inds <- grep("bibliography:", launcher_lines)
  if(length(bib_inds)){
    if(length(bib_inds) > 1){
      stop("The text `bibliography:` was found more than one in the launcher file `", launcher_fn, "`")
    }
  }else{
    stop("The text `bibliogrpahy:` was not found in the launcher file `", launcher_fn, "`")
  }

  bib_line <- launcher_lines[bib_inds]
  bib_fn <- gsub("bibliography: *", "", bib_line)
  bib_has_prespaces <- grep("^([[:space:]]+)bibliography:.*$", bib_fn)
  if(length(bib_has_prespaces)){
    bib_prespaces <- gsub("^([[:space:]]+)bibliography:.*$", bib_fn)
  }else{
    bib_prespaces <- ""
  }
  bib_src_fn <- gsub("\\.bib.*", ".bib", bib_fn)
  bib_dest_fn <- gsub(".*?([a-zA-Z0-9]+\\.bib).*", "\\1", bib_fn)
  bib_dest_fn <- file.path("bib", bib_dest_fn)
  launcher_lines[bib_inds] <- paste0(bib_prespaces, "bibliography: ", bib_dest_fn)

  # figures dir
  fd_ind <- grep("^[[:space:]]*figures_dir:", launcher_lines)
  if(length(fd_ind)){
    if(length(fd_ind) > 1){
      stop("The text `figures_dir:` was found more than one in the launcher file `", launcher_fn, "`")
    }
  }else{
    stop("The text `figures_dir:` was not found in the launcher file `", launcher_fn, "`")
  }
  fd_line <- launcher_lines[fd_ind]
  fd_fn <- gsub("figures_dir: *", "", fd_line)
  fd_has_prespaces <- grep("^([[:space:]]+)figures_dir:.*$", fd_line)
  if(length(fd_has_prespaces)){
    fd_prespaces <- gsub("^([[:space:]]+)figures_dir:.*$", "\\1", fd_line)
  }else{
    fd_prespaces <- ""
  }
  fd_fn <- gsub("\"", "", fd_fn)
  fd_src_fn <- gsub(" *", "", fd_fn)
  fd_dest_fn <- gsub(".*?([_a-zA-Z0-9\\-]+)$", "\\1", fd_src_fn)
  launcher_lines[fd_ind] <- paste0(fd_prespaces, "figures_dir: ", fd_dest_fn)

  fd_inds <- grep(figures_dir, launcher_lines)
  # Remove figures_dir: line because we already dealt with it above
  figures_dir_ind <- grep("figures_dir:", launcher_lines[fd_inds])
  if(length(figures_dir_ind)){
    if(length(figures_dir_ind) > 1){
      stop("Matched more than one `figures_dir:` in the launcher file ",
           launcher_fn)
    }
    fd_inds <- fd_inds[-figures_dir_ind]
  }
  if(length(fd_inds)){
    j <- map_chr(launcher_lines[fd_inds], ~{
      gsub(fd_src_fn, fd_dest_fn, .x)
    })
    launcher_lines[fd_inds] <- j
  }
  # Write temp launcher file
  temp_launcher_fn <- paste0("gotest-", launcher_fn)
  writeLines(launcher_lines, temp_launcher_fn)

  src_fns <- c(file.path(doc_path, temp_launcher_fn),
               file.path(doc_path, "001-load-packages.rmd"),
               file.path(doc_path, "002-load-globals.rmd"),
               file.path(doc_path, "003-load-models.rmd"),
               file.path(doc_path, "003-load-models.R"),
               file.path(doc_path, "004-load-project-variables.rmd"),
               file.path(doc_path, "999-blank.rmd"),
               file.path(doc_path, caption_adjustments_fn),
               file.path(doc_path, forecast_descriptions_fn),
               file.path(doc_path, object_placement_fn),
               file.path(doc_path, "preamble.tex"),
               file.path(doc_path, bib_src_fn),
               file.path(doc_path, csl_src_fn))

  dest_fns <- c(file.path(doc_path, "000-launcher.rmd"),
                file.path(doc_path, "001-load-packages.rmd"),
                file.path(doc_path, "002-load-globals.rmd"),
                file.path(doc_path, "003-load-models.rmd"),
                file.path(doc_path, "003-load-models.R"),
                file.path(doc_path, "004-load-project-variables.rmd"),
                file.path(doc_path, "999-blank.rmd"),
                file.path(doc_path, caption_adjustments_fn),
                file.path(doc_path, forecast_descriptions_fn),
                file.path(doc_path, object_placement_fn),
                file.path(doc_path, "preamble.tex"),
                file.path(doc_path, "bib/refs.bib"),
                file.path(doc_path, "csl/csas.csl"))

  fns_exists <- file.exists(file.path("../", src_fns))
  if(!all(fns_exists)){
    stop("One or more files that are required to be copied for testing ",
         "do not exist in the current directory. The file(s) that do ",
         "not exists are:\n\n",
         paste(src_fns[!fns_exists], collapse = "\n"),
         "\n\nCheck the `gotest_doc()` function")
  }

  # Add the main figures (prebuilt figures and logos in files)
  main_figs_src_dir <- fd_src_fn
  main_figs_basename_fns <- list.files(main_figs_src_dir)
  main_figs_fns <- file.path(fd_src_fn, main_figs_basename_fns)
  main_figs_dest_fns <- file.path(doc_path, fd_dest_fn, main_figs_basename_fns)
  src_fns <- c(src_fns, main_figs_fns)
  dest_fns <- c(dest_fns, main_figs_dest_fns)
  # add full path for those entries not relative
  src_fns <- map_chr(src_fns, ~{
    out <- .x
    is_relative <- grep("^\\.", .x)
    if(!length(is_relative)){
      out <- here(.x)
    }
    out
  })


  list(src_fns = src_fns,
       dest_fns = dest_fns)
}