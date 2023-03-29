#' Post-process the TEX file output by Bookdown
#'
#' @details
#' Add centering for section names,
#'
#' @param fn The TEX filename
#' @param tag Logical. If `TRUE`, include lines for LaTeX `tagpdf`
#' package to make a more web-accessible document. This will make the compile
#' time much longer and is typically used when done the document and doing the
#' final compile for government web distribution
#' @param figs_dir The name of the directory in which the knitr-built
#' figures reside. Used for matching figure import locations inside the tex
#' file
#' @param ... Arguments passed to [post_process_table_of_contents()]
#'
#' @return Nothing, overwrite the file `fn` with the modified TEX
#' @export
post_process <- function(fn,
                         tag = FALSE,
                         figs_dir = ifelse(exists("knitr_figs_dir"),
                                           knitr_figs_dir,
                                           NULL),
                         ...){

  if(is.null(figs_dir)){
    stop("`figs_dir` is `NULL`. You must provide the directory name ",
         "in which the knitr-built figures reside, relative to the doc ",
         "directory",
         call. = FALSE)
  }

  fn_base <- file_path_sans_ext(fn)
  fn_ext <- file_ext(fn)
  fn_bck <- paste0(fn_base, "-bck.", fn_ext)

  if(!file.exists(fn)){
    message("The TeX file `", fn, "` does not exist. Trying to copy it ",
            "from backup...\n")
    Sys.sleep(2)
    if(!file.exists(fn_bck)){
      stop("The backup file `", fn_bck, "` does not exist. Re-run the ",
           "bookdown command to create `", fn, "`",
           call. = FALSE)
    }
    file.copy(fn_bck, fn)
    if(file.exists(fn)){
      message("File copied successfully. Running post-processing on ",
              "`", fn, "`\n")
      Sys.sleep(2)
    }
  }

  x <- readLines(fn)

  modification_pre_text <- "% This file was modified by hake::post_process()"
  modification_text <- c(modification_pre_text,
                         paste0("% at ", Sys.time()),
                         "",
                         "")
  hasbeen_modified <- grep(modification_pre_text, x)
  if(length(hasbeen_modified)){
    if(file.exists(fn_bck)){
      # Copy from the backup, erasing the previous post processing changes
      file.copy(fn_bck, fn, overwrite = TRUE)
      x <- readLines(fn)
    }else{
      stop("The file `", fn, "` has already been modified by the ",
           "hake::post_process() function, and the backup file `", fn_bck,
           "` was not found. Re-run the bookdown command to create `", fn, "`",
           call. = FALSE)
    }
  }else{
    file.copy(fn, fn_bck, overwrite = TRUE)
  }

  if(tag){
    x <- c(
      "\\RequirePackage{pdfmanagement-testphase}",
      paste0("\\DocumentMetadata{testphase=phase-II, uncompress, ",
             "pdfstandard=A-2U, lang=en-US}"),
      x)
  }

  dc_ind <- grep("documentclass", x)
  if(!length(dc_ind)){
    stop("\\documentclass not found, document is not valid LaTeX and ",
         "cannot be built",
         call. = FALSE)
  }

  # Remove page number from title page
  title_ind <- grep("\\\\maketitle", x)
  if(!length(title_ind)){
    stop("`\\maketitle` not found. You must be using the `\\maketitle` ",
         "method to produce the title page for this document",
         call. = FALSE)
  }
  pre <- x[1:(title_ind)]
  post <- x[(title_ind + 1):length(x)]
  x <- c(pre, "\\thispagestyle{empty}", post)

  # Make sections/subsections uppercase/centered ----
  x <- post_process_section_headers(x)

  # Insert the Table of contents ----
  x <- post_process_table_of_contents(x, ...)

  # Executive summary catch plot placement----
  x <- post_process_set_figure_placement(x,
                                         figs_dir,
                                         knitr_label = "es-catches-1",
                                         place = "!b")

  # Add more latex to longtables ----
  x <- post_process_longtables(x, ...)

  x <- post_process_landscape_tables(x)

 # Mark file with modification text ----
  x <- c(modification_text, x)

  writeLines(x, fn)
  if(!file.exists(fn)){
    stop("The post processing step failed to create the file `", fn, "`.",
         call. = FALSE)
  }
  info <- file.info(fn)
  tm <- as.numeric(info$mtime)
  if(Sys.time() - tm > 1){
    stop("The post processing step failed to overwrite the file `", fn, "`. ",
         "The file's timestamp is not recent enough (< 3 seconds ago)",
         call. = FALSE)
  }
  message("Post-processing successfuly completed.\n")
}