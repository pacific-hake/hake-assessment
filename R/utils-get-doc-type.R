#' Get the document type for the given RMD file
#'
#' @param fn The RMD filename
#'
#' @return Either "beamer" or "pdf". "pdf" means the main assessment document
#' @export
get_doc_type <- function(fn){

  x <- readLines(fn)
  ind_output <- grep("output:", x)
  if(!length(ind_output)){
    stop("You must have a line containing `output:` in your index RMD file ",
         "(", bookdown_lst$rmd_fns[1], ")",
         call. = FALSE)
  }
  if(length(ind_output) > 1){
    stop("There is more than one line containing `output:` in your index ",
         "RMD file (", bookdown_lst$rmd_fns[1], ")",
         call. = FALSE)
  }

  ind_type <- grep("hake_(pdf|beamer):", x)
  if(!length(ind_type)){
    stop("You must have a line containing `output:` in your index RMD file ",
         "(", bookdown_lst$rmd_fns[1], ") followed by a line containing ",
         "either `hake_pdf:` or `hake_beamer:`",
         call. = FALSE)
  }
  if(!length(ind_type)){
    stop("There is more than one line containing either `hake_pdf:` or ",
         "`hake_beamer:`",
         call. = FALSE)
  }

  type <- x[ind_type]
  type <- gsub("^\\s*", "", type)
  type <- gsub(":$", "", type)
  type <- gsub("hake::", "", type)
  type <- gsub("^hake_", "", type)

  type
}