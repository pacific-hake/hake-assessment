#' Get the document type for the given RMD file
#'
#' @param fn The RMD filename
#'
#' @return Either "beamer" or "doc". "doc" means the main assessment document
#' @export
get_doc_type <- function(fn){

  x <- readLines(fn)
  ind_output <- grep("output:", x)
  if(!length(ind_output)){
    stop("You must have a line containing `output:` in your index RMD file ",
         "(", bookdown_lst$rmd_fns[1], ")")
  }
  if(length(ind_output) > 1){
    stop("There is more than one line containing `output:` in your index ",
         "RMD file (", bookdown_lst$rmd_fns[1], ")")
  }

  ind_type <- grep("hake_(pdf|beamer):", x)
  if(!length(ind_type)){
    stop("You must have a line containing `output:` in your index RMD file ",
         "(", bookdown_lst$rmd_fns[1], ") followed by a line containing ",
         "the document type (either `hake_pdf:` or `hake_beamer):`")
  }
  if(!length(ind_type)){
    stop("There is more than one line containing the document type ",
         "(either `hake_pdf:` or `hake_beamer)")
  }

  type <- x[ind_type]
  type <- gsub("^\\s*", "", type)
  type <- gsub(":$", "", type)
  type <- gsub("^hake_", "", type)

  if(type == "pdf"){
    type <- "doc"
  }

  type
}
