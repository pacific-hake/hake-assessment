#' Add an additional layer of curly braces around the titles found in the
#' supplied BIB file (BiBTeX format). This is necessary for CSL to write the
#' titles in the output document as-is. If it is not done, sentence-case is
#' implemented in CSL and your titles will be messed up.
#'
#' @details
#' Only done once for a project. Added here for reference and to save the code
#'
#' @param bib_fn The input BiBTeX file name
#' @param out_fn The output BiBTeX file name
#'
#' @return nothing
#' @export
fix_title_case_for_csl <- function(
    bib_fn = here("doc/bib/refs.bib"),
    out_fn = here("doc/bib/refs_modified.bib")){

  lines <- readLines(bib_fn)
  inds <- grep("^\\s*title\\s*=.*$", lines)
  strs <- lines[inds]

  x <- map(strs, ~{
    # Add the extra opening curly brace
    k <- gsub("^(\\s*title\\s*=\\s*\\{)(.*)$", "\\1{\\2", .x)
    # Add the extra closing curly brace
    gsub("^(.*\\})(.*)$", "\\1}\\2", k)
  })

  walk2(x, inds, ~{
    lines[.y] <<- .x
  }) |>
    map_chr(~{.x})

  writeLines(lines, out_fn)
  if(file.exists(out_fn)){
    message("File `", out_fn,"` was created")
  }else{
    stop("File `", out_fn,"` was not created")
  }
}
