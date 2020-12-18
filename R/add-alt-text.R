#' Add alternative text to figures in a TEX document, overwriting the original file
#'
#' @details Searches the given TEX file for the term `includegraphics` and inserts alternative
#' text for each one from the given `alt_fig_text`
#' @param tex_file The name of the TEX file
#' @param alt_fig_text A character vector of alternative text to insert
#' @param debug Show the matching `includegraphics` lines from the `tex_file` and their
#' line numbers and the `alt_fig_text` data frame for comparison
#'
#' @return Nothing
#' @export
add_alt_text <- function(tex_file = "hake-assessment.tex",
                         alt_fig_text,
                         debug = TRUE){

  # The match for knitr chunk figures looks like \\includegraphics[width=\\maxwidth]{Filename-1}
  # Some chunkns may have more than one plot in them, in those cases only the first plot will
  # have a tooltip added, therefore the -1 at the end of the regular expression
  inc_graphics_pattern <- "(\\\\includegraphics\\[width=\\\\maxwidth\\]\\{(.*?-1)\\})"
  j <- readLines(tex_file)
  g <- grep(inc_graphics_pattern, j)
  grep("^.*-1} *$", j[g])
  if(!length(g)){
    stop("There were no 'includegraphics' terms found in the TEX file",
         call. = FALSE)
  }
  if(debug){
    message("add_alt_text() matched the following lines in the file ", tex_file)
    print(map2(1:length(g), j[g], ~{
      sprintf("line %d: %s", g[.x], .y)
    }) %>% map_chr(~{.x}))
    message("\nadd_alt_text() has the following alt_fig_text descriptions (from knit_hooks): ")
    print(alt_fig_text)
    cat("\n\n")
    if(length(g) != nrow(alt_fig_text)){
      message("The alt_fig_text data frame does not have the same number of entries as ",
              "the number of knitr figures detected. Did you forget to add is.fig=TRUE to a figure chunk?")
      browser()
    }
  }else{
    if(length(g) != nrow(alt_fig_text)){
      stop("The alt_fig_text data frame does not have the same number of entries as ",
           "the number of knitr figures detected. Did you forget to add is.fig=TRUE to a figure chunk? ",
           "To debug, call add_alt_text() with debug=TRUE",
           call. = FALSE)
    }
  }
  modified_lines <- map2(j[g], alt_fig_text$text, ~{
    gsub(inc_graphics_pattern,
         paste0("\\\\pdftooltip{\\1}{", .y, "}"),
         .x)
  }) %>% map_chr(~{.x})
  j[g] <- modified_lines
  writeLines(j, tex_file)
}