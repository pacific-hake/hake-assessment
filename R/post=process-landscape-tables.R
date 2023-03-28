#' Post-process landscape tables in TEX code
#'
#' @param x Tex code, as a vector of lines read in from a TeX file by
#' [readLines()]
#'
#' @return The modified Tex code, as a vector
#' @export
post_process_landscape_tables <- function(x){

  # Add fancy landscape page type to landscape pages to remove the sideways
  # headers and footers, and add a page number at the bottom
  lscape_inds <- grep("\\\\begin\\{landscape\\}", x)
  lst <- post_process_extract_chunks(x, lscape_inds, lscape_inds)
  lst$between <- map(lst$between, \(lscape_line){
    c("\\pagestyle{fancylandscape}", lscape_line)
  })
  # Ensure there is a \\newpage before any landscape tables so that the
  # landscape pagestyle is not projected back to the previous portrait page
  lst$inbetween <- map(lst$inbetween, \(chunk){
    # Strip the trailing empty lines off the end
    k <- chunk[chunk != ""]
    if(tail(k, 1) != "\\\\newpage"){
      chunk <- c(chunk, "\\newpage")
    }
    chunk
  })

  post_process_interlace_chunks(lst)
}
