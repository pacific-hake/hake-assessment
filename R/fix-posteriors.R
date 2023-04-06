#' Remove multiple header/data rows found in posteriors files to leave just
#' one
#'
#' @param fn File name for the posterior file
#'
#' @return If the file contains only one header/data row (i.e is of correct
#' format) then return a 1-row data frame of the contents
#' @export
fix_posteriors <- function(fn){

  if(!file.exists(fn)){
    stop("File `", fn, " does not exist",
         call. = FALSE)
  }
  posts <- read.table(fn,
                      header = TRUE,
                      fill = TRUE)
  if(all(grepl("^[[:digit:]]", posts[, 1]))){
    return(invisible())
  }
  write.table(posts[1:(grep("\\D+", posts[, "Iter"])[1] - 1), ],
              fn,
              quote = FALSE,
              row.names = FALSE)
  message("Posteriors file `", fn, "` was modified. Some rows with non-",
          "numeric `Iter` column values were removed")
  invisible()
}
