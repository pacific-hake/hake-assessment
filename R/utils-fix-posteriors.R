#' Remove multiple header/data rows found in posteriors files to leave just
#' one
#'
#' @param fn File name for the posterior file
#'
#' @return Nothing. If the file contains only one header/data row (i.e is
#' of correct format) then nothing happens. If it contains more than one
#' header row, remove the second header and everything else after that in
#' the file and overwrite the file
#' @export
fix_posteriors <- function(fn){

  if(!file.exists(fn)){
    stop("File `", fn, " does not exist")
  }
  posts <- read.table(fn,
                      header = TRUE,
                      fill = TRUE) |>
    as_tibble()

  # Detect a second header in the same file and remove it and everything
  # below it
  if(length(grep("\\D+", posts$Iter))){
    write.table(posts[1:(grep("\\D+", posts$Iter)[1] - 1), ],
                fn,
                quote = FALSE,
                row.names = FALSE)
    message("Posteriors file `", fn, "` was modified. Some rows with non-",
            "numeric `Iter` column values were removed")
  }
  invisible()
}
