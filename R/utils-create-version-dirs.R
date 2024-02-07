#' Create set of directories for a new model version
#'
#' @param year A four-digit integer specifying the assessment year. E.g., for
#'   an assessment that uses data up to 2023 and was due in 2024, the
#'   assessment year is 2024.
#' @param dir_models A file path where you want to create the year directory.
#'   The default is the path that stores all of the model folders on hake-
#'   precision, i.e., `"/srv/hake/models"`.
#' @param version A string that starts with a two-digit integer and is followed
#'   by a dash and then the word "version".
#' @return
#' A vector of directory paths that were created is invisibly returned.
#' @author Kelli F. Johnson
#' @export
#' @examples
#' \dontrun{
#' create_version_dirs(year = 2023, dir_models = getwd())
#' unlink("01-version", recursive = TRUE)
#' }
create_version_dirs <- function(year,
                                dir_models = "/srv/hake/models",
                                version = "01-version") {
  dir_year <- fs::path(dir_models, year, version)
  fs::dir_create(dir_year, recurse = TRUE)

  dirs_needed <- fs::path(
    dir_year,
    c(
      "01-base-models",
      "02-bridging-models",
      "03-sensitivity-models",
      "04-request-models",
      "05-test-models"
    )
  )
  fs::dir_create(path = dirs_needed)

  return(invisible(dirs_needed))
}
