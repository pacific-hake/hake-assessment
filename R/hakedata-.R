# TODO: replace this with here::here()
#' Find the working directory for `hake-assessment`
#'
#' Find the directory called `hake-assessment/data-tables`, which should be a
#' result of cloning \url{www.github.com/pacific-hake/hake-assessment}. The
#' location of the directory is found based on a set of rules for a given system
#' and user name of the computer you are on. This `data` directory stores
#' non-confidential data used in the assessment of Pacific Hake and is integral
#' in building the bridging files to go from one year of data to the next. If
#' the combination of known system and user names are not found then it will
#' default to using your current working directory.
#'
#' @return
#' A string specifying the full file path for the `hake-assessment/data`
#' directory. The default is your current working directory.
#' @export
#' @author Kelli F. Johnson
#' @examples
#' hakedata_wd()
#'
hakedata_wd <- function() {
  user <- Sys.info()["user"]
  terminal_directory <- "data-tables"
  if (Sys.info()["sysname"] == "Linux") {
    wd <- fs::path(
      "/home", user,
      "github", "pacific-hake", "hake-assessment", terminal_directory
    )
  }
  if (Sys.info()["sysname"] == "Windows") {
    wd <- switch(user,
      "Kelli.Johnson" = {
        fs::path(
          "d:", "github", "pacific-hake",
          "hake-assessment", terminal_directory
        )
      },
      "Aaron.Berger" = {
        fs::path(
          "C:", "Users", "Aaron.Berger", "Documents",
          "GitHub", "hake-assessment", terminal_directory
        )
      },
      "Chantel.Wetzel" = {
        fs::path(
          "C:", "Users", "Chantel.Wetzel", "Documents",
          "github", "pacific-hake", "hake-assessment", terminal_directory
        )
      },
      {
        cli::cli_bullets(c(
          "x" = "Username not found",
          "i" = "Setting the directory to {getwd()}"
        ))
        getwd()
      }
    )
  }
  stopifnot(fs::dir_exists(wd))
  stopifnot(basename(wd) == "data-tables")
  return(wd)
}
