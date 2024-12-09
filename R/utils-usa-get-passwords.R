#' Find username and passwords for databases
#'
#' Find the username and passwords specific given the username of the computer
#' and stored or entered passwords for accessing databases that store
#' confidential information about landings of Pacific Hake.
#'
#' @inheritParams usa_pull_data
#'
#' @return
#' A list with two entries, `usernames` and `passwords`. Each entry contain a
#' named vector with one element for each database that is used, i.e., NORPAC
#' and PacFIN. The list is invisibly returned to ensure that the passwords are
#' not printed to the screen. Thus, the function call should be assigned to an
#' object.
#' @export
#' @author Kelli F. Johnson
#' @examples
#' \dontrun{
#' # Prompted for passwords for each database
#' test <- usa_get_passwords()
#' # Prompted for passwords for each database because password_file is not found
#' test <- usa_get_passwords(password_file = "doesnotwork.txt")
#' # On Kelli Johnson's machine, the following will work
#' test <- usa_get_passwords(password_file = "password.txt")
#' }
usa_get_passwords <- function(password_file) {
  user <- Sys.info()["user"]
  database <- c("NORPAC", "PacFIN")
  name <- switch(user,
    "Kelli.Johnson" = {
      c("NORPAC" = "JOHNSONK", "PacFIN" = "kjohnson")[database]
    },
    "Aaron.Berger" = {
      c("NORPAC" = "BERGERA", "PacFIN" = "aberger")[database]
    },
    "Ian.Taylor" = {
      c("NORPAC" = "TAYLORI", "PacFIN" = "itaylor")[database]
    },
    "Chantel.Wetzel" = {
      c("NORPAC" = "WETZELC", "PacFIN" = "cwetzel")[database]
    }
  )
  stopifnot(!is.null(name))
  stopifnot(all(names(name) %in% database))

  if (missing(password_file) || !file.exists(password_file)) {
    password_file <- NULL
  }

  if (is.null(password_file)) {
    passwords <- rep(NA, length(database))
    for (ii in seq_along(database)) {
      passwords[ii] <- readline(
        prompt = glue::glue("
          Enter password for {database[ii]} database without quotes and \\
          hit Enter.

          ")
      )
    }
  } else {
    passwords <- readLines(password_file, warn = FALSE)
    stopifnot(length(database) == length(passwords))
  }

  names(passwords) <- database
  invisible(list("username" = name, "password" = passwords))
}
