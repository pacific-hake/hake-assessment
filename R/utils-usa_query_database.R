#' Wrapper to make a query from a sql file and get the data from a database using RODBC
#'
#' @details Calls \code{RODBC} functions. The function first opens an ODBC
#' connection and, using sql, extracts data from the \code{db}.
#'
#' @author Kelli F. Johnson
#'
#' @param queryFilename The filename of the sql query to read in.
#' @param db The name of your database connection.
#' @param uid Your user ID.
#' @param pw The password for the database connection. If omitted, RODBC will
#'   prompt you for a password and will mask it. If entered, it will be visible
#'   and unsecure.
#' @param sp The species scientific name to extract from the database.
#' @param start The start year.
#' @param end   The end year.
#' @param ais A logical, specifying whether or not to convert columns, as in
#'   \code{\link[utils]{read.table}}.
#'
#' @seealso \code{\link[RODBC]{sqlQuery}}
#'
#' @export
#'
usa_query_database <- function(queryFilename,
                    db,
                    uid,
                    pw = "",
                    sp = " ",
                    start = " ",
                    end = " ",
                    asis = FALSE) {
  removeComments <- function(x, comment) {
    lines <- grep(comment, x)
    x[lines] <- unlist(lapply(
      strsplit(x[lines], comment),
      function(xx) {
        xx[1]
      }
    ))
    return(x)
  }

  # make the query as a character string from the file in the sql directory
  qq <- readLines(queryFilename)
  # Remove comment lines
  if (length(grep("REM", qq)) > 0) {
    qq <- removeComments(qq, "REM")
  }
  if (length(grep("--", qq)) > 0) {
    qq <- removeComments(qq, "--")
  }
  # Make appropriate substitutions
  sp <- paste0("'", paste(sp, collapse = "','"), "'")
  qq <- gsub("&sp", sp, qq)
  qq <- gsub("&beginyr", start, qq)
  qq <- gsub("&endyr", end, qq)
  query <- paste(qq, collapse = " ")

  # get data from the database
  stopifnot(require("RODBC", quietly = TRUE))
  database <- RODBC::odbcConnect(dsn = db, uid = uid, pw = pw)
  on.exit(RODBC::odbcClose(database))
  out <- RODBC::sqlQuery(database, query, as.is = asis)

  return(out)
}
