#' Load all SS3 report files from the directory provided and return
#' them as a list of data frames
#'
#' @param dr The directory to load from
#' @param file_pat A regular expression used by [grrep()] to find the files
#' in the directory
#' @param progress_n Report every time this many files are read in
#' @param verbose Show all progress messages
#' @param first Load this many of the files. If a non-positive number, load
#' them all. Used for debugging purposes to cut down the size of the
#' lists used
#'
#' @return A list of data frames, 1 for each report file
#' @export
load_extra_mcmc_repfiles <- function(dr,
                                     file_pat = "Report_mce_[0-9]+\\.sso$",
                                     progress_n = 500,
                                     verbose = TRUE,
                                     first = 0){

  if(!dir.exists(dr)){
    stop("The directory `", dr, "` does not exist")
  }
  if(substring(file_pat, 1, 1) != "/"){
    # If this isn't done, any paths that match the expression will remain
    # in the list even though they aren't a report file
    file_pat <- paste0("/", file_pat)
  }
  # Get the number of Report.sso files in the directory
  dir_list <- dir(dr, full.names = TRUE)
  repfile_lst <- grep(file_pat, dir_list, value = TRUE)

  if(first > 0 && first < length(repfile_lst)){
    repfile_lst <- repfile_lst[seq_len(first)]
    warning("Using only the first ", first, " posteriors!!")
  }

  if(!length(repfile_lst)){
    if(verbose){
      warning("There are no files matching the input regular expression in ",
              "the `", dr, "` directory.")
    }
    return(NA)
  }

  if(verbose){
    message("Loading ", length(repfile_lst), " files from:\n`",
            dr, "`\nProgress:")
  }

  imap(repfile_lst, \(fn, file_n){
    if(file_n %% progress_n == 0 && verbose){
      message(file_n, " files loaded")
    }
    readLines(fn)
  })

}