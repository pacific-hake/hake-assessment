#' Replace dotted paths in the vector with their correct full paths with
#' respect to the current directory and prepend the regular (non-dotted
#' paths) with the current directory path.
#'
#' @param x A vector of filenames, with or without leading dotted paths
#' (`../../`) or (`../../../../../`) or to any level
#'
#' @return The same vector of filenames with the dotted paths replaced by
#' full paths and the non-dotted paths prepended by the current directory
#' path
#' @export
replace_dotted_paths <- function(x){

  # Find paths which have dotted reference (../../ etc)
  dotted_inds <- grep("\\.\\.\\/+", x)
  dotted <- x[dotted_inds]
  dots_removed_fns <- gsub("\\.{1}\\.{1}\\/{1}", "", dotted)
  dots_only <- gregexpr("\\.{1}\\.{1}\\/", dotted)
  # Split up the current directory so we can build the full path directories
  # for the dotted directories
  curr_dir <- getwd()
  curr_dir_vec <- str_split(curr_dir, pattern = "\\/")[[1]]
  base_dirs <- map_chr(dots_only, ~{
    len <- length(as.vector(.x))
    paste0(head(curr_dir_vec, -len), collapse = "/")
  })
  dotted <- file.path(base_dirs, dots_removed_fns)

  non_dotted_inds <- seq_along(x)[-dotted_inds]
  non_dotted <- x[non_dotted_inds]
  non_dotted <- file.path(getwd(), non_dotted)

  x[dotted_inds] <- dotted
  x[non_dotted_inds] <- non_dotted

  x
}