#' Extract contents of text found in a `(ref:variable-alt)` found in any rmd
#' file
#'
#' @details
#' 1. Starts by parsing the `_bookdown.yml` file to extract the names of all
#'    uncommented rmd files in the current build.
#' 2. Searches all those files for any lines starting with `(ref:variable-alt)`.
#'    Preceding spaces are ignored.
#' 3. Extracts all the text found for the label given by `inp_str`, and
#'    returns the description found for it (the actual alternative text)
#'
#' @param inp_str The string to match, in the format `(ref:alt-text-label)`
#'
#' @return The text found for the label given by `inp_str`
extract_alt_text <- function(inp_str){


  bd_lines <- readLines(here("doc/_bookdown.yml"))
  bd_rmd_raw <- grep("\\.rmd", bd_lines, value = TRUE)
  # Remove commented-out lines (for speed)
  bd <- gsub("^ *", "", bd_rmd_raw)
  if(length(grep("^#", bd))){
    bd <- bd[-grep("^#", bd)]
  }

  fns <- gsub(".*([0-9]{3}\\-[a-zA-Z\\-]+\\.rmd).*", "\\1", bd)
  fns <- here("doc", fns)

  alt_str <- paste0(inp_str, "-alt")
  k <- map(fns, ~{
    rmd <- readLines(.x)
    x <- grep(alt_str, rmd)
    if(length(x)){
      if(!length(x)){
        return("No alternative text defined for this figure")
      }
      if(length(x) > 1){
        stop("Alt. text label `", alt_str, "-alt` defined more than once in ",
             "file `", .x, "`")
      }
      # Find all lines that belong in the alt text (there may be newlines
      # in between them in the source rmd file). Assuming that after the
      # alt t5ext is done, it will be followed by either a blank line or
      # the start of a chunk (starts with ```), or the end-of-file
      start_ind <- x
      end_ind <- x
      repeat{
        # Check the line for a new chunk or a blank line
        is_chunk <- grep("```", rmd[end_ind]) |>
          length() |>
          as.logical()
        is_blank_line <-  grep("^$", rmd[end_ind]) |>
          length() |>
          as.logical()
        is_eof <<- end_ind == length(rmd)
        if(is_chunk || is_blank_line || is_eof){
          break
        }
        end_ind <- end_ind + 1
      }
      # Now on either chunk start of blank line, so remove that line from
      # the text, checking the EOF conditions
      if(is_eof){
        # Only need to check if there's a blank line. A chunk cannot start
        # (and end) on the same line so no need to check that at EOF
        if(!length(grep("^$", rmd[length(rmd)]))){
          end_ind <- end_ind - 1
        }
      }else{
        end_ind <- end_ind - 1
      }
      # Glue all the text lines together
      alt_text <- paste(rmd[start_ind:end_ind], collapse = " ")
      # Remove the label
      ref_regex <- paste0("\\(ref:", alt_str, "\\) *")
      alt_text <- gsub(ref_regex, "", alt_text)

      # Return a vector of the label and it's text
      alt_text
    }
  })

  # Remove all NULLs from the list
  k[sapply(k, is.null)] <- NULL

  if(!length(k)){
    stop("Error retrieving your label ", inp_str, ". There were no matching ",
         "labels found with a description for this label")
  }
  if(length(k) > 1){
    stop("Error retrieving your label ", inp_str, ". There was more ",
         "than one text dfescription. Returned values:\n", k)
  }

  k <- unlist(k)

  # Replace any inline r code with actual text (mini-knitr parser)
  # Break up the string into chunks before and after the inline r code chunks
  # TEST
  # sp <- "hake"
  # common_name <- "this is the common name"
  # k <- "`r sp` `r sp` - Trying to match `r sp` with another `r 10 + 29 * 30` chunk `r common_name`."

  backtick_inds <- unlist(gregexpr("`", k))
  if(backtick_inds[1] == -1){
    backtick_inds <- NULL
  }
  if(!length(backtick_inds)){
    return(k)
  }
  if(length(backtick_inds) %% 2 != 0){
    stop("There is an odd number of backticks in the text referred ",
         "to by label ", inp_str, ". The text is:\n", k)
  }

  chunks_non_r <- str_split(k, "`r .*?`")[[1]]
  chunks_non_r <- chunks_non_r[chunks_non_r != ""]

  # Number of backticks are even as they must be, so break them into chunks
  start_inds <- backtick_inds[seq(1, length(backtick_inds), 2)]
  # Check to make sure the starting backticks have an 'r' immediately after
  walk(start_inds, ~{
    if(substr(k, .x + 1, .x + 1) != "r" && substr(k, .x + 2, .x + 2) != " "){
      stop("Non-r code chunk found. R code chunks must be of the format ",
           "`r code_here`")
    }
  })
  end_inds <- backtick_inds[seq(2, length(backtick_inds), 2)]
  chunks <- str_sub(k, start_inds, end_inds)

  # Evaluate the R chunks
  chunks <- map_chr(chunks, ~{
    # Remove `r and ` from the code
    x <- gsub("^`r", "", .x)
    x <- gsub("`$", "", x)
    x <- gsub(" +", "", x)
    eval(parse(text = x))
  })
  # Here we have chunks and chunks_non_r. We needd to find out which comes fist,
  chunk_len <- max(length(chunks), length(chunks_non_r))
  length(chunks) <- chunk_len
  length(chunks_non_r) <- chunk_len

  if(start_inds[1] == 1){
    out_str <- c(rbind(chunks, chunks_non_r))
  }else{
    out_str <- c(rbind(chunks_non_r, chunks))
  }
  out_str <- out_str[!is.na(out_str)]
  out_str <- paste(out_str, collapse = "")

  out_str
}