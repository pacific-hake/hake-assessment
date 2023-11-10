#' Load the Canadian age data from the CSV file which contains it
#'
#' @param fn The CSV filename
#'
#' @return A list of 6 data frames, one age comps and one number of samples
#' for each fleet: Freezer trawlers, shoreside, and JV
#' @export
load_can_age_data <- function(fn){

  if(!file.exists(fn)){
    stop("File `", fn, "` does not exist")
  }

  dat <- readLines(fn)

  # Extract age vector
  ages_ind <- grep("^,1.*$", dat)
  ages_line <- dat[ages_ind]
  dat <- dat[-ages_ind]
  ages <- ages_line |>
    strsplit(",") %>%
    `[[`(1) %>%
    `[`(-1)

  # Extract data
  hdr_line_inds <- grep("^[[:alpha:]].*$", dat)
  headers <- dat[hdr_line_inds]
  headers <- gsub("#+ +", "", headers)
  data_start <- hdr_line_inds + 1
  data_end <- c(hdr_line_inds[-1] - 1, length(dat))
  sect_lst <- map2(data_start, data_end, \(start, end){
    # First row has ages with a leading comma, extract those here:
    j <- dat[start:end] |>
      strsplit(",") |>
      map(~{
        vec2df(.x)
      }) |>
      purrr::map_df(~{.x}) %>%
      mutate_all(~{as.numeric(.)})
    if(ncol(j) == 2){
      # Sample data
      nms <- j |> pull(1)
      j <- j |> pull(2) |> as.character()
      names(j) <- nms
    }else{
      # Age comp data
      yrs <- j |> pull(1)
      j <- j |>
        select(-1) |>
        as.matrix()
      rownames(j) <- yrs
      colnames(j) <- ages
    }
    j
  }) |>
    setNames(headers)

  sect_lst
}
