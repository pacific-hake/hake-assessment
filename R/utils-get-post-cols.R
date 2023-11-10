#' Extract time series columns for a given parameter and change the column
#' names to years. Optionally, divide all values in the resulting data frame
#' by some value
#'
#' @param df A data frame
#' @param pat A regular expression to be used to extract columns from `df`.
#' If the column names start with this, they will be extracted and renamed
#' @param scale A value to divide all resulting values in the table by
#'
#' @return A simple data frame with years as columns and values as elements
#' @export
get_post_cols <- function(df, pat, scale = 1, exact = FALSE){

  if(exact){
    patt <- paste0("^", pat, "$")
  }else{
    patt <- paste0("^", pat, "_([0-9]{4})$")
  }

  d <- df |>
    as_tibble() |>
    select(matches(patt))

  if(!nrow(d)){
    stop("`get_post_cols()`: The regular expression `", patt, "` did not ",
         "match and column names in the data frame `df`")
  }

  if(!exact){
    # Change names to only years using the same regular expression used above
    d <- d %>%
      setNames(gsub(patt, "\\1", names(.)))
  }

   d |>
    mutate(across(everything(), ~{.x / scale}))
}
