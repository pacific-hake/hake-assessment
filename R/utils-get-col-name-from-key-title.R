#' Get the name of a column in the input data frame that matches a regular
#' expression in the `key_posteriors` list, given a title as found in
#' `key_posteriors_titles`
#'
#' @param df The data frame with column names
#' @param pat The regular expression used to match a title in
#' `key_posteriors_titles`
#'
#' @return A string, which is the name of a column in the data frame `df`
#' @export
get_col_name_from_key_title <- function(df = NULL,
                                        pat = NULL){

  stopifnot(!is.null(df))
  stopifnot(!is.null(pat))

  ind <- grep(pat, key_posteriors_titles)
  if(!length(ind)){
    warning("`get_col_name_from_key_title()` could not match a value in ",
            "`key_posteriors_titles` matching the regular expression `",
            pat, "`")
    return(NULL)
  }
  if(length(ind) > 1){
    stop("`get_col_name_from_key_title()` matched more than one value in ",
         "`key_posteriors_titles` matching the regular expression `",
         pat, "`")
  }
  key <- key_posteriors[ind][[1]]
  ind <- grep(key, names(df))
  if(!length(ind)){
    warning("`get_col_name_from_key_title()` could not find a value in the ",
            "column names of the `mcmc` posteriors data frame matching the ",
            "regular expression `", key, "`")
    return(NULL)
  }
  if(length(ind) > 1){
    stop("`get_col_name_from_key_title()` matched more than one value in ",
         "the column names of the `mcmc` posteriors data frame matching ",
         "the regular expression `", key, "`")
  }

  names(df)[ind]
}