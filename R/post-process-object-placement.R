#' Modify the tex code `x` by changing the table or figure label placement
#' code for the `knitr_label` to the value of `place`
#'
#' @param x Tex code, as a vector of lines read in from a TeX file by
#' @param row_df A data frame which is a single row, and has the columns
#' `type`, `label`, `file_name`, and `placement`
#' @param row_num Which row this is from the object placement file
#' @param ... Absorbs other arguments not meant for this function
#'
#' @return The modified Tex code, as a vector
#' @export
post_process_object_placement <- function(x,
                                          row_df,
                                          row_num,
                                          num_search_lines = 5,
                                          ...){

  if(nrow(row_df) != 1){
    stop("There should only be one row in the data frame passed in to ",
         "`row_df`")
  }
  if(!"type" %in% names(row_df) ||
     !"label" %in% names(row_df) ||
     !"file_name" %in% names(row_df) ||
     !"placement" %in% names(row_df)){
    stop("The one-row data frame must have the columns `type`, `label`, ",
         "`file_name`, and `placement`. One or more are missing")
  }
  if(is.na(row_df$label) && is.na(row_df$file_name)){
    stop("There is an error in the file `", object_placement_fn , "`",
         "Both `label` and `file_name` have no value in row ", row_num)
  }
  if(is.na(row_df$placement)){
    stop("There is an error in the file `", object_placement_fn , "`. ",
         "The `placement` column is missing a value in row ", row_num)
  }

  if(row_df$type == "table"){
    label <- paste0("\\\\caption\\{\\\\label\\{tab:", row_df$label)
  }else if(row_df$type == "figure"){
    if(is.na(row_df$label)){
      label <- paste0("\\/", row_df$file_name)
    }else{
      label <- paste0("\\/", row_df$label)
    }
  }else{
    stop("The `type` is ", row_df$type, " for row ", row_num, " in `",
         object_placement_fn , "`. It must be either 'figure' or 'table'")
  }

  ind <- grep(label, x)
  if(!length(ind)){
    warning(row_df$type, " label `", row_df$label, "`, was not found in ",
            "the tex file. It was not placed as requested in the `",
            object_placement_fn , "` file")
    return(x)
  }

  if(length(ind) > 1){
    warning("There was more than one ", row_df$type," label `", row_df$label,
            "`, found in the tex file. It was not placed as requested in the `",
            object_placement_fn , "` file")
    return(x)
  }

  srch_lines <- x[(ind - num_search_lines):(ind - 1)]
  if(row_df$type == "table"){
    beg_ind <- grep("\\\\begin\\{.*?table\\}", srch_lines)
  }else if(row_df$type == "figure"){
    beg_ind <- grep("\\\\begin\\{figure\\}", srch_lines)
  }

  if(!length(beg_ind)){
    type_txt <- ifelse(row_df$type == "table", ".*?table", "figure")
    warning("Did not find the line \\begin{", type_txt, "} associated with the ",
            row_df$type, " inclusion line `", x[ind], "`. Consider increasing ",
            "the number of lines searched above it (`num_search_lines` argument)")
  }
  beg_ind <- ind - num_search_lines + beg_ind - 1

  # Replace any placement values
  if(row_df$type == "table"){
    x[beg_ind] <- gsub("\\\\begin", "\\\\begin", x[beg_ind])
    has_sq_brac <- length(grep("\\[", x[beg_ind]))
    if(has_sq_brac){
      x[beg_ind] <- gsub("(\\[[a-zA-Z\\!]+\\])(.*)$",
                         paste0("[", row_df$placement, "]\\2"), x[beg_ind])
    }else{
      x[beg_ind] <- paste0(x[beg_ind],
                           "[",
                           row_df$placement,
                           "]")
    }
  }else if(row_df$type == "figure"){
    x[beg_ind] <- gsub("^(\\\\begin\\{figure\\})(\\[[a-zA-Z\\!]+\\])?(.*)$",
                       paste0("\\\\begin{figure}",
                              "[",
                              row_df$placement,
                              "]"),
                       x[beg_ind])
  }

  x
}
