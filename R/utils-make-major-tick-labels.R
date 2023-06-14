#' Create a vector of x-axis tick labels by only populating the labels
#' found at a given modulo, i.e. every nth tick mark will have a label
#'
#' @param x_breaks A vector of the x-axis tick locations that appear on the
#' plot
#' @param modulo The value to use in the modulo operation to determine
#' which tick marks will have a label and which won't
#'
#' @return A vector of labels
#' @export
make_major_tick_labels <- function(x_breaks,
                                   modulo = 5){
  x_labels <- NULL
  for(i in x_breaks){
    if(i %% modulo == 0){
      x_labels <- c(x_labels, i)
    }else{
      x_labels <- c(x_labels, "")
    }
  }

  x_labels
}