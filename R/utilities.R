#' Replace '+' with a newline in the given string
#'
#' @details
#' Used mainly for ggplot y axis labels, if they are too long
#' and get cut off
#'
#' @param x A character string
#' @param ... Absorbs other arguments not meant for this function
#'
#' @return A modified character string
add_newlines <- function(x, ...){
  gsub("\\+", "\n", x)
}

# Functions to make table generation easier -----------------------------------

#' Create an RGB string of the specified color and opacity
#'
#' @details Format of returned string is #RRGGBBAA where
#' RR = red, a 2-hexadecimal-digit string
#' GG = green, a 2-hexadecimal-digit string
#' BB = blue, a 2-hexadecimal-digit string
#' AA = opacity, 2-digit string
#'
#' @param color A vector of R color strings or numbers
#' @param opacity A number between 0 and 99
#'
#' @return An RGB string of the specified color and opacity
#' @export
get.shade <- function(color, opacity){

  stopifnot(opacity > 0 & opacity < 100)

  colorDEC <- col2rgb(color)
  if(is.matrix(colorDEC)){
    colorHEX <- matrix(nrow = 3, ncol = ncol(colorDEC))
    shade <- NULL
    for(col in 1:ncol(colorDEC)){
      for(row in 1:nrow(colorDEC)){
        colorHEX[row, col] <- sprintf("%X", colorDEC[row,col])
        if(nchar(colorHEX[row,col]) == 1){
          colorHEX[row, col] <- paste0("0", colorHEX[row,col])
        }
      }
      shade[col] <- paste0("#",
                           colorHEX[1, col],
                           colorHEX[2, col],
                           colorHEX[3, col],
                           opacity)
    }
  }else{
    colorHEX <- sprintf("%X", colorDEC)
    for(i in 1:length(colorHEX)){
      if(nchar(colorHEX[i]) == 1){
        colorHEX[i] <- paste0("0", colorHEX[i])
      }
    }
    shade <- paste0("#", colorHEX[1], colorHEX[2], colorHEX[3], opacity)
  }
  shade
}


#' Add a polygon to a plot
#'
#' @param yrvec A vector of years
#' @param lower A vector of lower CI values
#' @param upper A vector of upper CI values
#' @param color The color to make the polygon lines
#' @param shade.col The shade color to fill in the polygon with
#'
#' @return [base::invisible()]
#' @export
addpoly <- function(yrvec,
                    lower,
                    upper,
                    color = 1,
                    shade.col = NA){

  # max of value or 0
  lower[lower < 0] <- 0
  if(is.na(shade.col)){
    shade.col <- rgb(t(col2rgb(color)), alpha = 0.2 * 255, maxColorValue = 255)
  }
  polygon(x = c(yrvec, rev(yrvec)),
          y = c(lower, rev(upper)),
          border = NA,
          col = shade.col)
  lines(yrvec, lower, lty = 3, col = color)
  lines(yrvec, upper, lty = 3, col = color)
  invisible()
}


