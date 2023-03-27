#' Modify TeX code to capitalize and/or center section or subsection headers
#'
#' @param tex_vec  Tex code, as a vector of lines read in from a TeX file by
#' [readLines()]
#'
#' @return The modified Tex code, as a vector
#' @export
post_process_section_headers <- function(tex_vec){

  sec <- function(x,
                  inds,
                  type = c("section",
                           "subsection",
                           "subsubsection"),
                  caps = TRUE, center = FALSE){

    type <- match.arg(type)
    upc_text <- paste0("\\", type, "font{\\MakeUppercase}")

    # Make section headers uppercase and centered ----
    if(length(inds)){
      beg <- x[1:(inds[1] - 1)]
      if(length(inds) == 1){
        tmp <- beg
        if(center){
          tmp <- c(tmp, "\\begin{center}")
        }
        if(caps){
          tmp <- c(tmp, upc_text)
        }
        tmp <- c(tmp, x[inds[1]])
        if(center){
          tmp <- c(tmp, "\\end{center}")
        }
        return(c(tmp, x[(inds[1] + 1):length(x)]))
      }else{
        chunks <- map2(inds[1:(length(inds) - 1)],
                       inds[2:length(inds)], \(st, en){
                         tmp <- NULL
                         if(center){
                           tmp <- c(tmp, "\\begin{center}")
                         }
                         if(caps){
                           tmp <- c(tmp, upc_text)
                         }
                         tmp <- c(tmp, x[st])
                         if(center){
                           tmp <- c(tmp, "\\end{center}")
                         }
                         c(tmp, x[(st + 1):(en - 1)])
                       })
        end <- NULL
        if(center){
          end <- c(end, "\\begin{center}")
        }
        if(caps){
          end <- c(end, upc_text)
        }
        end <- c(end, x[inds[length(inds)]])
        if(center){
          end <- c(end, "\\end{center}")
        }
        end <- c(end, x[(inds[length(inds)] + 1):length(x)])

        return(c(beg, unlist(chunks), end))
      }
    }else{
      return(x)
    }
  }

  inds <- grep("\\\\section\\*?\\{.*(})?", tex_vec)
  tex_vec <- sec(tex_vec, inds, type = "section", center = TRUE)
  inds <- grep("\\\\(sub){1}section\\*?\\{.*(})?", tex_vec)
  tex_vec <- sec(tex_vec, inds, type = "subsection")
  section_inds <- grep("\\\\(sub){2}section\\*?\\{.*(})?", tex_vec)
  tex_vec <- sec(tex_vec, inds, type = "subsubsection")

  tex_vec
}
