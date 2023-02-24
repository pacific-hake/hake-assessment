#' Add alternative text to figures in the document via the `tagpdf` LaTeX
#' package
#'
#' @details Searches the given TEX file for the `includegraphics{}`,
#' `\\begin{caption}`, `\\end{caption}`, `\\begin{knitrout}`,
#' `\\end{knitrout}`, and `\\label{fig.*}` and manipulates the
#' structure of those commands with the figure tagging commands
#' injected. Alternative text comes from a data frame created
#' during the knitting process via `knitr` hooks that read in the
#' text from the `alt.text` knitr chunk tags in the figures. Those
#' have to be manually changed each year
#'
#' @param tex_file The name of the TEX file
#' @param alt_fig_text A character vector of alternative text to insert
#'
#' @return Nothing, overwrites the `tex_file`
#' @export
add_alt_text <- function(tex_file = "hake-assessment.tex",
                         alt_fig_text){

  if(is.null(alt_fig_text) || !length(alt_fig_text)){
    stop("`alt_fig_text` has not been populated. Did you remove the `knitr-cache` ",
         "directory before building? This is a necessary step to cerate this list",
         call. = FALSE)
  }
  if(!file.exists(tex_file)){
    stop("File `", tex_file, "` does not exist",
         call. = FALSE)
  }

  # Make a backup of the tex file, in case there is an error we can try
  # again without having to rebuild the whole file using knitr
  backup_fn <- paste0(file_path_sans_ext(tex_file),
                      "-bck.",
                      file_ext(tex_file))

  if(!file.copy(tex_file, backup_fn, overwrite = TRUE, copy.mode = TRUE)){
    stop("Could not make a backup of the file `", text_file, "`",
         call. = FALSE)
  }

  # The match for knitr chunk figures looks like \\includegraphics[width=\\maxwidth]{Filename-1}
  # Some chunks may have more than one plot in them, in those cases only the first plot will
  # have a tooltip added, therefore the -1 at the end of the regular expression
  # knitr-base figures:
  inc_graphics_pattern_knitr <- "(\\\\includegraphics\\[width=\\\\maxwidth\\]\\{(.*?-1)\\})"
  # Manually-added figures (from files previously created). Note it must all be on ONE line in the file,
  # no manual newlines. These don't need processing (alt text added), they just need to be included in
  # the csv file output, in the correct order
  inc_graphics_pattern_manual <- "(^\\\\pdftooltip\\{\\\\includegraphics\\[.*\\]?\\{(.*?)\\}$)"
  lines <- readLines(tex_file)
  # Strip whitespace from the beginning of all lines
  lines <- gsub("^[[:space:]]+", "\\1", lines)
  # Strip out lines that begin with comment character (after removing leading whitespace)
  lines <- lines[!grepl("^%", lines)]

  knitr_inds <- grep(inc_graphics_pattern_knitr, lines)
  manual_inds <- grep(inc_graphics_pattern_manual, lines)
  if(!length(knitr_inds) && !length(manual_inds)){
    stop("There were no `includegraphics{} macros found in the file ",
         "`", tex_file, "`")
  }
  all_inds <- sort(c(knitr_inds, manual_inds))
  alt_fig_text_knitr <- alt_fig_text

  if(length(manual_inds)){
    # Modify alt_fig_text for the manually-placed figures
    insert_row <- function(df, new_row, ind) {
      if(ind > nrow(df)){
        # Insert at the end of the table. This is needed or an extra `NA` will
        # appear in the last row of the table
        df[nrow(df) + 1, ] <- as.list(new_row)
      }else{
        # Insert at the beginning or middle of the table
        df[seq(ind + 1, nrow(df) + 1), ] <- df[seq(ind, nrow(df)), ]
        df[ind, ] <- as.list(new_row)
      }
      df
    }
    # Get the row indices of where to place the manual figures in the alt_figs_text data frame
    manual_row_inds <- map(manual_inds, ~{.x == all_inds}) %>%
      Reduce("|", .) |>
      which()

    walk2(manual_row_inds, manual_inds, ~{
      fig_text <- gsub(inc_graphics_pattern_manual, "\\2", lines[.y])
      new_row <- c("Manually added figure", fig_text)
      alt_fig_text <<- insert_row(alt_fig_text, new_row, .x)
    })
  }else{
    warning("There were no manually-placed 'includegraphics' terms found ",
            "in the TEX file")
    # Make sure this gets seen, if someone is watching
    Sys.sleep(5)
  }

  if(length(all_inds) != length(alt_fig_text$text)){
    # If this trips, check the chunks against the alt text list by using this
    # code. Do ten at a time (1:10 then 11:20, etc) and look at the last one
    # of the ten for a match to narrow it down:
    # k <- alt_fig_text$text
    # kk <- basename(lines[all_inds])
    # a <- 1:10;k[a];message();kk[a];
    stop("The number of lines containing the includegraphics{} macro does ",
         "not match the number of entries in the alt text data frame. Make ",
         " sure you have entered an alt.text chunk entry for all figures",
         call. = FALSE)
  }

  # Number of figures. Some figures may be made up of more than one plot
  begin_fig_inds <- grep("begin\\{figure\\}", lines)
  end_fig_inds <- grep("end\\{figure\\}", lines)
  if(length(begin_fig_inds) != length(end_fig_inds)){
    stop("Number of \\begin{figure} and \\end{figure} are not the same",
         call. = FALSE)
  }
  num_figs <- length(end_fig_inds)

  # Split the lines into two lists of chunks, one is a list of chunks of
  # individual figure tex lines and the other is a list of what is in between
  # those chunks
  fig_chunks <- map2(begin_fig_inds, end_fig_inds, ~{
    lines[.x:.y]
  })

  prev_end_ind <- 0
  non_fig_chunks <- map2(begin_fig_inds, end_fig_inds, ~{
    if(.x == 1 || .y == length(lines)){
      NULL
    }
    out <- lines[(prev_end_ind + 1):(.x - 1)]
    prev_end_ind <<- .y
    out
  })

  if(end_fig_inds[length(end_fig_inds)] != length(lines)){
    non_fig_chunks <- c(non_fig_chunks,
                        list(lines[(prev_end_ind + 1):length(lines)]))
  }


  tag_fig_chunk <- function(chunk,
                            knitr_pat,
                            manual_pat,
                            alt_text_df){

    # Store figure beginning and end lines, and remove from the chunk
    beg <- chunk[1]
    end <- chunk[length(chunk)]
    chunk <- chunk[-c(1, length(chunk))]

    beg_cap_ind <- grep("caption\\{", chunk)
    end_cap_ind <- grep("label\\{fig:", chunk)

    if(length(beg_cap_ind) != length(end_cap_ind)){
      stop("Number of caption beginnings and ends not equal. The caption ",
           "ends are defined by a label line so if that is missing this can ",
           "happen. The chunk is:\n\n",
           paste(c(beg, chunk, end), collapse = "\n"), "\n",
           call. = FALSE)
    }

    # Split the chunk into individual figures based on the ends of the captions
    beg_next_chunk <- 1
    fig_chunks <- map(end_cap_ind, ~{
      ret <- chunk[beg_next_chunk:.x]
      beg_next_chunk <<- .x + 1
      ret
    })

    # Insert figure tagging elements
    fig_chunks <- map(fig_chunks, function(subchunk){

      is_knitr <- as.logical(length(grep("begin\\{knitrout\\}", subchunk)))

      # Captions and labels must exist whether created by knitr or not
      # Caption existence has been done prior to this loop so no checking here
      beg_cap_ind <- grep("caption\\{", subchunk)
      end_cap_ind <- grep("label\\{fig:", subchunk)

      if(is_knitr){
        # The figure was created by knitr
        fig_ind <- grep(knitr_pat, subchunk)
        beg_knitr_ind <- grep("begin\\{knitrout\\}", subchunk)
        end_knitr_ind <- grep("end\\{knitrout\\}", subchunk)
        end_total <- grep("end\\{center\\}", subchunk)
        if(!length(end_total)){
          # If center wasn't used, set the end to the `\\end{knitrout}` line
          end_total <- end_knitr_ind
        }

        ret <- c(subchunk[1:(fig_ind - 1)],
                 paste0("\\tagstructbegin{tag=Figure,alttext=",
                        alt_text_df[1, ]$text,
                        "}"),
                 "\\tagmcbegin{tag=Figure}",
                 subchunk[fig_ind],
                 "\\tagmcend",
                 "\\tagstructend",
                 "\\tagpdfparaOn",
                 "\\tagstructbegin{tag=Caption}",
                 "\\tagmcbegin{tag=Caption}",
                 subchunk[beg_cap_ind:end_cap_ind],
                 "\\tagmcend",
                 "\\tagstructend",
                 subchunk[end_knitr_ind:end_total])
      }else{
        # The figure was inserted manually
        fig_ind <- grep(manual_pat, subchunk)
        # Check subchunk for center command and insert end if necessary
        has_center <- any(grepl("begin\\{center\\}", subchunk))
        ret <- c(subchunk[1:(fig_ind - 1)],
                 paste0("\\tagstructbegin{tag=Figure,alttext=",
                        alt_text_df[1, ]$text,
                        "}"),
                 "\\tagmcbegin{tag=Figure}",
                 subchunk[fig_ind],
                 ifelse(has_center, "\\end{center}", NA),
                 "\\tagmcend",
                 "\\tagstructend",
                 "\\tagpdfparaOn",
                 "\\tagstructbegin{tag=Caption}",
                 "\\tagmcbegin{tag=Caption}",
                 subchunk[beg_cap_ind:end_cap_ind],
                 "\\tagmcend",
                 "\\tagstructend")
        ret <- ret[!is.na(ret)]
      }
      alt_text_df <<- alt_text_df[-1, ]
      ret
    })

    list(c(beg, unlist(fig_chunks), end), alt_text_df)
  }

  figure_chunks <- map(fig_chunks, ~{
    ret <- tag_fig_chunk(.x,
                         knitr_pat = inc_graphics_pattern_knitr,
                         manual_pat = inc_graphics_pattern_manual,
                         alt_fig_text)
    alt_fig_text <<- ret[[2]]
    ret[[1]]
  })

  lines_out <- lines[1:(begin_fig_inds[1] - 1)]
  begin_fig_inds <- c(begin_fig_inds, length(lines) + 1)
  for(i in seq_along(figure_chunks)){
    lines_out <- c(lines_out,
                   figure_chunks[[i]],
                   lines[(end_fig_inds[i] + 1):(begin_fig_inds[i + 1] - 1)])
  }
  writeLines(lines_out, tex_file)
}
