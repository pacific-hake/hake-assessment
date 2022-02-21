#' Add alternative text to figures in a TEX document, overwriting the original file
#'
#' @details Searches the given TEX file for the term `includegraphics` and inserts alternative
#' text for each one from the given `alt_fig_text`
#' @param tex_file The name of the TEX file
#' @param alt_fig_text A character vector of alternative text to insert
#' @param debug Show the matching `includegraphics` lines from the `tex_file` and their
#' line numbers and the `alt_fig_text` data frame for comparison
#'
#' @return Nothing
#' @export
add_alt_text <- function(tex_file = "hake-assessment.tex",
                         alt_fig_text,
                         debug = TRUE){

  # The match for knitr chunk figures looks like \\includegraphics[width=\\maxwidth]{Filename-1}
  # Some chunks may have more than one plot in them, in those cases only the first plot will
  # have a tooltip added, therefore the -1 at the end of the regular expression
  # knitr-base figures:
  inc_graphics_pattern_knitr <- "(\\\\includegraphics\\[width=\\\\maxwidth\\]\\{(.*?-1)\\})"
  # Manually-added figures (from files previously created). Note it must all be on ONE line in the file,
  # no manual newlines. These don't need processing (alt text added), they just need to be included in
  # the csv file output, in the correct order
  inc_graphics_pattern_manual <- "(^\\\\pdftooltip\\{\\\\includegraphics\\[.*\\]?\\{(.*?)\\}$)"
  j <- readLines(tex_file)
  g_knitr <- grep(inc_graphics_pattern_knitr, j)
  g_manual <- grep(inc_graphics_pattern_manual, j)
  g_all <- sort(c(g_knitr, g_manual))
  alt_fig_text_knitr <- alt_fig_text

  if(!length(g_knitr)){
    stop("There were no knitr-created 'includegraphics' terms found in the TEX file")
  }
  if(length(g_manual)){
    # Modify alt_fig_text for the manually-placed figures
    insert_row <- function(df, new_row, ind) {
      df[seq(ind + 1, nrow(df) + 1), ] <- df[seq(ind, nrow(df)), ]
      df[ind, ] <- as.list(new_row)
      df
    }
    # Get the row indices of where to place the manual figures in the alt_figs_text data frame
    manual_ind <- map(g_manual, ~{.x == g_all}) %>%
      Reduce("|", .) %>%
      which

    map2(manual_ind, g_manual, ~{
      fig_text <- gsub(inc_graphics_pattern_manual, "\\2", j[.y])
      new_row <- c("Manually added figure", fig_text)
      alt_fig_text <<- insert_row(alt_fig_text, new_row, .x)
    })
  }else{
    warning("There were no manually-placed 'includegraphics' terms found in the TEX file")
    # Make sure this gets seen, if someone is watching
    Sys.sleep(5)
  }
  # Figures before line 4000 in the TEX file are assumed to be in the executive summary
  # After 4000 are grouped together
  num_exec_summary_figs <- length(g_all[g_all < 4000])
  exec_summary_figs <- paste("Figure", letters[seq_len(num_exec_summary_figs)])
  num_figure_section_figs <- length(g_all[g_all >= 4000])
  # Assumes the Executive summary is always present. The rest can be missing though.
  if(num_figure_section_figs){
    figure_section_figs <- paste("Figure", seq_len(num_figure_section_figs))
    figure_summary <- enframe(c(exec_summary_figs, figure_section_figs), name = NULL) %>%
      bind_cols(alt_fig_text) %>%
      select(value, text) %>%
      rename(`Figure tag` = value, `Alternative text` = text)
  }else{
    figure_summary <- enframe(exec_summary_figs, name = NULL) %>%
      bind_cols(alt_fig_text) %>%
      select(value, text) %>%
      rename(`Figure tag` = value, `Alternative text` = text)
  }
  write_csv(figure_summary, "alternative_text.csv")
  if(debug){
    message("add_alt_text() matched the following lines in the file ", tex_file)
    print(map2(1:length(g_knitr), j[g_knitr], ~{
      sprintf("line %d: %s", g_knitr[.x], .y)
    }) %>% map_chr(~{.x}))
    message("\nadd_alt_text() has the following alt_fig_text descriptions (from knit_hooks): ")
    print(alt_fig_text)
    cat("\n\n")
    if(length(g_all) != nrow(alt_fig_text)){
      message("The alt_fig_text data frame does not have the same number of entries as ",
              "the number of figures detected. Did you forget to add is.fig = TRUE to a figure chunk?")
    }
  }else{
    if(length(g_all) != nrow(alt_fig_text)){
      stop("The alt_fig_text data frame does not have the same number of entries as ",
           "the number of knitr figures detected. Did you forget to add is.fig = TRUE to a figure chunk? ",
           "To debug, call add_alt_text() with debug = TRUE",
           call. = FALSE)
    }
  }
  modified_lines <- map2(j[g_knitr], alt_fig_text_knitr$text, ~{
    gsub(inc_graphics_pattern_knitr,
         paste0("\\\\pdftooltip{\\1}{", .y, "}"),
         .x)
  }) %>% map_chr(~{.x})
  j[g_knitr] <- modified_lines
  writeLines(j, tex_file)
}
