#' Inject LaTeX float placement codes for figure and tables, based on their
#' knitr chunk label names
#'
#' @param x Tex code, as a vector of lines read in from a TeX file by
#' [readLines()]
#' @param figs_dir The name of the directory in which the knitr-built
#' figures reside. Used for matching figure import locations inside the tex
#' file
#' @return The modified Tex code, as a vector
#' @export
post_process_set_object_placement <- function(x, figs_dir){

  # x <- post_process_set_tab_fig_placement(
  #   x,
  #   type = "table",
  #   knitr_label = "es-catches-tab",
  #   place = "!tbp")

  x <- post_process_set_tab_fig_placement(
    x,
    type = "table",
    knitr_label = "es-recruitment-tab",
    place = "H")

  x <- post_process_set_tab_fig_placement(
    x,
    type = "table",
    knitr_label = "es-reference-points-tab",
    place = "H")

  x <- post_process_set_tab_fig_placement(
    x,
    type = "table",
    knitr_label = "es-biomass-tab",
    place = "!b")

  x <- post_process_set_tab_fig_placement(
    x,
    type = "figure",
    knitr_label = "es-catches-fig",
    place = "!b")
  x <- post_process_set_tab_fig_placement(
    x,
    type = "figure",
    knitr_label = "es-survey-biomass-fig",
    place = "H")
  x <- post_process_set_tab_fig_placement(
    x,
    type = "figure",
    knitr_label = "es-survey-age1-fig",
    place = "H")
  x <- post_process_set_tab_fig_placement(
    x,
    type = "figure",
    knitr_label = "es-biomass-fig",
    place = "H")
  x <- post_process_set_tab_fig_placement(
    x,
    type = "figure",
    knitr_label = "es-relative-biomass-fig",
    place = "H")
  x <- post_process_set_tab_fig_placement(
    x,
    type = "figure",
    knitr_label = "es-exploitation-fraction-fig",
    place = "H")

  x <- post_process_set_tab_fig_placement(
    x,
    type = "table",
    knitr_label = "es-decisions-biomass-tab",
    place = "H")

  x <- post_process_set_tab_fig_placement(
    x,
    type = "table",
    knitr_label = "es-decisions-spr-tab",
    place = "H")

  x <- post_process_set_tab_fig_placement(
    x,
    type = "figure",
    knitr_label = "main-overview-map-fig",
    place = "H")


}