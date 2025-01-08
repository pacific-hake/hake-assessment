#' hake: A package for creating the Pacific hake stock assessment document
#'
#' Uses the [bookdown], [rmarkdown], and [knitr] packages along with
#' TexLive's `lualatex` to generate a PDF of the stock assessment document.
#' Also included are [bookdown] beamer presentation templates for the various
#' meetings required.
#'
#' All of these are in alphabetical order, both packages and functions
#' within them
#' @import adnuts coda dplyr
#' @import GGally ggfx ggh4x ggplot2 ggrepel
#' @import knitr
#' @import magick
#' @import purrr
#' @import rnaturalearth rnaturalearthhires
#' @import stats
#' @rawNamespace import(r4ss, except = c(profile))
#'
#' @importFrom cli cli_abort cli_inform cli_warn symbol
#' @importFrom clipr clipr_available write_clip dr_clipr
#' @importFrom crayon green
#' @importFrom cowplot get_legend plot_grid
#' @importFrom data.table fread
#' @importFrom fs dir_ls path file_chmod
#' @importFrom future availableCores plan
#' @importFrom furrr future_map future_imap furrr_options
#' @importFrom furrr future_walk future_walk2
#' @importFrom gfdata get_commercial_samples run_sql
#' @importFrom gfplot tidy_lengths_raw
#' @importFrom ggmagnify geom_magnify
#' @importFrom ggnewscale new_scale_fill
#' @importFrom glue glue
#' @importFrom graphics arrows grid hist layout legend lines matplot
#' @importFrom graphics mtext par points polygon rect segments
#' @importFrom graphics strwidth symbols text title
#' @importFrom grDevices colorRampPalette gray rgb
#' @importFrom grid convertX gpar grid.draw grid.newpage rectGrob textGrob
#' @importFrom grid unit
#' @importFrom gridExtra arrangeGrob grid.arrange
#' @importFrom gtable gtable_add_grob gtable_add_rows
#' @importFrom gtools running
#' @importFrom kableExtra add_header_above column_spec kable_styling kbl
#' @importFrom kableExtra landscape linebreak pack_rows row_spec
#' @importFrom here here i_am
#' @importFrom lubridate day hour minute month now second seconds_to_period year
#' @importFrom marmap fortify.bathy getNOAA.bathy
#' @importFrom minpack.lm nlsLM
#' @importFrom parallelly supportsMulticore
#' @importFrom RColorBrewer brewer.pal.info brewer.pal
#' @importFrom readr cols read_csv read_table2 write_csv
#' @importFrom rlang `%||%` as_label enquos set_names sym
#' @importFrom rgl rglwidget
#' @importFrom scales comma ordinal rescale
#' @importFrom sf st_as_sf st_cast st_combine st_crop st_crs st_crs<-
#' @importFrom sf st_transform st_coordinates
#' @importFrom stringr str_count str_flatten str_flatten_comma str_split str_sub
#' @importFrom tools file_path_sans_ext file_ext
#' @importFrom tibble as_tibble as_tibble_row enframe
#' @importFrom tidyselect all_of any_of matches one_of starts_with
#' @importFrom tidyr complete pivot_longer pivot_wider unnest
#' @importFrom utils head globalVariables menu object.size read.csv read.table
#' @importFrom utils tail type.convert write.csv write.table
#' @importFrom vctrs vec_as_names vec_group_rle
#' @importFrom withr defer

#' @docType _PACKAGE
#' @name hake
NULL
