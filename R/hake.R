#' hake: A package for creating the Pacific hake stock assessment document
#'
#' Uses the `knitr` R package along with `lualatex` to generate a PDF
#' of the stock assessment document. Aldo included are beamer presentation
#' templates for the various meetings required.
#'
#' @import adnuts coda dplyr
#' @import ggh4x ggplot2 ggrepel
#' @import rnaturalearthhires r4ss
#'
#' @importFrom cli symbol
#' @importFrom crayon green
#' @importFrom cowplot plot_grid
#' @importFrom future plan
#' @importFrom furrr future_map future_imap furrr_options
#' @importFrom glue glue
#' @importFrom graphics arrows grid hist layout legend lines matplot
#' @importFrom graphics mtext par points polygon rect segments
#' @importFrom graphics strwidth symbols text title
#' @importFrom grDevices colorRampPalette gray rgb
#' @importFrom grid gpar grid.draw grid.newpage rectGrob textGrob
#' @importFrom gridExtra arrangeGrob
#' @importFrom here here
#' @importFrom lubridate hour minute month second seconds_to_period year
#' @importFrom purrr map imap map2 map_chr map_dbl map_lgl walk walk2 flatten
#' @importFrom purrr map_df map_dfr map_dfc map2_df map2_dfc map2_dfr pmap
#' @importFrom RColorBrewer brewer.pal.info brewer.pal
#' @importFrom readr cols read_csv read_table2 write_csv
#' @importFrom rlang sym
#' @importFrom scales comma rescale
#' @importFrom sf st_as_sf st_crs<- st_coordinates
#' @importFrom stats end median na.omit quantile rbeta reshape rlnorm rnorm runif setNames start
#' @importFrom stringr str_split
#' @importFrom tools file_path_sans_ext file_ext
#' @importFrom tibble as_tibble enframe
#' @importFrom tidyr all_of any_of pivot_longer pivot_wider unnest
#' @importFrom utils  head object.size read.csv read.table tail type.convert write.csv write.table
#' @importFrom xtable xtable

#' @docType package
#' @name hake
NULL
