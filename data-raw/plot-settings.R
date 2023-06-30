# Time series plots
ts_ribbon_fill <- "royalblue"
ts_ribbon_alpha <- 0.1
ts_ribbon_linetype <- "dotted"

ts_pointshape <- 16
ts_pointsize <- 2
ts_linewidth <- 1

ts_single_model_linecolor <- "black"
ts_single_model_pointshape <- 1

# Non-time series plots
main_fill <- "royalblue"
main_alpha <- 0.7

# Age bubble plots
age_diag_line_color = "darkgreen"
age_diag_line_width = 1
age_diag_line_type = "solid"

# All plots
axis_title_font_size <- 14
axis_tick_font_size <- 11
axis_label_color <- "black"
minor_tick_length <- 0.1

# Source this file to see the changes
library(usethis)
use_data(ts_ribbon_fill, overwrite = TRUE)
use_data(ts_ribbon_alpha, overwrite = TRUE)
use_data(ts_ribbon_linetype, overwrite = TRUE)
use_data(ts_pointshape, overwrite = TRUE)
use_data(ts_pointsize, overwrite = TRUE)
use_data(ts_linewidth, overwrite = TRUE)
use_data(ts_single_model_linecolor, overwrite = TRUE)
use_data(ts_single_model_pointshape, overwrite = TRUE)

use_data(main_fill, overwrite = TRUE)
use_data(main_alpha, overwrite = TRUE)

use_data(age_diag_line_color, overwrite = TRUE)
use_data(age_diag_line_width, overwrite = TRUE)
use_data(age_diag_line_type, overwrite = TRUE)

use_data(axis_title_font_size, overwrite = TRUE)
use_data(axis_tick_font_size, overwrite = TRUE)
use_data(axis_label_color, overwrite = TRUE)
use_data(minor_tick_length, overwrite = TRUE)
