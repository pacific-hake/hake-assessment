# Time series plots
ts_ribbon_fill <- "royalblue"
ts_ribbon_alpha <- 0.1
ts_ribbon_linetype <- "dotted"
ts_pointshape <- 16
ts_pointsize <- 2
ts_linewidth <- 1
ts_single_line_color <- "black"

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
usethis::use_data(ts_ribbon_fill, overwrite = TRUE)
usethis::use_data(ts_ribbon_alpha, overwrite = TRUE)
usethis::use_data(ts_ribbon_linetype, overwrite = TRUE)
usethis::use_data(ts_pointshape, overwrite = TRUE)
usethis::use_data(ts_pointsize, overwrite = TRUE)
usethis::use_data(ts_linewidth, overwrite = TRUE)
usethis::use_data(ts_single_line_color, overwrite = TRUE)

usethis::use_data(main_fill, overwrite = TRUE)
usethis::use_data(main_alpha, overwrite = TRUE)

usethis::use_data(age_diag_line_color, overwrite = TRUE)
usethis::use_data(age_diag_line_width, overwrite = TRUE)
usethis::use_data(age_diag_line_type, overwrite = TRUE)

usethis::use_data(axis_title_font_size, overwrite = TRUE)
usethis::use_data(axis_tick_font_size, overwrite = TRUE)
usethis::use_data(axis_label_color, overwrite = TRUE)
usethis::use_data(minor_tick_length, overwrite = TRUE)
