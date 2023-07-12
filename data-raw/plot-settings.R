# Source this file to apply changes. Don't forget to add documentation for
# any new package data to file R/data.R

# Shape, size, and stroke of points for multiple model time series plots
create_data_hake("ts_pointshape", 16)
create_data_hake("ts_pointsize", 1.5)
create_data_hake("ts_pointstroke", 1)

# Line width for multiple model time series plots (don't need color or type
# because those is assigned by the plotting functions)
create_data_hake("ts_linewidth", 0.5)

# Gap between lines and where they connect to points in all time series plots
create_data_hake("ts_linegap", 0.2)

# Attributes applicable to single model plots only
create_data_hake("ts_single_model_linecolor", "black")
create_data_hake("ts_single_model_linewidth", 1)
create_data_hake("ts_single_model_linetype", "solid")
create_data_hake("ts_single_model_pointcolor", "black")
create_data_hake("ts_single_model_pointshape", 1)
create_data_hake("ts_single_model_pointsize", 2)
create_data_hake("ts_single_model_pointstroke", 1.25)
create_data_hake("ts_single_model_ribbon_fill", "royalblue")
create_data_hake("ts_single_model_ribbon_linetype", "dashed")

# Attributes applicable to both single model and multiple model plots
create_data_hake("ts_ribbon_alpha", 0.1)
create_data_hake("ts_ribbon_linetype", "dotted")

# Attributes applicable to non-time-series plots
create_data_hake("main_fill", "royalblue")
create_data_hake("main_alpha", 0.7)

# Attributes applicable to age bubble plots with diagonal cohort lines
create_data_hake("age_diag_linecolor", "darkgreen")
create_data_hake("age_diag_linewidth", 1)
create_data_hake("age_diag_linetype", "solid")
create_data_hake("age_fillcolor", "royalblue")

# Attributes applicable to axis fonts for all plots
create_data_hake("axis_title_font_size", 14)
create_data_hake("axis_tick_font_size", 11)
create_data_hake("axis_label_color", "black")
create_data_hake("minor_tick_length", 0.1)

create_data_hake("ts_refpt_bo_linecolor", "blue")
create_data_hake("ts_refpt_usr_linecolor", "green")
create_data_hake("ts_refpt_lrp_linecolor", "red")
create_data_hake("ts_refpt_bo_linewidth", 0.75)
create_data_hake("ts_refpt_usr_linewidth", 0.5)
create_data_hake("ts_refpt_lrp_linewidth", 0.5)
create_data_hake("ts_refpt_bo_linetype", "dashed")
create_data_hake("ts_refpt_usr_linetype", "dashed")
create_data_hake("ts_refpt_lrp_linetype", "dashed")
