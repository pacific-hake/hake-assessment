#' Create plots suring the loading phase, and return a list of them to be
#' attached to the model object for saving in a model RDS file
#'
#' @details
#' Assumes that many global variables exist. While this is usually bad form,
#' this one function is only to be called from within [create_rds_file()]
#' at a point when all those variables exist. If they do not, the Rmd files
#' doc/002- through 005-* must be sourced prior to calling this.
#'
#' @param model A model, created by [create_rds_file()]
#' @param probs A vector of 3 values, the lower CI, median, and upper CI
#'
#' @return A list of [ggplot2::ggplot()] objects
plot_during_loading <- function(model,
                                probs){

  out <- list()

  # Plot the survey fit with many MCMC posterior lines
  # This is done MUST be made here because the MCMC object is not stored
  # in the RDS files
  out$survey_fit <- plot_survey_fit_mcmc(model,
                                         type = "acoustic",
                                         n_posts = 1000,
                                         probs = probs)

  out$age1_index_fit <- plot_survey_fit_mcmc(model,
                                             type = "age1",
                                             n_posts = 1000,
                                             ylim = c(0, 10),
                                             probs = probs)

  out$overview_map <- plot_overview_map(ports_df, states_df)

  out$catches <- plot_catches(ct, xlim = c(start_yr, last_data_yr))

  out$us_depths <- plot_depth_2_panel(us_atsea_fishing_depth_df,
                                      us_atsea_bottom_depth_df,
                                      yrs = (last_data_yr - 4):last_data_yr)

  out$can_depths <- plot_depth_4_panel(can_ft_gear_depth_df,
                                       can_ft_bottom_depth_df,
                                       can_ss_gear_depth_df,
                                       can_ss_bottom_depth_df,
                                       fleet_1_name = "Freezer trawlers",
                                       fleet_2_name = "Shoreside",
                                       yrs = (last_data_yr - 4):last_data_yr)


  out$data_overview <- plot_data_summary(model)

  out$plots$age_comp_bubbles <-
    {
      p_lst <- NULL
      p_lst[[1]] <- plot_age_comp_bubbles(model,
                                          type = "fishery",
                                          leg_pos = "top",
                                          alpha = 0.4)
      p_lst[[2]] <- plot_age_comp_bubbles(model,
                                          type = "survey",
                                          alpha = 0.4)
      suppressWarnings(plot_grid(plotlist = p_lst, nrow = 2))
    }

  out$survey_extrap_biomass <-
    plot_survey_biomass(model, index = "age2")

  out$survey_age1 <- plot_survey_biomass(model,
                                         index = "age1",
                                         y_lim = c(0, 16))
  out$maturity_ogive <-
    {
      p_lst <- list()
      p_lst[[1]] <- plot_maturity_ogives(model, maturity_samples_df)
      p_lst[[2]] <- plot_fecundity(model,
                                   d = maturity_samples_df,
                                   yrs = start_yr_age_comps:(end_yr - 1))
      plot_grid(plotlist = p_lst, ncol = 1)
    }

  out$weight_at_age <- plot_heatmap_weight_at_age(
    model,
    cell_font_size = 3,
    sample_size_df = weight_age_sample_sizes,
    pre_yrs = start_yr_age_comps:model$endyr,
    pre_func = mean,
    post_yrs = (model$endyr - 4):model$endyr,
    post_func = mean,
    cols = c("red",
             "yellow",
             "green",
             "dodgerblue"))

  out$sample_size_weight_at_age <- plot_heatmap_sample_size_weight_at_age(
    model,
    cell_font_size = 3,
    sample_size_df = weight_age_sample_sizes,
    pre_yrs = start_yr_age_comps:model$endyr,
    pre_func = mean,
    post_yrs = (model$endyr - 4):model$endyr,
    post_func = mean,
    cols = c("purple",
             "lightgreen",
             "yellow",
             "dodgerblue"))

  out$weight_at_age_lines <- plot_weight_at_age(wt_at_age,
                                                ages = 2:10,
                                                bold_ages = 5,
                                                cols = c("purple",
                                                         "darkblue",
                                                         "yellow",
                                                         "darkgreen"))
out$bridge_summary <-
  {
    plist <- list()
    plist[[1]] <-
      plot_rel_biomass(d_obj = d_obj_bridge_rel_biomass[[1]],
                       ylim = c(0, 2.5),
                       x_labs_mod = 10,
                       clip_cover = 0.35,
                       wrap_y_label = TRUE,
                       axis_title_font_size = 12,
                       leg_pos = "none")
    plist[[2]] <-
      plot_recdevs(d_obj = d_obj_bridge_recdev[[1]],
                   leg_pos = "none",
                   x_labs_mod = 10,
                   clip_cover = 0.25,
                   axis_title_font_size = 12,
                   line_width = 0.25)

    plist[[3]] <-
      plot_survey_index_fits(d_obj = d_obj_bridge_age2_index[[1]],
                             survey_type = "age2",
                             rev_colors = FALSE,
                             clip_cover = 0.25,
                             axis_title_font_size = 12,
                             leg_pos = "none")

    plist[[4]] <-
      plot_survey_index_fits(d_obj = d_obj_bridge_age1_index[[1]],
                             survey_type = "age1",
                             xlim = c(1995, 2022),
                             ylim = c(0, 11),
                             y_breaks = seq(0, 11, by = 2),
                             clip_cover = 0.25,
                             axis_title_font_size = 12,
                             rev_colors = FALSE,
                             leg_pos = "none")

    # Grid of 2 rows and 1 column with one plot at the top (biomass)
    # and 4 plots embedded in another grid (quad_grid) at the bottom
    quad_grid <- cowplot::plot_grid(plotlist = plist,
                                    nrow = 2,
                                    ncol = 2)
    full_plist <- list()
    full_plist[[1]] <-
      plot_biomass(d_obj = d_obj_bridge_biomass[[1]],
                   leg_ncol = 2,
                   leg_font_size = 7,
                   wrap_y_label = TRUE,
                   axis_title_font_size = 12,
                   leg_pos = c(0.72, 0.82))

    full_plist[[2]] <- quad_grid

    cowplot::plot_grid(plotlist = full_plist,
                       nrow = 2,
                       ncol = 1,
                       rel_heights = c(0.33, 0.67))
  }

out$age_comp_fits <-
  {
    plist <- NULL
    plist[[1]] <-
      plot_age_comp_fit(base_model,
                        label_font_size = 3,
                        n_col = 4)
    plist[[2]] <-
      plot_age_comp_fit(base_model,
                        n_col = 1,
                        type = "survey",
                        x_breaks = 2:15,
                        label_font_size = 3,
                        label_loc = c(14, 0.45))
    plot_grid(plotlist = plist, nrow = 2)
  }


out
}