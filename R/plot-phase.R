#' Create plot of the relative fishing intensity in year t-1 against relative
#' spawning biomass in year t, as a historical look at the fishery for the
#' MCMC given by model
#'
#' @param model A model, created by [create_rds_file()]
#' @param start_yr The minimum year in the plot
#' @param end_yr The maximum year in the plot
#' @param x_lim Vector of two values for minimum and maximum x-axis values
#' @param y_lim Vector of two values for minimum and maximum y-axis values
#' @param biomass_scale Amount to divide biomass by so the axis has small nums
#' @param init_lbl_x_off Initial year label x offset in units of axis
#' @param init_lbl_y_off Initial year label y offset in units of axis
#' @param final_lbl_x_off Final year label x offset in units of axis
#' @param final_lbl_y_off Final year label y offset in units of axis
#' @param detail_b40_outliers Shade the part of the plot less that B40,
#' plot the missing points at the lower 2.5% of the depletion distribution,
#' draw a box around those points and show a label with the description
#' @param detail_fspr_outliers Shade the part of the plot greater than
#' FSPR = 1, plot the missing points at the upper 2.5% of the FSPR
#' distribution, draw a box around those points and show a label with the
#' description
#' @param show_joint_prob_points Logical. If `TRUE`, show jittered points
#' near the X and Y points and a scatterplot in the upper left corner showing
#' where the joint probabilities are `TRUE`
#' @param joint_point_color If `show_joint_prob_points` is `TRUE`, this is
#' the color of those points
#'
#' @return A [ggplot2::ggplot()] object
#' @export
plot_phase <- function(model,
                       start_yr,
                       end_yr,
                       x_lim = c(0, 1.5),
                       y_lim = c(0, 1.2),
                       biomass_scale = 1e6,
                       init_lbl_x_off = 0,
                       init_lbl_y_off = 0,
                       final_lbl_x_off = 0,
                       final_lbl_y_off = 0,
                       cross_line_width = 1,
                       cross_line_color = "grey",
                       title_y_font_size = axis_title_font_size,
                       detail_b40_outliers = FALSE,
                       detail_fspr_outliers = FALSE,
                       show_joint_prob_points = FALSE,
                       joint_point_color = "purple"){

  yrs <- start_yr:end_yr

  extract_yrs <- function(param){
    tmp <- model$mcmccalcs[[param]]
    tmp <- tmp[grep("^[0-9]{4}$", names(tmp))]
    tmp[names(tmp) %in% yrs]
  }
  dlower <- extract_yrs("dlower")
  dmed <- extract_yrs("dmed")
  dupper <- extract_yrs("dupper")

  plower <- extract_yrs("plower")
  pmed <- extract_yrs("pmed")
  pupper <- extract_yrs("pupper")

  # Only include start year to end year (results contain extra calculations),
  # but don't need dmed[start_yr], since no pmed[start_yr-1] to plot it with,
  # or pmed[end_yr] since it's meaningless (even though SS calculates it)
  dmed <- dmed[(names(dmed) %in% yrs[-1])]
  dlower <- dlower[(names(dlower) %in% yrs[-1])]
  dupper <- dupper[(names(dupper) %in% yrs[-1])]
  pmed <- pmed[(names(pmed) %in% yrs[-length(yrs)])]
  plower <- plower[(names(plower) %in% yrs[-length(yrs)])]
  pupper <- pupper[(names(pupper) %in% yrs[-length(yrs)])]

  col_vec <- rev(rich_colors_short(n = length(dmed)))
  d <- tibble(dmed,
              pmed,
              col = col_vec)

  # Credible intervals for final year ----
  dmed_init <- dmed[names(dmed) %in% min(names(dmed))]
  dmed_final <- dmed[names(dmed) %in% max(names(dmed))]
  dlower_final <- dlower[names(dlower) %in% max(names(dlower))]
  dupper_final <- dupper[names(dupper) %in% max(names(dupper))]
  pmed_init <- pmed[names(pmed) %in% min(names(pmed))]
  pmed_final <- pmed[names(pmed) %in% max(names(pmed))]
  plower_final <- plower[names(plower) %in% max(names(plower))]
  pupper_final <- pupper[names(pupper) %in% max(names(pupper))]
  d_fin_unc <- tibble(x_med = dmed_final,
                      x = dlower_final,
                      xend = dupper_final,
                      y_med = pmed_final,
                      y = plower_final,
                      yend = pupper_final)

  # Points outside credible intervals for final year ----
  ssb <- get_post_cols(model$mcmc, "SSB", biomass_scale) |>
    select(as.character(end_yr))
  ssbo <- get_post_cols(model$mcmc, "SSB_Initial", biomass_scale, exact = TRUE)
  depl_df <- ssb |>
    bind_cols(ssbo) |>
    rename(biomass = as.character(end_yr),
           bo = SSB_Initial) |>
    mutate(depl = biomass / bo)
  depl <- depl_df |>
    pull(depl)
  depl_lowest <- depl[depl < dlower_final]
  depl_lowest_df <- tibble(dmed = depl_lowest,
                           pmed = rep(pmed_final, length(depl_lowest)))
  depl_highest <- depl[depl > dupper_final]
  depl_highest_df <- tibble(dmed = depl_highest,
                            pmed = rep(pmed_final, length(depl_highest)))
  spr_df <- get_post_cols(model$mcmc, "SPRratio") |>
    select(as.character(end_yr - 1))

  spr <-spr_df |>
    pull()
  spr_highest <- spr[spr > pupper_final]
  spr_lowest <- spr[spr <= plower_final]
  spr_highest_df <- tibble(dmed = rep(dmed_final, length(spr_highest)),
                           pmed = spr_highest)
  spr_lowest_df <- tibble(dmed = rep(dmed_final, length(spr_lowest)),
                          pmed = spr_lowest)

  # Plotting ----
  x_breaks <- seq(x_lim[1], x_lim[2], 0.2)
  y_breaks <- seq(y_lim[1], y_lim[2], 0.2)

  g <- ggplot(d,
              aes(x = dmed, y = pmed)) +
    geom_point(color = "black", size = 4) +
    geom_point(color = col_vec, size = 3) +
    theme(legend.position = "none") +
    coord_cartesian(xlim = x_lim,
                    ylim = y_lim) +
    ylim(y_lim) +
    geom_path(color = col_vec,
              linewidth = 0.5,
              arrow = arrow(length = unit(0.5, "cm"),
                            type = "open",
                            angle = 10)) +
    geom_hline(yintercept = 1.0,
               linetype = "dashed",
               color = "grey") +
    geom_vline(xintercept = c(0.1, 0.4, 1.0),
               linetype = "dashed",
               color = "grey") +
    geom_segment(data = d_fin_unc,
                 aes(x = x,
                     xend = xend,
                     y = y_med,
                     yend = y_med),
                 color = cross_line_color,
                 linewidth = cross_line_width,
                 lineend = "round") +
    geom_segment(data = d_fin_unc,
                 aes(x = x_med,
                     xend = x_med,
                     y = y,
                     yend = yend),
                 color = cross_line_color,
                 linewidth = cross_line_width,
                 lineend = "round") +
    scale_x_continuous(breaks = x_breaks,
                       labels = x_breaks,
                       expand = c(0, 0)) +
    scale_y_continuous(breaks = y_breaks,
                       labels = y_breaks,
                       expand = c(0, 0)) +
    annotate("text",
             x = dmed_init + init_lbl_x_off,
             y = pmed_init + init_lbl_y_off,
             label = min(names(dmed))) +
    annotate("text",
             x = dmed_final + final_lbl_x_off,
             y = pmed_final + final_lbl_y_off,
             label = max(names(dmed))) +
    # theme(# plot.margin: top, right,bottom, left
    #       # Needed to avoid tick labels cutting off
    #       plot.margin = margin(12, 12, 7, 0)) +
    xlab(expression(paste("Relative spawning biomass",
                          ~~B[t]/B[0]))) +
    ylab(expression(paste("Relative fishing intensity",
                          ~~(1-SPR[t-1])/(1-SPR['40%'])))) +
    theme(axis.title.y = element_text(size = title_y_font_size))

  shade_top_left <- !(detail_b40_outliers && detail_fspr_outliers)

  if(detail_b40_outliers){

    label_y_offset <- 0.1
    label_df <- tibble(x = 0.4,
                       y = pmed_final - label_y_offset)

    min_depl <- min(depl_lowest_df$dmed)
    max_depl <- max(depl_lowest_df$dmed)

    g <- g +
      geom_point(data = depl_lowest_df,
                 aes(x = dmed,
                     y = pmed),
                 color = "red") +
      geom_point(data = depl_highest_df,
                 aes(x = dmed,
                     y = pmed),
                 color = "red") +
      # Box around the depletion uncertainty below 40%
      geom_rect(data = depl_lowest_df,
                aes(xmin = min_depl - 0.05,
                    xmax = 0.4,
                    ymin = pmed - 0.05,
                    ymax = pmed + 0.05),
                color = "black",
                fill = "transparent") +
      geom_rect(aes(xmin = 0,
                    xmax = 0.4,
                    ymin = 0,
                    ymax = ifelse(shade_top_left,
                                  Inf,
                                  1)),
                fill = "grey20",
                alpha = 0.005) +
      geom_segment(aes(x = dlower_final,
                       xend = 0.4,
                       y = pmed_final,
                       yend = pmed_final),
                   size = 1.5,
                   color = "black",
                   lineend = "round",
                   alpha = 0.003) +
      geom_label(data = label_df,
                 aes(x = x,
                     y = y),
                 label = paste0("P(B[", end_yr, "] / B[0] < 0.4~B[0])"),
                 parse = TRUE,
                 hjust = 1,
                 size = 3,
                 inherit.aes = FALSE)
  }

  if(detail_fspr_outliers){

    min_fspr <- min(spr_highest_df$pmed)
    max_fspr <- max(spr_highest_df$pmed)
    spr_highest_med <- (min_fspr + max_fspr) / 2


    label_x_offset <- 0.07
    label_df <- tibble(x = dmed_final + label_x_offset,
                       y = spr_highest_med)



    g <- g +
      geom_point(data = spr_lowest_df,
                 aes(x = dmed,
                     y = pmed),
                 color = "red") +
      geom_point(data = spr_highest_df,
                 aes(x = dmed,
                     y = pmed),
                 color = "red") +
      # Box around the FSPR uncertainty above 1
      geom_rect(data = spr_highest_df,
                aes(xmin = dmed_final - 0.05,
                    xmax = dmed_final + 0.05,
                    ymin = 1,
                    ymax = max_fspr),
                color = "black",
                fill = "transparent") +
      geom_rect(aes(xmin = ifelse(shade_top_left,
                                  x_lim[1],
                                  0.4),
                    xmax = x_lim[2],
                    ymin = 1,
                    ymax = y_lim[2]),
                fill = "grey20",
                alpha = 0.005)


    g <- g +
      geom_label(data = label_df,
                 aes(x = x,
                     y = y),
                 label = paste0("P(Relative~fishing~intensity~at~end~of~",
                                last_data_yr,
                                " > 1)"),
                 parse = TRUE,
                 hjust = 0,
                 size = 3,
                 inherit.aes = FALSE)
  }

  # if(detail_b40_outliers && detail_fspr_outliers){
  #   g <- g +
  #     geom_rect(aes(xmin = x_lim[1],
  #                   xmax = 0.4,
  #                   ymin = 1,
  #                   ymax = y_lim[2]),
  #               fill = "white")
  # }

  if(show_joint_prob_points){
    # Plot joint probability points

    # Find posterior points both under B40 and over FSPR40=1
    wch_depl_lowest <- which(depl <= 0.4)
    wch_spr_highest <- which(spr >= 1)
    wch_joint <- intersect(wch_depl_lowest, wch_spr_highest)
    wch_joint_depl_df <- depl_df |>
      slice(wch_joint) |>
      select(depl) |>
      rename(dmed = depl) |>
      mutate(pmed = rep(pmed_final, length(wch_joint)))

    wch_joint_spr_df <- spr_df |>
      slice(wch_joint) |>
      rename(pmed = 1) |>
      mutate(dmed = rep(dmed_final, length(wch_joint)))

    both <- wch_joint_depl_df |>
      select(dmed) |>
      mutate(wch_joint_spr_df$pmed)
    names(both) <- c("dmed", "pmed")

    # Points that remain and are NOT jointly probable
    #spr_hi_not_joint_df <- anti_join(spr_highest_df, wch_joint_spr_df)
    #depl_lo_not_joint_df <- anti_join(depl_lowest_df, wch_joint_depl_df)

    g <- g +
      geom_point(data = wch_joint_depl_df,
                 aes(x = dmed,
                     y = pmed + 0.02),
                 color = joint_point_color) +
      geom_point(data = wch_joint_spr_df,
                 aes(x = dmed - 0.02,
                     y = pmed),
                 color = joint_point_color) +
      geom_point(data = both,
                 aes(x = dmed,
                     y = pmed),
                 color = joint_point_color)
  }
  g
}
