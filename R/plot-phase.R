#' Create plot of the relative fishing intensity in year t-1 against relative
#' spawning biomass in year t, as a historical look at the fishery for the
#' MCMC given by model
#'
#' @param model A model, created by [create_rds_file()]
#' @param start_yr The minimum year in the plot
#' @param end_yr The maximum year in the plot
#' @param x_lim Vector of two values for minimum and maximum x-axis values
#' @param y_lim Vector of two values for minimum and maximum y-axis values
#' @param init_lbl_x_off Initial year label x offset in units of axis
#' @param init_lbl_y_off Initial year label y offset in units of axis
#' @param final_lbl_x_off Final year label x offset in units of axis
#' @param final_lbl_y_off Final year label y offset in units of axis
#'
#' @return A [ggplot2::ggplot()] object
#' @export
plot_phase <- function(model,
                       start_yr,
                       end_yr,
                       x_lim = c(0, 1.4),
                       y_lim = c(0, 1.2),
                       init_lbl_x_off = 0,
                       init_lbl_y_off = 0,
                       final_lbl_x_off = 0,
                       final_lbl_y_off = 0,
                       title_y_font_size = axis_title_font_size){

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

  # Credible intervals for final year:
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
                 color = "grey",
                 linewidth = 1,
                 lineend = "round") +
    geom_segment(data = d_fin_unc,
                 aes(x = x_med,
                     xend = x_med,
                     y = y,
                     yend = yend),
                 color = "grey",
                 linewidth = 1,
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

  g
}
