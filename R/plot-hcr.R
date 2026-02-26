#' Plot the effects of the harvest control rule on the MCMC output of a model.
#'
#' @details
#' Plots relative spawning biomass for a given `year` on the x-axis by default
#' HCR relative fishing intensity on the y-axis. Extracts all MCMC posteriors
#' from SS3 output, specifically from the column called `Bratio_<year>` and uses
#' those as the basis for the calculation to make th plot.
#'
#' @param model The SS model output as loaded by [create_rds_file()]
#' @param year The year to extract Bratio for from the SS3 MCMC output matrix
#' @param lrp Limit Reference Point
#' @param urp upper reference point, also called USR or Upper Stock Reference
#' @param background_alpha The opacity of the colored areas representing the
#' zones
#' @param show_labels If `TRUE`, show the labels specifying the number of
#' posteriors in each zone
#' @param point_size The size of the points shown. Smaller points will allow you
#' to see individual points, larger ones make it seem like a solid line
#' @param arrowhead_crit_x The x value for the head of the arrow for the
#' critical zone text label
#' @param arrowhead_crit_y The y value for the head of the arrow for the
#' critical zone text label
#' @param arrowhead_caut_x The x value for the head of the arrow for the
#' cautious zone text label
#' @param arrowhead_caut_y The y value for the head of the arrow for the
#' cautious zone text label
#' @param arrowhead_healthy_x The x value for the head of the arrow for the
#' healthy zone text label
#' @param arrowhead_healthy_y The y value for the head of the arrow for the
#' healthy zone text label
#' @param label_crit_x The x value for the label location for the
#' critical zone text label
#' @param label_crit_y The y value for the label location for the
#' critical zone text label
#' @param label_caut_x The x value for the label location for the
#' cautious zone text label
#' @param label_caut_y The y value for the label location for the
#' cautious zone text label
#' @param label_healthy_x The x value for the label location for the
#' healthy zone text label
#' @param label_healthy_y The y value for the label location for the
#' healthy zone text label
#' @param arrow_obj Definition of the arrow shape and size for the text label
#' arrows
#'
#' @returns a [ggplot2::ggplot()] object
#' @export
plot_hcr <- \(model,
              year,
              lrp = 0.1,
              urp = 0.4,
              background_alpha = 0.1,
              show_labels = TRUE,
              point_size = 0.25,
              arrowhead_crit_x = 0.05,
              arrowhead_crit_y = 0.1,
              arrowhead_caut_x = 0.3,
              arrowhead_caut_y = 0.5,
              arrowhead_healthy_x = 0.7,
              arrowhead_healthy_y = 0.8,
              label_crit_x = 1.5,
              label_crit_y = 0.1,
              label_caut_x = 1.3,
              label_caut_y = 0.1,
              label_healthy_x = 0.8,
              label_healthy_y = 0.1,
              arrow_obj = arrow(length = unit(0.5, "cm"),
                                angle = 20,
                                type = "closed")){

  depl <- model$mcmc[[paste0("Bratio_", year)]]

  crit_inds <- which(depl <= lrp)
  caut_inds <- which(depl > lrp & depl <= urp)
  healthy_inds <- which(depl > urp)

  d_crit <- depl[crit_inds]
  f_crit <- rep(0, length.out = length(d_crit))
  d_caut <- depl[caut_inds]
  f_caut <- 1 / (urp - lrp) * (d_caut - lrp)
  d_healthy <- depl[healthy_inds]
  f_healthy <- rep(1, length.out = length(d_healthy))

  # Order the points according to zone crit -> caut -> healthy
  crit_inds <- seq_along(d_crit)
  caut_inds <- seq_along(d_caut) + ifelse(length(crit_inds), max(crit_inds), 0)
  healthy_inds <- seq_along(d_healthy) + max(caut_inds)

  df_crit <- tibble(iter = crit_inds,
                    depl = d_crit,
                    f_mort = f_crit,
                    grp = "critical")
  df_caut <- tibble(iter = caut_inds,
                    depl = d_caut,
                    f_mort = f_caut,
                    grp = "cautious")
  df_healthy <- tibble(iter = healthy_inds,
                       depl = d_healthy,
                       f_mort = f_healthy,
                       grp = "healthy")

  d <- list(df_crit, df_caut,df_healthy) |>
    bind_rows()

  lab_crit_df <- tibble(x = arrowhead_crit_x,
                        y = arrowhead_crit_y,
                        text = paste0("Critical zone: ",
                                      nrow(df_crit),
                                      " posteriors"))
  lab_caut_df <- tibble(x = arrowhead_caut_x,
                        y = arrowhead_caut_y,
                        text = paste0("Cautious zone: ",
                                      nrow(df_caut),
                                      " posteriors"))
  lab_healthy_df <- tibble(x = arrowhead_healthy_x,
                           y = arrowhead_healthy_y,
                           text = paste0("Healthy zone: ",
                                         nrow(df_healthy),
                                         " posteriors"))

  g <- ggplot(d,
              aes(x = depl,
                  y = f_mort,
                  group  =grp)) +
    annotate("rect",
             xmin = 0, xmax = lrp - 0.002,
             ymin = 0, ymax = 1,
             alpha = background_alpha,
             fill = "red", color = "red") +
    annotate("rect",
             xmin = lrp + 0.002, xmax = urp - 0.002,
             ymin = 0, ymax = 1,
             alpha = background_alpha,
             fill = "orange", color = "orange") +
    annotate("rect",
             xmin = urp + 0.002, xmax = 3,
             ymin = 0, ymax = 1,
             alpha = background_alpha,
             fill = "green", color = "green") +
    geom_point(size = point_size) +
    xlab(paste0("Relative spawning biomass in ", year)) +
    ylab("Default HCR relative fishing intensity")

  if(show_labels){
    g <- g +
      geom_segment(data = lab_crit_df,
                   aes(x = x + label_crit_x,
                       y = y + label_crit_y,
                       xend = x,
                       yend = y),
                   inherit.aes = FALSE,
                   arrow = arrow_obj) +
      geom_label(data = lab_crit_df,
                 aes(x = x,
                     y = y,
                     label = text),
                 inherit.aes = FALSE,
                 nudge_x = label_crit_x,
                 nudge_y = label_crit_y,
                 fill = "white") +
      geom_segment(data = lab_caut_df,
                   aes(x = x + label_caut_x,
                       y = y + label_caut_y,
                       xend = x,
                       yend = y),
                   inherit.aes = FALSE,
                   arrow = arrow_obj) +
      geom_label(data = lab_caut_df,
                 aes(x = x,
                     y = y,
                     label = text),
                 inherit.aes = FALSE,
                 nudge_x = label_caut_x,
                 nudge_y = label_caut_y,
                 fill = "white") +
      geom_label(data = lab_healthy_df,
                 aes(x = x,
                     y = y,
                     label = text),
                 inherit.aes = FALSE,
                 nudge_x = label_healthy_x,
                 nudge_y = label_healthy_y,
                 fill = "white")
  }

  g
}
