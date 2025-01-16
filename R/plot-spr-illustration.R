#' Create a three-panel plot illustrating how the SPR is calculated.
#'
#' @param model A model object, , created by [create_rds_file()]
#' @param yrs A vector of years to include in the calculation. If `NULL`,
#' the years found in the weight-at-age data frame (`model$wtatage`)
#' will be used
#' @param color The color to use for the bars
#' @param show_legend Logical. Whether or not to show the text 'legend'
#'
#' @return A [ggplot2::ggplot()] object
#' @export
plot_spr_illustration <- function(model,
                                  yrs = NULL,
                                  color = "royalblue",
                                  show_legend = TRUE){

  # Ages to show in the plot
  ages_chr <- grep("\\d+", names(model$ageselex), value = TRUE)
  if(!length(ages_chr)){
    stop("Problem with object `model$ageselex`, no ages were found as ",
         "column names in the data frame. Check to make sure they are ",
         "there and not prepended by some characters, e.g. 'a15'")
  }
  ages <- as.numeric(ages_chr)
  yrs <- yrs %||% unique(model$wtatage$year)

  # Average maturity * fecundity
  matfec_vec <- model$wtatage |>
    dplyr::filter(fleet == -2,
                  year %in% yrs) |>
    select(all_of(ages_chr)) |>
    summarize_all(mean) |>
    unlist()
  # Average weight at age
  meanwt_vec <- model$wtatage |>
    dplyr::filter(fleet == -1,
                  year %in% yrs) |>
    select(all_of(ages_chr)) |>
    summarize_all(mean) |>
    unlist()

  # numbers at age in equilibrium
  n_at_age_equil <- model$natage |>
    dplyr::filter(Era == "VIRG",
                  `Beg/Mid` == "B") |>
    select(all_of(ages_chr)) |>
    unlist()

  m_at_age_endyr <- model$Natural_Mortality |>
    dplyr::filter(Yr == model$endyr) |>
    select(all_of(ages_chr)) |>
    unlist()

  f_at_age_endyr <- model$fatage |>
    dplyr::filter(Yr == model$endyr) |>
    select(all_of(ages_chr)) |>
    unlist()

  z_at_age_endyr <- f_at_age_endyr + m_at_age_endyr

  # Calculate numbers-at-age with and without fishing based on 1 at age 0
  # This assumes the `ages` vector starts at zero
  n_at_age_m <- rep(1, length(ages))
  n_at_age_z <- rep(1, length(ages))
  for(iage in ages[-1]){
    n_at_age_m[iage + 1] <- n_at_age_m[iage] * exp(-m_at_age_endyr[iage])
    n_at_age_z[iage + 1] <- n_at_age_z[iage] * exp(-z_at_age_endyr[iage])
  }

  d <- tibble(age = ages,
              natage_m = n_at_age_m,
              natage_z = n_at_age_z,
              # Biomass-per-recruit unfished
              biomass_pr_unfished = n_at_age_m * meanwt_vec,
              # Biomass-per-recruit fished
              biomass_pr_fished = n_at_age_z * meanwt_vec,
              # Female spawning biomass per recruit unfished
              fsbiomass_pr_unfished = 0.5 * n_at_age_m * matfec_vec,
              # Female spawning biomass per recruit fished
              fsbiomass_pr_fished = 0.5 * n_at_age_z * matfec_vec)

  p <- list()
  p[[1]] <- ggplot(d) +
    geom_bar(aes(x = age,
                 y = natage_m),
             stat = "identity",
             fill = color,
             alpha = 0.5) +
    geom_bar(aes(x = age,
                 y = natage_z),
             stat = "identity",
             fill = color,
             alpha = 1) +
    xlab("") +
    ylab("") +
    ggtitle("Numbers per recruit") +
    theme(plot.margin = margin(c(6, 0, 0, 0))) +
    scale_y_continuous(labels = ~{f(.x, 2)})

  p[[2]] <- ggplot(d) +
    geom_bar(aes(x = age,
                 y = biomass_pr_unfished),
             stat = "identity",
             fill = color,
             alpha = 0.5) +
    geom_bar(aes(x = age,
                 y = biomass_pr_fished),
             stat = "identity",
             fill = color,
             alpha = 1) +
    xlab("") +
    ylab("") +
    ggtitle("Biomass per recruit") +
    theme(plot.margin = margin(c(0, 0, 0, 0))) +
    scale_y_continuous(labels = ~{f(.x, 2)})

  p[[3]] <- ggplot(d) +
    geom_bar(aes(x = age,
                 y = fsbiomass_pr_unfished),
             stat = "identity",
             fill = color,
             alpha = 0.5) +
    geom_bar(aes(x = age,
                 y = fsbiomass_pr_fished),
             stat = "identity",
             fill = color,
             alpha = 1) +
    xlab("Age") +
    ylab("") +
    ggtitle("Female spawning biomass per recruit") +
    theme(plot.margin = margin(c(0, 0, 6, 0))) +
    scale_y_continuous(labels = ~{f(.x, 2)})

  # calculate spawning potential with and without fishing and SPR
  spawn_potential_m <- sum(0.5 * n_at_age_m * matfec_vec)
  spawn_potential_z <- sum(0.5 * n_at_age_z * matfec_vec)
  sum_m <- sum(spawn_potential_m)
  sum_z <- sum(spawn_potential_z)
  spr <- sum(spawn_potential_z) / sum(spawn_potential_m)

  # format numbers for legend
  sum_m_txt <- f(sum_m, 2)
  sum_z_txt <- f(sum_z, 2)
  spr_txt <- f(spr, 2)
  intensity_txt <- f((1 - spr) / (1 - 0.4), 2)

  if(show_legend){
    p[[3]] <- p[[3]] +
      annotate("point",
               x = 7.5,
               y = 0.1,
               shape = 22,
               size = 6,
               fill = color,
               alpha = 0.5,
               color = "transparent") +
      annotate("point",
               x = 7.5,
               y = 0.09,
               shape = 22,
               size = 6,
               fill = color,
               alpha = 1,
               color = "transparent") +
      annotate("text",
               x = 10,
               y = 0.1,
               size = 4,
               label = paste0("Total = ",
                              sum_m_txt)) +
      annotate("text",
               x = 10,
               y = 0.09,
               size = 4,
               label = paste0("Total = ",
                              sum_z_txt)) +
      annotate("text",
               x = 12.5,
               y = 0.1,
               size = 4,
               hjust = 0,
               label = paste0("SPR = ",
                              sum_z_txt,
                              " / ",
                              sum_m_txt,
                              " = ",
                              spr_txt)) +
      annotate("text",
               x = 12.5,
               y = 0.08,
               size = 4,
               hjust = 0,
               label = "Rel. Fishing intensity =") +
      annotate("text",
               x = 12.5,
               y = 0.07,
               size = 4,
               hjust = 0,
               label = "(1 - SPR) / (1 - 0.40) =") +
    annotate("text",
             x = 12.5,
             y = 0.06,
             size = 4,
             hjust = 0,
             label = paste0("(1 - ",
                            spr_txt,
                            ") / (1 - 0.40) = ",
                            intensity_txt))
  }

  plot_grid(plotlist = p, ncol = 1, align = "v")
}
