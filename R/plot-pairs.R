#' Create a pairs plot showing correlations among key parameters or
#' recruitment deviations against the R0 parameter
#'
#' @param model The model output from Stock Synthesis as loaded by
#'   [create_rds_file()].
#' @param type Either "key" or "devs", which describe the type of pairs plot
#' to create. If this is "key", all parameters listed in the `key_posteriors`
#' argument will be plotted as well as the objective function and recruitment
#' parameters for years listed in `recr`. If this is "devs", The R0 parameter
#' will be plotted against the recruitment deviation parameters for the years
#' listed in `recr`
#' @param key_posteriors A list of key posterior names (as regular
#' expressions)
#' @param key_posteriors_titles  A list of key posterior titles (pretty
#' names which are meant to be shown on plots)
#' @param recr A vector of the recruitment parameter years to include.
#' @param bratio A vector of years to plot the relative spawning biomass for
#' @param forecatch A vector of years to plot the default harvest catch for.
#' There are forecasts, hence the name
#' @param point_color The color of the scatter plot points
#' @param point_alpha The transparency of the scatter plot points
#' @param point_size Th size of the scatter plot points
#' @param lm_color The color of the regression line found on the scatter plot
#' panels
#' @param lm_linetype The type of the regression line found on the scatter
#' plot panels
#' @param lm_linewidth The width of the regression line found on the scatter
#' plot panels
#' @param strip_fill_color The color to fill in the strip labels
#' (facet labels)
#' @param strip_fill_alpha The transparency for the strip labels
#' (facet labels)
#'
#' @return A [GGally::ggmatrix()] object
#' @export
plot_pairs <- function(model,
                       type = c("key", "devs"),
                       key_posteriors = NULL,
                       key_posteriors_titles = NULL,
                       recr = NULL,
                       bratio = NULL,
                       forecatch = NULL,
                       point_color = "royalblue",
                       point_alpha = 0.3,
                       point_size = 0.05,
                       lm_color = "black",
                       lm_linetype = "solid",
                       lm_linewidth = 1,
                       strip_fill_color = "transparent",
                       strip_fill_alpha = 0.3){

  type <- match.arg(type)

  key_posteriors <- tolower(key_posteriors)

  mc <- model$mcmc |>
    as_tibble() %>%
    set_names(tolower(names(.)))

  if(type == "key"){
    select_vec <- c("Objective_function", key_posteriors)
    labels_vec <- c("Objective function", key_posteriors_titles)
  }else if(type == "devs"){
    ro_ind <- grep("R\\[0", map_chr(key_posteriors_titles, ~.x))
    if(!length(ro_ind)){
      stop("Counld not find the R0 parameter in the list of key parameters")
    }
    select_vec <- key_posteriors[ro_ind]
    labels_vec <- key_posteriors_titles[ro_ind]
  }

  if(!is.null(recr[1])){
    mc_recr <- model$recruitpars |>
      as_tibble(rownames = "name") %>%
      set_names(tolower(names(.))) |>
      mutate(type = tolower(type)) |>
      mutate(name = tolower(name)) |>
      dplyr::filter(yr %in% recr)

    if(type == "key"){
      select_vec <- c(select_vec,
                      paste0("^recr_", recr, "$"))
      labels_vec <- c(labels_vec,
                      paste0("Recruitment ", recr))
    }else if(type == "devs"){
      select_vec <- c(select_vec,
                      mc_recr$name)
      labels_vec <- c(labels_vec,
                      paste0("Recruitment dev. ", recr))
    }
  }
  if(!is.null(bratio[1])){
    select_vec <- c(select_vec,
                    paste0("^bratio_", bratio, "$"))
    labels_vec <- c(labels_vec,
                    paste0("Relative spawn. biomass ", bratio))

  }

  if(!is.null(forecatch[1])){
    select_vec <- c(select_vec,
                    paste0("^forecatch_", forecatch, "$"))
    labels_vec <- c(labels_vec,
                    paste0("Default harvest ", forecatch))

  }

  # Modify strip labels so they actually fit
  labels_vec <- gsub("Objective function",
                     paste0("atop(scriptstyle('Objective'), ",
                     "scriptstyle('function'))"),
                     labels_vec)
  labels_vec <- gsub("Natural mortality",
                     "italic(M)",
                     labels_vec)
  labels_vec <- gsub("ln\\(R\\[0\\]\\)",
                     "ln(italic(R[0]))",
                     labels_vec)
  labels_vec <- gsub("Steepness",
                     "italic(h)",
                     labels_vec)
  labels_vec <- gsub("Survey extra SD",
                     paste0("atop(scriptstyle('Survey'), ",
                            "scriptstyle('extra SD'))"),
                     labels_vec)
  labels_vec <- gsub("Dirichlet-multinomial fishery",
                     "scriptstyle('DM~fish.')",
                     labels_vec)
  labels_vec <- gsub("Dirichlet-multinomial survey",
                     "scriptstyle('DM~surv.')",
                     labels_vec)
  labels_vec <- gsub("Recruitment +(\\d{4})",
                     paste0("atop(scriptstyle('Rec.'), ",
                            "scriptstyle('\\1'))"),
                     labels_vec)
  labels_vec <- gsub("Recruitment dev. +(\\d{4})",
                     paste0("atop(scriptstyle('Recdev.'), ",
                            "scriptstyle('\\1'))"),
                     labels_vec)
  labels_vec <- gsub("Relative spawn. biomass +(\\d{4})",
                     paste0("atop(scriptstyle('Rel.'), ",
                            "scriptstyle('SSB \\1'))"),
                     labels_vec)
  labels_vec <- gsub("Default harvest +(\\d{4})",
                     paste0("atop(scriptstyle('Def.'), ",
                            "scriptstyle('catch \\1'))"),
                     labels_vec)

  d <- mc |>
    select(matches(select_vec))

  # Plot for the panels in the upper triangle of the pairs plot
  # @param data The plot data (passed from [GGally::ggpairs()])
  # @param mapping The [ggplot2::aes()] aes mapping
  # (passed from [GGally::ggpairs()])
  # @param ... Other arguments passed from [GGally::ggpairs()]
  # @return
  upper_triangle <- function(data,
                             mapping,
                             color = I("grey50"),
                             sizeRange = c(2.5, 3.5),
                             ...) {

    # Get the x and y data to use the other code
    x <- unlist(data[quo_name(mapping$x) == names(data)])
    y <- unlist(data[quo_name(mapping$y) == names(data)])

    ct <- cor.test(x,y)

    r <- unname(ct$estimate)
    rt <- format(r, digits = 2)[1]

    # Since we can't print it to get the strsize, just use the max size range
    cex <- max(sizeRange)

    # Helper function to calculate a usable size
    percent_of_range <- function(percent, range) {
      percent * diff(range) + min(range, na.rm = TRUE)
    }

    # Plot the cor value
    ggally_text(label = as.character(rt),
                mapping = aes(),
                xP = 0.5,
                yP = 0.5,
                size = I(percent_of_range(cex * abs(r), sizeRange)),
                color = color,
                ...) +

      # Remove all the background stuff and wrap it with a dashed line
      theme_classic() +
      theme(panel.background = element_rect(color = color,
                                            linetype = "longdash"),
            axis.line = element_blank(),
            axis.ticks = element_blank(),
            axis.text.y = element_blank(),
            axis.text.x = element_blank())
  }

  # Plot for the panels in the lower triangle of the pairs plot
  # @param data The plot data (passed from [GGally::ggpairs()])
  # @param mapping The [ggplot2::aes()] aes mapping  (passed from [GGally::ggpairs()])
  # @param ... Other arguments passed from [GGally::ggpairs()]
  # @return
  lower_triangle <- function(data, mapping, ...){

    # Round the axis tick values on each panel of the pairs plot to two
    # decimals unless they are all integers, then show no decimal points
    # @param x A vector of axis tick labels to format
    # @return A formatted vector of strings
    round_axis_vals <- function(x){
      if(all(x[!is.na(x)] %% 1 == 0)){
        sprintf("%d", as.integer(x))
      }else{
        sprintf("%.2f", x)
      }
    }

    ggplot(data = data,
           mapping = mapping) +
      geom_point(color = point_color,
                 alpha = point_alpha,
                 size = point_size) +
      geom_smooth(method = "lm",
                  color = "white",
                  linetype = lm_linetype,
                  linewidth = lm_linewidth + 0.5 * lm_linewidth,
                  ...) +
      geom_smooth(method = "lm",
                  color = lm_color,
                  linetype = lm_linetype,
                  linewidth = lm_linewidth,
                  ...) +
      # scale_y_continuous(labels = round_axis_vals) +
      # scale_x_continuous(labels = round_axis_vals) +
      theme(axis.ticks = element_blank(),
            axis.text.y.left = element_blank(),
            axis.text.x.bottom = element_blank())
  }

  # Plot for the panels in the diagonals of the pairs plot
  # @param data The plot data (passed from [GGally::ggpairs()])
  # @param mapping The [ggplot2::aes()] aes mapping  (passed from [GGally::ggpairs()])
  # @param ... Other arguments passed from [GGally::ggpairs()]
  # @return
  diagonals <- function(data, mapping, ...){
    ggally_densityDiag(data, mapping) +
      theme(axis.ticks.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.x = element_blank(),
            axis.text.x = element_blank())
  }

  ind <- grep("Age 1 extra SD", labels_vec)
  if(length(ind)){
    labels_vec[ind] <- "atop(scriptstyle('Age 1'), scriptstyle('extra SD'))"
  }
  # The `labeller` cannot have spaces in the names, we have to use ~ instead
  labels_vec <- gsub(" +", "~", labels_vec)
  # Only some are scriptstyle (small text), make sure all are
  has_scriptstyle <- grep("scriptstyle", labels_vec)
  labels_vec[-has_scriptstyle] <- paste0("scriptstyle(",
                                         labels_vec[-has_scriptstyle],
                                         ")")
  names(d) <- labels_vec

  # For debugging, reduce number of columns to make it faster
  #d <- d[, 1:4]
  g <- ggpairs(d,
               labeller = label_parsed,
               upper = list(continuous = upper_triangle),
               diag = list(continuous = diagonals),
               lower = list(continuous = lower_triangle)) +
    # Rotate the right-hand parameter names so they are the same as the rest
    # and reduce the size and angle the x and y axis tick labels so they
    # are readable. Also, remove the border for the strip labels and color
    # the strip label background
    theme(strip.text.y.right = element_text(angle = 0),
          strip.background = element_rect(color = "white",
                                          fill = alpha(strip_fill_color,
                                                       strip_fill_alpha),
                                          size = 3))

  g
}