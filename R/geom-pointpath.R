unique0 <- ggplot2:::unique0

#' Key glyph for legends including [geom_pointpath()]
#'
#' Each geom has an associated function that draws the key when the geom needs
#' to be displayed in a legend. These functions are called `draw_key_*()`, where
#' `*` stands for the name of the respective key glyph. The key glyphs can be
#' customized for individual geoms by providing a geom with the `key_glyph`
#' argument (see [`layer()`] or examples below.)
#'
#' @return A grid grob.
#' @param data A single row data frame containing the scaled aesthetics to
#'   display in this key
#' @param params A list of additional parameters supplied to the geom.
#' @param size Width and height of key in mm.
#' @export
draw_key_pointpath <- function(data, params, size) {

  if(is.null(data$linetype)){
    data$linetype <- 0
  }else{
    data$linetype[is.na(data$linetype)] <- 0
  }

  segmentsGrob(
    0.1, 0.5, 0.9, 0.5,
    gp = gpar(
      col = alpha(data$colour %||% data$fill %||% "black", data$alpha),
      fill = alpha(params$arrow.fill %||% data$colour
                   %||% data$fill %||% "black", data$alpha),
      lwd = (data$linewidth %||% 0.5) * .pt,
      lty = data$linetype %||% 1,
      lineend = params$lineend %||% "butt"
    ),
    arrow = params$arrow)
}

#' Connect observations with lines and points
#'
#' `geom_pointpath()` connects the observations in the order in which they
#' appear in the data.
#' @param lineend Line end style (round, butt, square).
#' @param linejoin Line join style (round, mitre, bevel).
#' @param linemitre Line mitre limit (number greater than 1).
#' @param arrow Arrow specification, as created by [grid::arrow()].
#' @section Missing value handling:
#' `geom_pointpath()` handle `NA` in the same way as [ggplot2::geom_point()]
#' and [ggplot2::geom_line()]
#' @export
geom_pointpath <- function(
    mapping = NULL,
    data = NULL,
    stat = "identity",
    position = "identity",
    ...,
    lineend = "butt",
    linejoin = "round",
    linemitre = 10,
    arrow = NULL,
    na.rm = FALSE,
    show.legend = NA,
    inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomPointPath,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = rlang::list2(
      lineend = lineend,
      linejoin = linejoin,
      linemitre = linemitre,
      arrow = arrow,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomPointPath <- ggproto(
  "GeomPointPath", Geom,
  required_aes = c("x", "y"),

  default_aes = aes(colour = "black", linewidth = 0.5, linetype = 1, alpha = NA,
                    shape = 19, size = 1.5, fill = NA, stroke = 0.5),

  handle_na = function(self, data, params) {
    # Drop missing values at the start or end of a line - can't drop in the
    # middle since you expect those to be shown by a break in the line
    complete <- stats::complete.cases(data[names(data) %in%
                              c("x", "y", "linewidth", "colour", "linetype")])
    kept <- stats::ave(complete, data$group, FUN = ggplot2:::keep_mid_true)
    data <- data[kept, ]

    if (!all(kept) && !params$na.rm) {
      cli::cli_warn(paste0(
        "Removed {sum(!kept)} row{?s} containing missing values or values ",
        "outside the scale range ({.fn {snake_class(self)}})."
      ))
    }

    data
  },

  draw_panel = function(self, data, panel_params, coord, arrow = NULL,
                        lineend = "butt", linejoin = "round", linemitre = 10,
                        na.rm = FALSE) {

    coords <- coord$transform(data, panel_params)
    stroke_size <- coords$stroke
    stroke_size[is.na(stroke_size)] <- 0

    if (is.character(data$shape)) {
      data$shape <- ggplot2:::translate_shape_string(data$shape)
    }

    data <- ggplot2:::check_linewidth(data, snake_class(self))
    if (!anyDuplicated(data$group)) {
      cli::cli_inform(c(
        "{.fn {snake_class(self)}}: Each group consists of only one observation.",
        i = "Do you need to adjust the {.field group} aesthetic?"
      ))
    }

    # must be sorted on group
    data <- data[order(data$group), , drop = FALSE]
    munched <- coord_munch(coord, data, panel_params)

    # Silently drop lines with less than two points, preserving order
    rows <- stats::ave(seq_len(nrow(munched)), munched$group, FUN = length)
    munched <- munched[rows >= 2, ]
    if (nrow(munched) < 2) return(zeroGrob())

    # Work out whether we should use lines or segments
    attr <- ggplot2:::dapply(munched, "group", function(df) {
      linetype <- unique0(df$linetype)
      ggplot2:::data_frame0(
        solid = identical(linetype, 1) || identical(linetype, "solid"),
        constant = nrow(unique0(df[, names(df) %in% c("alpha",
                                                      "colour",
                                                      "linewidth",
                                                      "linetype")])) == 1,
        .size = 1
      )
    })
    solid_lines <- all(attr$solid)
    constant <- all(attr$constant)
    if (!solid_lines && !constant) {
      cli::cli_abort(
        "{.fn {snake_class(self)}} can't have varying {.field colour}, {.field linewidth}, and/or {.field alpha} along the line when {.field linetype} isn't solid")
    }

    # Work out grouping variables for grobs
    n <- nrow(munched)
    group_diff <- munched$group[-1] != munched$group[-n]
    start <- c(TRUE, group_diff)
    end <-   c(group_diff, TRUE)

    if (!constant) {

      arrow <- repair_segment_arrow(arrow, munched$group)

      segmentsGrob(
        munched$x[!end], munched$y[!end], munched$x[!start], munched$y[!start],
        default.units = "native", arrow = arrow,
        gp = gpar(
          col = alpha(munched$colour, munched$alpha)[!end],
          fill = alpha(munched$colour, munched$alpha)[!end],
          lwd = munched$linewidth[!end] * .pt,
          lty = munched$linetype[!end],
          lineend = lineend,
          linejoin = linejoin,
          linemitre = linemitre
        )
      )
    } else {
      id <- match(munched$group, unique0(munched$group))

      lg <- polylineGrob(
        munched$x, munched$y, id = id,
        default.units = "native", arrow = arrow,
        gp = gpar(
          col = alpha(munched$colour, munched$alpha)[start],
          fill = alpha(munched$colour, munched$alpha)[start],
          lwd = munched$linewidth[start] * .pt,
          lty = munched$linetype[start],
          lineend = lineend,
          linejoin = linejoin,
          linemitre = linemitre
        )
      )
      pg <- pointsGrob(
        coords$x, coords$y,
        pch = coords$shape,
        gp = gpar(
          col = alpha(coords$colour, coords$alpha),
          fill = alpha(coords$fill, coords$alpha),
          # Stroke is added around the outside of the point
          fontsize = coords$size * .pt + stroke_size * .stroke / 2,
          lwd = coords$stroke * .stroke / 2
        )
      )
      ggplot2:::ggname("geom_pointpath", grobTree(lg, pg))
    }
  },

  draw_key = draw_key_pointpath,

  rename_size = TRUE
)

repair_segment_arrow <- function(arrow, group) {
  # Early exit if there is no arrow
  if(is.null(arrow)){
    return(arrow)
  }

  # Get group parameters
  rle <- vctrs::vec_group_rle(group) # handles NAs better than base::rle()
  n_groups <- length(rle)
  rle_len <- field(rle, "length") - 1 # segments have 1 member less than lines
  rle_end <- cumsum(rle_len)
  rle_start <- rle_end - rle_len + 1

  # Recycle ends and lengths
  ends <- rep(rep(arrow$ends,   length.out = n_groups), rle_len)
  len <- rep(rep(arrow$length, length.out = n_groups), rle_len)

  # Repair ends
  # Convert 'both' ends to first/last in multi-member groups
  is_both <- which(ends == 3)
  ends[setdiff(intersect(rle_start, is_both), rle_end)] <- 1L
  ends[setdiff(intersect(rle_end, is_both), rle_start)] <- 2L
  arrow$ends <- ends

  # Repair lengths
  zero <- unit(0, "mm")
  # Set length of first segment to zero when ends is 'last'
  len[intersect(setdiff(rle_start, rle_end), which(ends == 2))] <- zero
  # Set length of last segment to zero when ends is 'first'
  len[intersect(setdiff(rle_end, rle_start), which(ends == 1))] <- zero
  # Set length of middle pieces to zero
  len[setdiff(seq_along(len), c(rle_start, rle_end))] <- zero
  arrow$length <- len

  arrow
}
