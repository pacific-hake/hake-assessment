#' Turn a long data frame of weight-at-age data into a wide data frame by age
#'
#' Take the long-format, which is indicative of raw data, weight-at-age data
#' and make a wide data frame based on ages such that the data frame can be
#' written for Stock Synthesis.
#'
#' @param dat A data frame created from [weight_at_age_read()] or one that has the
#'   following column names:
#'   * Source,
#'   * Weight_kg,
#'   * Sex,
#'   * Age_yrs,
#'   * Length_cm,
#'   * Month, and
#'   * Year.
#' @param value A character value specifying which type of data you are
#'   interested in. The options include `"weight"`, `"length"`, and `"count"`,
#'   where the latter will supply the sample size.
#' @param maxage The age of the plus-group bin. The default is 15 years old.
#'   This age and all older ages will be combined into a single age bin..
#'
#' @export
#' @author Ian G. Taylor
#' @seealso [weight_at_age_read()]
#' @return A data frame with the first six columns pertaining to metadata,
#' i.e., #year, seas, sex, GP, bseas, and fleet, and additional columns
#' pertaining to each age starting with age zero up to the maximum age
#' supplied with the argument `maxage`. Ages for which there were no
#' samples are filled with `NA`. If \code{value = "count"}, then
#' the data frame will return sample sizes for each age rather than mean
#' weight- or length-at-age.
#'

weight_at_age_wide <- function(dat,
                               value = c("weight", "length", "count"),
                               maxage = 15) {
  value <- match.arg(value)
  agebinspop <- 0:maxage
  fleet_names <- c(
    "Acoustic Poland",
    "US_FOREIGN", "US_foreign",
    "US_JV",
    "ATSEA", "US_atsea",
    "US_shore", "SHORE",
    "",
    "CAN_freezer",
    "CAN_JV",
    "CAN_shoreside",
    "CAN_domestic",
    "CAN_polish",
    "Acoustic U.S.", "US_acoustic",
    "Acoustic Canada", "CAN_acoustic",
    "U.S. Acoustic", "Canada Acoustic"
  )

  dat_filtered <- dat %>%
    dplyr::mutate(
      Fleet = 1,
      # I wonder if I should use the maximum population age b/c then the data would
      # be more correct, albeit more sparse in those bins, check the sample sizes
      Age_yrs = ifelse(Age_yrs <= maxage, Age_yrs, 15)
    ) %>%
    # dplyr::filter(!outlier, !is.na(Weight_kg), !is.na(Age_yrs)) %>%
    dplyr::filter(!is.na(Weight_kg), !is.na(Age_yrs)) %>%
    dplyr::group_by(Year, Age_yrs, Fleet)

  if (length(unique(dat$Source[which(!dat$Source %in% fleet_names)])) > 0) {
    cli::cli_warn(
      "'Source' contains entries other than:
      {glue::double_quote(fleet_names)}."
    )
  }

  if (value == "weight") {
    dat_grouped <- dat_filtered %>%
      dplyr::summarise(
        to_summarize = mean(Weight_kg)
      )
  }
  if (value == "length") {
    dat_grouped <- dat_filtered %>%
      dplyr::summarise(
        to_summarize = mean(Length_cm)
      )
  }
  if (value == "count") {
    dat_grouped <- dat_filtered %>%
      dplyr::summarise(
        to_summarize = dplyr::n()
      )
  }

  weight_at_age <- dat_grouped %>%
    # tidyr::complete(Fleet = -2:3) %>%
    tidyr::pivot_wider(
      values_from = to_summarize,
      names_from = Age_yrs,
      names_prefix = "a"
    ) %>%
    dplyr::relocate(a0, .before = a1) %>%
    dplyr::full_join(
      x = data.frame(
        Year = unique(.[["Year"]]),
        seas = 1,
        gender = 1,
        GP = 1,
        bseas = 1
      ),
      y = .,
      by = c("Year")
    ) %>%
    dplyr::rename("#Yr" = Year)
  return(weight_at_age)
}

fill_wtage_matrix <- function(wtage, option = c("row", "age")) {
  # fills in NA values with average of adjacent years
  option <- paste0("i", match.arg(option, several.ok = FALSE))
  if (!"Note" %in% colnames(wtage)) wtage$Note <- ""
  nages <- ncol(wtage) - ifelse("Note" %in% colnames(wtage), 1, 0) - 6
  for (irow in 1:nrow(wtage)) {
    isNA <- (1:nages)[is.na(wtage[irow, -(1:6)])]
    if (length(isNA) > 0) {
      wtage$Note[irow] <- paste(
        wtage$Note[irow],
        "# interpolated ages", paste(isNA - 1, collapse = ",")
      )
      for (iage in isNA) {
        if (get(option) > 1) {
          if (option == "irow") earliervals <- wtage[1:(irow - 1), iage + 6]
          if (option == "iage") earliervals <- wtage[irow, (1:(iage - 1)) + 6]
        } else {
          earliervals <- NA
        }
        if (get(option) < ifelse(option == "irow", nrow(wtage), nages)) {
          if (option == "irow") latervals <- wtage[(irow + 1):nrow(wtage), iage + 6]
          if (option == "iage") latervals <- wtage[irow, ((iage + 1):nages) + 6]
        } else {
          latervals <- NA
        }
        lastearlier <- rev(earliervals[!is.na(earliervals)])[1]
        firstlater <- latervals[!is.na(latervals)][1]
        if (is.na(lastearlier)) lastearlier <- firstlater
        if (is.na(firstlater)) firstlater <- lastearlier
        wtage[irow, iage + 6] <- mean(lastearlier, firstlater, na.rm = TRUE)
      }
    }
  }
  return(wtage)
}

rich.colors.short <- function(n, alpha = 1) {
  x <- seq(0, 1, length = n)
  r <- 1 / (1 + exp(20 - 35 * x))
  g <- pmin(pmax(0, -0.8 + 6 * x - 5 * x^2), 1)
  b <- dnorm(x, 0.25, 0.15) / max(dnorm(x, 0.25, 0.15))
  rgb.m <- matrix(c(r, g, b), ncol = 3)
  rich.vector <- apply(rgb.m, 1, function(v) rgb(v[1], v[2], v[3], alpha = alpha))
}

# need to update the yrvec here because not an argument in makewtatagaeplots function
makeimage <- function(agevec = 0:15, yrvec = 1975:2017,
                      mat, # matrix of values by age and year
                      meanvec = NULL, # vector of mean value by age across years
                      Ntext = FALSE, # switch to have text show sample size rather than value
                      Nsamp.mat = NULL, # matrix of sample sizes
                      Nsamp.meanvec = NULL, # sum of sample sizes by age across years
                      interpmat = NULL, # matrix indicating which values are interpolations
                      main = "", # title for plot
                      dofont = TRUE, # use bold font to indicate interpolation
                      dorect = FALSE, # use shaded rectangles to indicate interpolation
                      margins = c(4.2, 4.2, 4, 1) + .1) {
  # if meanvec is not a separate input, assume that it's the first row
  if (is.null(meanvec)) {
    meanvec <- mat[, 1]
    mat <- mat[, -1]
    # same calculation for the sample sizes
    # (need to add them as an input)
    if (Ntext) {
      Nsamp.meanvec <- Nsamp.mat[, 1]
      Nsamp.mat <- Nsamp.mat[, -1]
    }
  }
  par(mar = margins)
  yrvec2 <- c(1973, 1974, yrvec)
  mat2 <- cbind(meanvec, NA, mat)
  if (Ntext) {
    Nsamp.mat2 <- cbind(Nsamp.meanvec, NA, Nsamp.mat)
  }
  if (max(mat, na.rm = TRUE) < 4) { # assume weights
    breaks <- seq(0, 4, length = 51)
    digits <- 2 # round weights to 2 digits
  } else { # assume length
    breaks <- seq(10, 80, length = 51)
    digits <- 1 # round lengths to 1 digit
  }
  image(
    x = 0:15, y = yrvec2, z = mat2, axes = FALSE, xlab = "Age", ylab = "Year",
    col = rainbow(60)[1:50], main = main, breaks = breaks
  )
  # add text
  zdataframe <- expand.grid(yr = yrvec2, age = agevec)
  zdataframe$z <- c(t(mat2))
  if (Ntext) {
    zdataframe$Nsamp <- c(t(Nsamp.mat2))
  }
  if (!is.null(interpmat)) {
    interpmat2 <- cbind(meanvec, NA, interpmat)
    zdataframe$interp <- c(t(interpmat2))
  } else {
    zdataframe$interp <- 0
  }
  zdataframe$font <- 1
  if (dofont) zdataframe$font <- ifelse(is.na(zdataframe$interp), 2, 1)

  if (!Ntext) {
    ztext <- format(round(zdataframe$z, digits))
    ztext[ztext == "  NA"] <- ""
    ztext[ztext == "   NA"] <- ""
    text(x = zdataframe$age, y = zdataframe$yr, label = ztext, font = zdataframe$font, cex = .7)
  } else {
    ztext <- zdataframe$Nsamp
    text(x = zdataframe$age, y = zdataframe$yr, label = ztext, font = zdataframe$font, cex = .7)
  }
  interp <- zdataframe[is.na(zdataframe$interp) & zdataframe$yr != 1974, ]
  if (dorect) {
    rect(interp$age - .5, interp$yr - .5,
      interp$age + .5, interp$yr + .5,
      col = rgb(0, 0, 0, .3), density = 20
    )
  }
  # finish plot
  axis(1, at = 0:15, cex.axis = .7)
  axis(2,
    at = c(1973, yrvec),
    labels = c("mean", yrvec), las = 1, cex.axis = .7
  )
}

dointerpSimple <- function(df, skipcols = 1:6) {
  cols <- setdiff(1:ncol(df), skipcols)
  n <- nrow(df)
  for (icol in cols) {
    df[, icol] <- approx(x = 1:n, xout = 1:n, y = df[, icol])$y
  }
  return(df)
}

#' Write Weight-At-Age Plots to Disk
#'
#' todo: write a description for make_wtatage_plots
#'
#' @param plots A vector of digits that specifies which plots to create.
#' @param data A data frame of weight at age data generated from
#' \code{weight_at_age_wide(..., getmean = TRUE)}.
#' @param counts A data frame of weight at age counts generated from
#' \code{weight_at_age_wide(..., getmean = TRUE, value = "count")}.
#' @param lengths A data frame of weight at length generated from
#' \code{weight_at_age_wide(..., getmean = TRUE, value = "length")}.
#' The default for this argument is `NULL`, and the function can be ran
#' without supplying length data.
#' @param dir The directory that you want to save the plots in. The default
#' is to save the plots in your current working directory.
#' @param year A character value giving the current year that will be used in
#' the file names for each plot.
#' @param maxage The maximum age of fish modelled in the stock assessment, i.e.,
#' what is the age at which all fish are grouped into a plus group.
#'
#' @export
#' @import grDevices
#' @author Ian G. Taylor
#'
make_wtatage_plots <- function(plots = 1:6, data, counts, lengths = NULL,
                               dir = getwd(), year = format(Sys.Date(), "%Y"), maxage = 15) {
  # make plots
  # plot of all data with mean

  on.exit(grDevices::graphics.off())
  data <- data[, !grepl("Note", colnames(data), ignore.case = TRUE)]
  agecols <- grep(paste0("^a", 0:maxage, collapse = "|"), colnames(data))
  meanvec <- as.numeric(data[1, agecols])
  mat <- t(as.matrix(data[-1, agecols]))
  wt1 <- dointerpSimple(data[-1, ])
  temp <- fill_wtage_matrix(wt1[, agecols])
  mat2 <- t(as.matrix(temp[, !grepl("Note", colnames(temp))]))
  rm(temp)
  Nsamp.meanvec <- as.numeric(counts[1, agecols])
  Nsamp.mat <- t(as.matrix(counts[-1, agecols]))

  # plot without extrapolation
  if (1 %in% plots) {
    fileplot <- file.path(
      dir,
      paste0("empirical_wtatage_", year, "_alldata_1_nointerp.png")
    )
    grDevices::png(fileplot, width = 7, height = 9, units = "in", res = 400)
    makeimage(
      mat = mat, meanvec = meanvec, main = "Mean weight at age (all data)",
      yrvec = (year - ncol(mat) + 1):year
    )
    dev.off()
    # plot showing sample sizes
    fileplot <- file.path(
      dir,
      paste0("empirical_wtatage_", year, "_alldata_1B_nointerp_numbers.png")
    )
    grDevices::png(fileplot, width = 7, height = 9, units = "in", res = 400)
    makeimage(
      mat = mat, meanvec = meanvec, Ntext = TRUE,
      Nsamp.meanvec = Nsamp.meanvec, Nsamp.mat = Nsamp.mat,
      main = "Mean weight at age (colors) with sample sizes (numbers)",
      yrvec = (year - ncol(mat) + 1):year
    )
    dev.off()
  }

  if (2 %in% plots) {
    mat1 <- t(as.matrix(wt1[, agecols]))
    fileplot <- file.path(
      dir,
      paste0("empirical_wtatage_", year, "_alldata_2_interp.png")
    )
    grDevices::png(fileplot, width = 7, height = 9, units = "in", res = 400)
    makeimage(
      mat = mat1, meanvec = meanvec,
      main = "Mean weight at age with interpolation (all data)",
      yrvec = (year - ncol(mat1) + 1):year
    )
    dev.off()
  }

  if (3 %in% plots) {
    fileplot <- file.path(
      dir,
      paste0("empirical_wtatage_", year, "_alldata_3_interp_extrap.png")
    )
    grDevices::png(fileplot, width = 7, height = 9, units = "in", res = 400)
    makeimage(
      mat = mat2, meanvec = meanvec,
      main = "Mean weight at age with interpolation & extrapolation (all data)",
      yrvec = (year - ncol(mat2) + 1):year
    )
    dev.off()
  }

  if (4 %in% plots) {
    fileplot <- file.path(
      dir,
      paste0("empirical_wtatage_", year, "_alldata_4_interp_extrap_shade.png")
    )
    grDevices::png(fileplot, width = 7, height = 9, units = "in", res = 400)
    makeimage(
      mat = mat2, interpmat = mat, dofont = FALSE, dorect = TRUE,
      meanvec = meanvec,
      main = "Mean weight at age with interpolation & extrapolation (all data)",
      yrvec = (year - ncol(mat2) + 1):year
    )
    dev.off()
  }

  if (5 %in% plots) {
    fileplot <- file.path(
      dir,
      paste0("empirical_wtatage_", year, "_alldata_5_interp_extrap_bold.png")
    )
    grDevices::png(fileplot, width = 7, height = 9, units = "in", res = 400)
    makeimage(
      mat = mat2, interpmat = mat, dofont = TRUE, dorect = FALSE,
      meanvec = meanvec,
      main = "Mean weight at age with interpolation & extrapolation (all data)",
      yrvec = (year - ncol(mat2) + 1):year
    )
    dev.off()
  }

  # Mean length plots
  if (6 %in% plots & !is.null(lengths)) {
    fileplot <- file.path(
      dir,
      paste0("empirical_lenatage_", year, "_alldata_6_nointerp.png")
    )
    len.meanvec <- as.numeric(lengths[1, agecols])
    len.mat <- t(as.matrix(lengths[-1, agecols]))

    grDevices::png(fileplot, width = 7, height = 9, units = "in", res = 400)
    makeimage(
      mat = len.mat, meanvec = len.meanvec,
      main = "Mean length at age (all data, cm)",
      yrvec = (year - ncol(len.mat) + 1):year
    )
    dev.off()

    fileplot <- file.path(
      dir,
      paste0("empirical_lenatage_", year, "_6B_nointerp_numbers.png")
    )
    grDevices::png(fileplot, width = 7, height = 9, units = "in", res = 400)
    makeimage(
      mat = len.mat, meanvec = len.meanvec, Ntext = TRUE,
      Nsamp.meanvec = Nsamp.meanvec, Nsamp.mat = Nsamp.mat,
      main = "Mean length at age (colors) with sample sizes (numbers)",
      yrvec = (year - ncol(len.mat) + 1):year
    )
    dev.off()
  }
}

#' Write weight-at-age file
#'
#' @param file A filename that you want to save the information as. The default
#'   is to have an extension of \code{.ss} such that the file can be used for
#'   Stock Synthesis. The file path can either be relative or absolute.
#' @param data Weight at age matrix.
#' @param maturity A vector of maturity at age.
#' @param n_fleets An integer specifying the number of fleets that are in the
#'   model. The default is two, e.g., one fishery and one survey. When the
#'   age-1 index is also included in the model, then this value should be
#'   changed to 3, e.g., one fishery, age 2+ survey, and relative age-1 index
#'   of abundance.
#'
write_wtatage_file <- function(
  file = paste0("wtatage_", format(Sys.time(), "%d-%b-%Y_%H.%M"), ".ss"),
  data,
  maturity,
  n_fleets = 2
) {
  stopifnot(is.vector(maturity))

  # Ensure column name that matters is lowercase
  colnames(data)[grep("fleet", ignore.case = TRUE, colnames(data))] <- "fleet"

  # stuff copied from SS_writedat for printing tables
  on.exit({
    if (sink.number() > 0) sink()
  })

  printdf <- function(dataframe) {
    # function to print data frame with hash mark before first column name
    names(dataframe)[1] <- paste("#_", names(dataframe)[1], sep = "")
    print(dataframe, row.names = FALSE, strip.white = TRUE)
  }

  oldwidth <- options()$width
  oldmax.print <- options()$max.print
  on.exit(options(width = oldwidth), add = TRUE)
  on.exit(options(max.print = oldmax.print), add = TRUE)
  options(width = 5000, max.print = 9999999)

  # Remove the file if it exists because you do not want to append it
  if (fs::file_exists(file)) {
    fs::file_delete(file)
  }
  zz <- file(file, open = "at")
  on.exit(close(zz), add = TRUE)
  sink(zz)

  nrows_per_matrix <- nrow(data)
  nrows_total <- 1 + 4 * nrows_per_matrix

  header <- c(
    "# empirical weight-at-age Stock Synthesis input file for hake",
    "# created by code in the R script: wtatage_calculations.R",
    paste("# creation date:", Sys.time()),
    "###################################################",
    "20 # Maximum age",
    "",
    "#Maturity x Fecundity: Fleet = -2 (Values maturity unchanged from 2012 Stock Assessment)",
    "#Maturity x Fecundity: Fleet = -2 (are maturity * wtatage)",
    ""
  )
  writeLines(header)

  # Make fleet -2 for most recent years
  fleetn2 <- cbind(
    data[, c(1:6)],
    t(apply((data[, -c(1:6)]), 1, function(x) x * maturity))
  )
  fleetn2$fleet <- -2
  printdf(fleetn2)

  writeLines("#All matrices below use the same values, pooled across all data sources")

  for (ifleet in -1:n_fleets) {
    data$fleet <- ifleet
    if (ifleet == -1) note <- "#Weight at age for population in middle of the year: Fleet = -1"
    if (ifleet == 0) note <- "#Weight at age for population at beginning of the year: Fleet = 0"
    if (ifleet > 0) {
      note <- glue::glue(
        "#Weight at age for {ifelse(ifleet == 1, 'Fishery', 'Survey')}: Fleet = {ifleet}"
      )
    }

    writeLines(c("", note))
    printdf(data)
  }

  # terminator line
  terminator <- 0 * fleetn2[1, ]
  terminator[, 1] <- -9999
  terminator[, "fleet"] <- 2
  writeLines("")
  writeLines("# terminator line")
  printdf(terminator)

  writeLines("# End of wtatage.ss file")
}

#' Identify weight-at-age outliers
#'
#' Get weight-at-age data and identify any outliers using three screening
#' methods.
#' * Data in September of 2003 are screened for fish less than 0.52 kg and
#'   greater than 45 cm in length.
#' * Data are screen for a weight-length relationship beyond that deemed to be
#'   normal, i.e., 20e-6*length^3 and 2e-6*length^3.
#' * Identify fish with a recorded age of greater than or equal to 99, which
#'   is sometimes used for data that should be flagged or not used instead of
#'   an `NA` value.
#' These protocols started in 2011.
#'
#' @param data A data frame of weight-at-age values.
#' @param filter A logical specifying if the outliers should be removed or not.
#'   The default is to remove them.
#' @param drop A logical specifying if the new column called `outlier` should
#'   be removed from the data frame prior to returning it.
#' @export
#' @author Kelli F. Johnson
#' @return A data frame with the same columns or one more column, i.e.,
#' `outlier`, than the input data frame.
#' @family weight-at-age
weight_at_age_outlier <- function(data, filter = TRUE, drop = TRUE) {
  out <- data %>%
    dplyr::mutate(
      outlier = dplyr::case_when(
        Weight_kg < 0.52 & Length_cm > 45 & Year == 2003 & Month == 9 ~ TRUE,
        Weight_kg > (20e-6) * Length_cm^3 ~ TRUE,
        Weight_kg < (2e-6) * Length_cm^3 ~ TRUE,
        Age_yrs >= 99 ~ TRUE,
        TRUE ~ FALSE
      )
    ) %>%
    dplyr::filter(outlier %in% c(FALSE, ifelse(filter, FALSE, TRUE)))
  if (drop) {
    return(out %>% select(-outlier))
  } else {
    return(out)
  }
}

#' @export
#' @rdname weight_at_age_outlier
#' @family weight-at-age
plot_weight_at_age_outlier <- function(data, ...) {
  data_plot <- if (!"outlier" %in% colnames(data)) {
    weight_at_age_outlier(data, filter = FALSE, drop = FALSE)
  } else {
    {
      data
    } %>%
      dplyr::filter(!is.na(Weight_kg))
  }
  data_lines <- data.frame(
    Length_cm = 1:max(data_plot[["Length_cm"]], na.rm = TRUE)
  ) %>%
    dplyr::mutate(
      lower = Length_cm^3 * 2e-6,
      upper = Length_cm^3 * 20e-6
    )

  gg_length <- ggplot2::ggplot(
    data = data_plot,
    ggplot2::aes(
      x = Length_cm,
      y = Weight_kg
    )
  ) +
    ggplot2::geom_point(pch = 16, ggplot2::aes(color = outlier)) +
    ggplot2::scale_colour_manual(values = c(grDevices::rgb(0, 0, 0, 0.2), 2)) +
    ggplot2::geom_line(data = data_lines, colour = 4, ggplot2::aes(y = lower)) +
    ggplot2::geom_line(data = data_lines, colour = 4, ggplot2::aes(y = upper)) +
    ggplot2::ylim(c(0, max(data_plot[["Weight_kg"]], na.rm = TRUE) * 1.01)) +
    ggplot2::xlab("Length (cm)") +
    ggplot2::ylab("Weight (kg)") +
    ggplot2::theme(
      legend.title = ggplot2::element_text("Outlier"),
      legend.position = "none"
    ) +
    wrap_by(...)

  gg_age <- ggplot2::ggplot(
    data = data_plot,
    ggplot2::aes(
      x = Age_yrs,
      y = Weight_kg
    )
  ) +
    ggplot2::geom_point(pch = 16, ggplot2::aes(colour = outlier)) +
    ggplot2::scale_colour_manual(values = c(grDevices::rgb(0, 0, 0, 0.2), 2)) +
    ggplot2::xlab("Age (years)") +
    ggplot2::ylab("Weight (kg)") +
    ggplot2::theme(
      legend.title = ggplot2::element_text("Outlier"),
      legend.position = "none"
    ) +
    wrap_by(...)

  gg_age_log <- gg_age +
    ggplot2::scale_x_continuous(trans = "log10") +
    ggplot2::scale_y_continuous(trans = "log10") +
    ggplot2::theme(legend.position = "top", legend.box = "horizontal") +
    wrap_by(...)

  final <- cowplot::plot_grid(
    gg_length,
    gg_age,
    gg_age_log,
    align = "h",
    nrow = 1
  )
  return(final)
}

#' Read weight-at-age files from the disk and filter appropriately
#'
#' Read the file using the appropriate method based on the file extension and
#' filter the data such that data are not duplicated. For example, Canadian data
#' are in the cumulative file but a better version was provided by Chris Grandin
#' in 2022 as a csv file.
#'
#' @param file The file path, full or relative, to the `.csv` file you are
#'   importing. The file should have the following seven columns: Source,
#'   Weight_kg, Sex, Age_yrs, Length_cm, Month, and Year.
#' @export
#' @author Ian G. Taylor
#' @return A data frame of weight-at-age information.
#' The data frame includes the following columns:
#' \enumerate{
#'   \item Source
#'   \item Weight_kg
#'   \item Sex
#'   \item Age_yrs
#'   \item Length_cm
#'   \item Month
#'   \item Year
#'   \item OutlierL
#' }.
#' @family weight-at-age
weight_at_age_read <- function(file) {
  grep_CAN_fishery <- "^CAN_[jJps]"
  grep_US_fishery <- "^US|Poland"
  data_in <- if (tools::file_ext(file) == "csv") {
    utils::read.csv(file) %>%
      dplyr::select(-dplyr::matches("X"))
  } else {
    stop("Extensions other than csv are not currently supported.")
  }
  out <- data_in %>%
    dplyr::mutate(
      Source = gsub("(^SHORE$|^ATSEA$)", "US_\\L\\1", Source, perl = TRUE),
      Source = dplyr::case_when(
        grepl("_jv$", Source, ignore.case = TRUE) ~ toupper(Source),
        Source == "Poland_acoustic" ~ "Acoustic Poland",
        Source == "Acoustic U.S." ~ "US_acoustic",
        Source == "Acoustic Canada" ~ "CAN_acoustic",
        Source == "U.S. Acoustic" ~ "US_acoustic",
        Source == "Canada Acoustic" ~ "CAN_acoustic",
        Source == "ATSEA" ~ "US_atsea",
        TRUE ~ Source
      )
    ) %>%
    dplyr::select(-dplyr::matches("Sex"))

  invisible(out)
}

wrap_by <- function(...) {
  ggplot2::facet_wrap(ggplot2::vars(...), labeller = ggplot2::label_both)
}
