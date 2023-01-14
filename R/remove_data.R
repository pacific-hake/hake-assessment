#' Remove a fleet
#'
#' Remove a fleet from SS input files.
#'
#' @param dir A directory with SS input files.
#' @param fleets A vector of fleets to turn off.
#' @param remove A logical value whether or not the fleet should be
#' removed from all files or if `FALSE` then it will just be turned off.
#' The default is `FALSE`.
#'
#' @return Files are saved to the disk, nothing is returned.
#' @author Kelli F. Johnson
#'
remove_data.fleet <- function(dir, fleets, remove = FALSE) {
  starter <- r4ss::SS_readstarter(file.path(dir, "starter.ss"),
    verbose = FALSE)
  dat <- r4ss::SS_readdat(file.path(dir, starter[["datfile"]]),
    verbose = FALSE, echoall = FALSE)
  file.ctl <- file.path(dir, "control.ss_new")
  if (!file.exists(file.ctl)) {
    file.ctl <- file.path(dir, starter[["ctlfile"]])
  }
  ctl <- r4ss::SS_readctl(file.ctl,
    verbose = FALSE, echoall = FALSE, use_datlist = TRUE,
    datlist = dat)
  forecast <- r4ss::SS_readforecast(file.path(dir, "forecast.ss"),
    verbose = FALSE,
    Nfleets = dat[["Nfleets"]],
    Nareas = dat[["N_areas"]],
    nseas = dat[["nseas"]])
  if (all(seq(dat[["Nfleets"]]) %in% fleets)) {
    stop("You cannot remove all fleets from the model, modify\n",
      "'fleets'")
  }
  if (dat[["use_meanbodywt"]] != 0) {
    stop("remove_data.fleet was not designed to work with mean body weight.")
  }
  if (dat[["use_MeanSize_at_Age_obs"]] != 0) {
    stop("remove_data.fleet was not designed to work with mean size.")
  }
  if (dat[["N_discard_fleets"]] != 0) {
    stop("remove_data.fleet was not designed to work with discard data.")
  }

  if (!remove) {
    dat[["catch"]][, "year"] <- ifelse(
      dat[["catch"]][, "fleet"] %in% fleets, -1, 1) *
      dat[["catch"]][, "year"]
    dat[["CPUE"]][, "index"] <- ifelse(
      dat[["CPUE"]][, "fleet"] %in% fleets, -1, 1) *
      dat[["CPUE"]][, "index"]
    dat[["lencomp"]][, "Yr"] <- ifelse(
      dat[["lencomp"]][, "FltSvy"] %in% fleets, -1, 1) *
      dat[["lencomp"]][, "Yr"]
    dat[["agecomp"]][, "Yr"] <- ifelse(
      dat[["agecomp"]][, "FltSvy"] %in% fleets, -1, 1) *
      dat[["agecomp"]][, "Yr"]
  } else {
    dat[["fleetinfo"]] <- dat[["fleetinfo"]][!seq(dat[["Nfleets"]]) %in% fleets, ]
    dat[["CPUEinfo"]] <- dat[["CPUEinfo"]][!dat[["CPUEinfo"]][, "Fleet"] %in% fleets, ]
    dat[["Nfleets"]] <- NROW(dat[["fleetinfo"]])
    dat[["fleetnames"]] <- dat[["fleetinfo"]][, "fleetname", drop = TRUE]
    dat[["surveytiming"]] <- dat[["fleetinfo"]][, "surveytiming", drop = TRUE]
    dat[["units_of_catch"]] <- dat[["fleetinfo"]][, "units", drop = TRUE]
    dat[["catch"]] <- dat[["catch"]][!dat[["catch"]][, "fleet"] %in% fleets, ]
    dat[["CPUE"]] <- dat[["CPUE"]][!dat[["CPUE"]][, "index"] %in% fleets, ]
    dat[["len_info"]] <- dat[["len_info"]][
      row.names(dat[["len_info"]]) %in% dat[["fleetnames"]], ]
    dat[["age_info"]] <- dat[["age_info"]][
      row.names(dat[["age_info"]]) %in% dat[["fleetnames"]], ]
    dat[["lencomp"]] <- dat[["lencomp"]][
      !dat[["lencomp"]][, "FltSvy"] %in% fleets, ]
    dat[["agecomp"]] <- dat[["agecomp"]][
      !dat[["agecomp"]][, "FltSvy"] %in% fleets, ]
  }
  r4ss::SS_writectl(dat, file.path(dir, starter[["datfile"]]),
    verbose = FALSE)
  r4ss::SS_writectl(ctl, file.path(dir, starter[["ctlfile"]]),
    verbose = FALSE)

}

remove_data.comps <- function(dir, fleets, removeyears, removewtatage = FALSE) {
  starter <- r4ss::SS_readstarter(file.path(dir, "starter.ss"), verbose = FALSE)
  dat <- r4ss::SS_readdat(file.path(dir, starter[["datfile"]]),
    verbose = FALSE, echoall = FALSE)
  if (!is.null(dat[["lencomp"]])) {
    dat[["lencomp"]][, "Yr"] <- ifelse(
      (dat[["lencomp"]][, "FltSvy"] == fleets &
       dat[["lencomp"]][, "Yr"] %in% removeyears),
      -1, 1) * dat[["lencomp"]][, "Yr"]
  }
  if (!is.null(dat[["agecomp"]])) {
    dat[["agecomp"]][, "Yr"] <- ifelse(
      (dat[["agecomp"]][, "FltSvy"] == fleets &
       dat[["agecomp"]][, "Yr"] %in% removeyears),
      -1, 1) * dat[["agecomp"]][, "Yr"]
  }
  r4ss::SS_writedat(dat, file.path(dir, starter[["datfile"]]),
    verbose = FALSE, overwrite = TRUE)

  filename <- dir(dir, pattern = starter[["ctlfile"]], full.names = TRUE,
    ignore.case = TRUE)
  ctl <- r4ss::SS_readctl(filename,
    use_datlist = TRUE, datlist = dat, verbose = FALSE,
    version = type.convert(dat[["ReadVersion"]], as.is = TRUE))
  ctl[["MainRdevYrFirst"]] <- min(ifelse(dat[["agecomp"]][, "Yr"] > 0,
    dat[["agecomp"]][, "Yr"], NA), na.rm = TRUE) - 5
  ctl[["first_yr_fullbias_adj"]] <- min(ifelse(dat[["agecomp"]][, "Yr"] > 0,
    dat[["agecomp"]][, "Yr"], NA), na.rm = TRUE) + 5

  if (removewtatage) {
    wtatage <- r4ss::SS_readwtatage(file.path(dir, "wtatage.ss"))
    newwtatage <- wtatage[!(wtatage[["Yr"]] %in% removeyears), ]
    newmeans <- newwtatage %>% filter(Yr %in% 0:dat$endyr) %>%
      group_by(Fleet) %>% summarize(across(everything(), mean), .groups = "keep") %>%
      mutate(Yr = min(wtatage$Yr))
    newcomplete <- dplyr::full_join(filter(newwtatage, Yr != min(wtatage$Yr)), newmeans,
      by = colnames(wtatage)) %>% ungroup() %>% arrange(Fleet, Yr)
    r4ss::SS_writewtatage(newcomplete, dir = dir, file = "wtatage.ss",
      overwrite = TRUE, verbose = FALSE, warn = FALSE)
  }

  r4ss::SS_writectl(ctl, filename, verbose = FALSE, overwrite = TRUE)
  return(dir)
}
