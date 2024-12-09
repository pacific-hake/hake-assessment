#' Write a set of Stock Synthesis input files to a directory for bridging
#'
#' Write a set of Stock Synthesis input files to a directory after modifying
#' them with new data for the year. Children functions manipulates specific
#' parts of the data or control file and the wrapper function ensures that they
#' are written in the correct order and saved using a standard naming protocol.
#'
#' @details
#' * Catch: The catches are taken from data/landings-tac-history.csv, which is
#'   updated in [process_catch()].
#' * Weight-At-Age: The weight-at-age information is stored in data/wtatage.ss,
#'   which is updated in [process_weight_at_age()].
#' * CPUE: Catch-per-unit-effort data for the age-2-plus and age-1 surveys,
#'   with the following columns:
#'   * `year`
#'   * `seas`
#'   * `index`
#'   * `obs`
#'   * `se_log`.
#'   TODO: needs to be saved somewhere in the repository, this could be in
#'         survey-history.csv but the decimal places are not enough right now
#'         and the age-1 index is not saved. Would also need to add 0's for the
#'         years without a survey.
#' * agecomp_survey Age-composition data for the age-2-plus survey.
#'   TODO: save this information somewhere
#' @param dir_input A string that specifies the path to a directory that stores
#'   the input files for a model that you want to use as the basis of the
#'   bridging. It will be passed to [r4ss::SS_read()]. The path will typically
#'   be to the previous year's base model, which represents the first model in a
#'   bridging set.
#' @param dir_output A string that specifies the path to a directory that may
#'   or may not already exist that will be used to save the output.
#'
#' @author Kelli F. Johnson
#' @return An invisible list of modified Stock Synthesis input files. A
#' directory is also created in `dir_output` with multiple directories, each
#' storing a bridging step.
#' @export
write_bridging <- function(dir_input,
                           dir_output) {
  input <- r4ss::SS_read(
    dir_input,
    ss_new = fs::file_exists(fs::path(dir_input, "starter.ss_new")),
    verbose = FALSE
  )

  unlink(
    x = fs::dir_ls(path = dir_output, type = "directory"),
    recursive = TRUE
  )
  # 01 new executable
  output_01 <- r4ss::SS_write(
    input,
    dir = fs::path(dir_output, "01-updated-ss-exe"),
    overwrite = TRUE
  )
  # 02 catch
  output_02 <- write_bridging_catch(
    input = input,
    dir_output = dir_output
  )
  # 03 weight-at-age data
  output_03 <- write_bridging_weight_at_age(
    input = output_02,
    dir_output = dir_output
  )
  # 04 survey age-2+ fish
  output_04 <- write_bridging_other(
    input = output_03,
    dir_output = dir_output,
    dir_name = "04-add-survey-age-2-plus"
    # CPUE = ,
    # agecomp_survey =
  )
  # 05 survey age-1 fish
  output_05 <- write_bridging_other(
    input = output_04,
    dir_output = dir_output,
    dir_name = "05-add-survey-age-1"
    # CPUE =
  )
  # 06 fishery
  output_06 <- write_bridging_other(
    input = output_05,
    dir_output = dir_output,
    dir_name = "06-add-fishery-ages",
    fishery = TRUE
  )

  return(invisible(output_06))
}

write_bridging_catch <- function(input, dir_output) {
  repo_catch <- utils::read.csv(
    file = fs::path(hakedata_wd(), "landings-tac-history.csv"),
  )
  input[["dat"]][["catch"]] <- rbind(
    input[["dat"]][["catch"]] %>%
      dplyr::filter(year <= 0),
    data.frame(
      "year" = repo_catch$Year,
      "seas" = 1,
      "fleet" = 1,
      catch = repo_catch$TOTAL,
      catch_se = 0.01
    )
  )
  input[["dat"]][["endyr"]] <- max(repo_catch$Year)
  input[["par"]] <- NULL
  r4ss::SS_write(
    inputlist = input,
    dir = fs::path(dir_output, "02-add-new-catch"),
    overwrite = TRUE,
    verbose = FALSE
  )
  return(invisible(input))
}

write_bridging_weight_at_age <- function(input, dir_output) {
  # Need to add input weight-at-age data
  weight_at_age <- r4ss::SS_readwtatage(
    file = fs::path(hakedata_wd(), "wtatage.ss"),
    verbose = FALSE
  ) %>%
    dplyr::select(-dplyr::matches("comment")) %>%
    dplyr::arrange(Yr, Fleet)
  input[["wtatage"]] <- weight_at_age
  input[["par"]] <- NULL
  r4ss::SS_write(
    inputlist = input,
    dir = fs::path(dir_output, "03-add-new-weight-at-age"),
    overwrite = TRUE,
    verbose = FALSE
  )
  return(invisible(input))
}

write_bridging_other <- function(input,
                                 dir_output,
                                 CPUE,
                                 agecomp,
                                 dir_name,
                                 fishery = FALSE) {
  # Deal with new data
  if (!missing(CPUE)) {
    input[["dat"]][["CPUE"]] <- rbind(
      input[["dat"]][["CPUE"]] %>%
        dplyr::filter(!index %in% CPUE[["index"]]),
      CPUE
    )
  }
  if (!missing(agecomp)) {
    input[["dat"]][["agecomp"]] <- rbind(
      input[["dat"]][["agecomp"]] %>%
        dplyr::filter(!FltSvy %in% agecomp[["FltSvy"]]),
      agecomp
    )
  }
  if (missing(CPUE) & missing(agecomp) & fishery) {
    agecomp <- load_age_fishery()
    input[["dat"]][["agecomp"]] <- rbind(
      dplyr::filter(input[["dat"]][["agecomp"]], Yr < 2008 | FltSvy != 1),
      setNames(
        dplyr::filter(agecomp, year >= 2008),
        colnames(input[["dat"]][["agecomp"]])
      )
    )
  }

  # Standard inputs that do not depend on new data
  # Check selectivity deviation year
  input[["ctl"]] <- update_ss3_selectivity(input_list = input)
  # Check recruitment year
  input[["ctl"]] <- update_ss3_recruitment_year(input_list = input)
  # Check ageing error matrix
  input[["dat"]] <- update_ss3_ageing_error(input_list = input)

  input[["par"]] <- NULL
  r4ss::SS_write(
    inputlist = input,
    dir = fs::path(dir_output, dir_name),
    overwrite = TRUE,
    verbose = FALSE
  )
  return(invisible(input))
}
