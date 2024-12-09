load_age <- function(datapath) {
  source(
    "https://raw.githubusercontent.com/pacific-hake/hake-assessment/master/R/load-data.R",
    local = TRUE
  )

  canada <- load.can.age.data(file.path(datapath, "can-age-data.csv"))
  can <- mapply(
    function(x, y) cbind(x, Nsamples = y),
    setNames(canada[1:3], c("can_shore", "can_freeze", "can_jv")),
    lapply(canada[4:6], as.numeric)
  ) %>%
    lapply(tibble::as_tibble, rownames = "year", .name_repair = "minimal") %>%
    dplyr::bind_rows(.id = "sector") %>%
    dplyr::mutate(year = as.numeric(year)) %>%
    dplyr::rename_with(~ gsub("^([0-9]+)$", "a\\1", .))

  usa <- list(
    "us_cp" = read.csv(file.path(datapath, "us-cp-age-data.csv")),
    "us_ms" = read.csv(file.path(datapath, "us-ms-age-data.csv")),
    "us_shore" = read.csv(file.path(datapath, "us-shore-age-data.csv"))
  ) %>%
    dplyr::bind_rows(.id = "sector") %>%
    dplyr::mutate(Nsamples = ifelse(is.na(n.trips), n.hauls, n.trips)) %>%
    dplyr::select(-dplyr::matches("n.", ignore.case = FALSE))

  return(dplyr::full_join(can, usa, by = colnames(can)))
}
#' Load data and create age compositions
#'
# TODO:
# 1. Weight-at-age is summed over all ages 0-20 but ages are not available for 16+?
# 2. Remove FALSE if statement b/c it was just used to upweight CAN comps
#' @param dirdata The directory with the data.
load_age_fishery <- function(dirdata = hakedata_wd()) {
  options(default.stringsAsFactors = FALSE)
  options(stringsAsFactors = FALSE)

  wtatage_repo <- r4ss::SS_readwtatage(
    file = fs::path(dirdata, "wtatage.ss"),
    verbose = FALSE
  ) %>%
    dplyr::filter(Fleet == 1) %>%
    dplyr::rename(year = "Yr") %>%
    dplyr::select(year, dplyr::matches("^[0-9]+$")) %>%
    tidyr::pivot_longer(names_to = "age", values_to = "weight", cols = -year)

  catch_repo <- load_catch(file.path(dirdata, "landings-tac-history.csv")) %>%
    dplyr::select(
      -dplyr::matches("tot|TAC|attain|_catch",
        ignore.case = TRUE
      )
    ) %>%
    tidyr::pivot_longer(
      names_to = c("sector", "x"),
      values_to = "catch",
      cols = -year,
      names_sep = "_x"
    ) %>%
    dplyr::select(-x)

  if (FALSE) {
    ave_attain <- load_catch(
      file.path(dirdata, "landings-tac-history.csv")
    ) %>%
      dplyr::filter(
        year %in% (max(year) - (5:1))
      ) %>%
      dplyr::mutate(percent_shore = can_shore_xx / can_catch) %>%
      dplyr::summarise_all(mean)
    new_catch <- load_catch(
      file.path(dirdata, "landings-tac-history.csv")
    ) %>%
      dplyr::filter(year == 2022) %>%
      dplyr::pull(can_tac) * ave_attain[["can_attain"]] / 100 *
      (c(1, 0) - ave_attain[["percent_shore"]] * c(1, -1))
    catch_repo[
      catch_repo[["year"]] == 2022 & catch_repo[["sector"]] == "can_shore",
      "catch"
    ] <- new_catch[1]
    catch_repo[
      catch_repo[["year"]] == 2022 & catch_repo[["sector"]] == "can_freeze",
      "catch"
    ] <- new_catch[2]
  }

  final <- load_age(dirdata) %>%
    dplyr::left_join(catch_repo, by = c("sector", "year")) %>%
    tidyr::pivot_longer(
      names_prefix = "a",
      names_to = "age",
      values_to = "numbers",
      cols = dplyr::matches("^a[0-9]+$")
    ) %>%
    dplyr::left_join(wtatage_repo, by = c("age", "year")) %>%
    dplyr::group_by(year, sector) %>%
    dplyr::mutate(
      prop = (catch * numbers) / sum(numbers * weight)
    ) %>%
    dplyr::group_by(year, age) %>%
    dplyr::summarize(
      prop = sum(prop, na.rm = TRUE)
    ) %>%
    dplyr::group_by(year) %>%
    dplyr::mutate(prop = prop.table(prop) * 100) %>%
    dplyr::filter(!is.na(prop)) %>%
    dplyr::arrange(age = as.numeric(age)) %>%
    tidyr::pivot_wider(names_from = age, values_from = prop) %>%
    dplyr::left_join(
      x = load_age(dirdata) %>%
        dplyr::group_by(year, sector) %>%
        dplyr::summarize(n = unique(Nsamples)) %>%
        dplyr::group_by(year) %>%
        dplyr::summarize(nTrips = sum(n)),
      by = "year"
    ) %>%
    dplyr::mutate(
      Month = 7,
      Fleet = 1,
      Sex = 0,
      Partition = 0,
      AgeErr = year - 1972,
      LbinLo = -1,
      LbinHi = -1,
      .after = year
    )
  return(final)
}

load_catch <- function(file) {
  source(
    "https://raw.githubusercontent.com/pacific-hake/hake-assessment/master/R/catches.R",
    local = TRUE
  )
  out <- load_catches(file)
  return(out)
}
