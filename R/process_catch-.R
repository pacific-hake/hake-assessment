#' Process NORPAC catches
#'
#' Summarize and plot NORPAC catches for the at-sea hake fishery. Fleet types
#' are assigned to create summaries by fleet and year.
#'
#' @details # Tribal catches
#' The column `CDQ_CODE` contains strings starting with an upper-case followed
#' by and the last two digits of the the CDQ code of the vessel. The code was
#' created in 1995 and is used to track tribal catches in the hake fishery. The
#' following codes exist:
#' * C51 -- Aleutian Pribilof Island Community Development Association
#' * C52 -- Bristol Bay Economic Development Corporation
#' * C53 -- Central Bering Sea Fishermen's Association
#' * C54 -- Coastal Villages Fishing Cooperative
#' * C55 -- Norton Sound Economic Development Corporation
#' * C56 -- Yukon Delta Fisheries Development Association
#' * R10 -- Research Permit - Alaska Fisheries Science Center Catch Estimation
#' * M01 -- Makah Tribe Whiting Association
#' * R11 -- Research Permit - WA Sea Grant bird/longline interaction
#' * R12 -- Research Permit - WA Sea Grant bird/longline interaction
#'
#' @details # Raw data
#' Raw data consists of sampled and unsampled hauls with the total weight
#' including bycatch weight. For sampled hauls, `EXTRAPOLATED_WEIGHT` is the
#' weight of the catch only. For unsampled hauls that have no knowledge of the
#' amount of bycatch included in the haul, `OFFICIAL_TOTAL_CATCH` must be used
#' along with a estimated bycatch rate to calculate `EXTRAPOLATED_WEIGHT`.
#' Monthly fleet-specific bycatch rates are estimated from the sampled hauls and
#' multiplied by the `OFFICIAL_TOTAL_CATCH` to get an estimate of the amount of
#' bycatch that should be subtracted from `OFFICIAL_TOTAL_CATCH` to get just the
#' weight of Pacific Hake.
#' The raw column names are as follows `colnames(ncatch)`:
#' * "HAULJOIN"
#' * "CRUISE"
#' * "PERMIT": "a unique vessel identifier"
#' * "VESSEL"
#' * "VESSEL_TYPE"
#' * "HAUL_DATE"
#' * "HAUL"
#' * "DEPLOYMENT_DATE"
#' * "RETRIEVAL_DATE"
#' * "HRS"
#' * "CDQ_CODE"
#' * "OFFICIAL_TOTAL_CATCH"
#' * "HAUL_SAMPLED_BY"
#' * "LATDD_START"
#' * "LONDD_START"
#' * "LATDD_END"
#' * "LONDD_END"
#' * "FISHING_DEPTH_FATHOMS"
#' * "BOTTOM_DEPTH_FATHOMS"
#' * "CATCHER_BOAT_ADFG": "unique vessel identifier for catcher vessels
#'                         delivering to motherships"
#' * "SPECIES"
#' * "EXTRAPOLATED_WEIGHT"
#'
#' @details # Definitions
#' 1. `OFFICIAL_TOTAL_CATCH` is measured in metric tons (mt) and includes both
#'    retained and discarded species. Thus, this measurement sums the targeted
#'    species, prohibited species, and non-allocated species.
#' 1. `EXTRAPOLATED_WEIGHT` is measured in kilograms (kg) and includes just the
#'    weight of the relevant `SPECIES` included in that row of data, e.g., if
#'    `SPECIES == `r species_norpac``, then `EXTRAPOLATED_WEIGHT` is the kg of
#'    Pacific Hake in that haul.
#'
#' @details # Assumptions
#' * sampled hauls have an entry in the `SPECIES` column, i.e., hake are
#'   denoted with `r species_norpac`
#' * an unsampled haul is represented by one unique record because there no
#'   additional lines of catch for the bycatch species given it was not sampled
#'
#' @param ncatch A data frame with catch information extracted from the NORPAC
#'   database. The default is to read the `.Rdat` file from the disk using
#'   [get_local()]. Else, users can just pass the R object itself, which would
#'   typically be `hakedata[["ncatch"]]`.
#' @param nyears The number of years for plotting.
#'
#' @import ggplot2 grDevices
#' @export
#' @author Kelli F. Johnson
#'
#' @return Figures and csv files are saved to the disk regarding
#' catch rates and depth (fishing and bottom depths).
#'
#' In the `Catches` directory, the following csv files are saved:
#'   * depth-us-atsea-bottom.csv
#'   * depth-us-atsea-fishing.csv
#'   * us-cp-catch-by-month.csv
#'   * us-ms-catch-by-month.csv
#'   * us-cp-catch-rate-by-month.csv
#'   * us-ms-catch-rate-by-month.csv
#'
process_catch_norpac <- function(ncatch = get_local(file = "ncatch.Rdat"),
                                 nyears = 5,
                                 savedir = hakedata_wd()) {
  # Setup the environment
  args <- list(
    width = 6.5,
    height = 4.5,
    pointsize = 10,
    units = "in",
    res = 600
  )
  oldop <- options()$warn
  options(warn = -1)
  on.exit(options(warn = oldop), add = TRUE)

  # File management
  fs::dir_create(
    path = fs::path(savedir, "Figures"),
    recurse = TRUE
  )

  out_ncatch <- ncatch |>
    dplyr::mutate(
      Date = f_date(RETRIEVAL_DATE, format = "%Y-%m-%d"),
      month = f_date(RETRIEVAL_DATE, format = "%m"),
      Month = f_date(RETRIEVAL_DATE, format = "%b", factor = TRUE),
      year = f_date(RETRIEVAL_DATE, "%Y"),
      # Create catch rate in mt/hr
      crate = EXTRAPOLATED_WEIGHT / 1000 / HRS,
      FISHING_DEPTH_M = FISHING_DEPTH_FATHOMS * fathom_to_meter,
      BOTTOM_DEPTH_M = BOTTOM_DEPTH_FATHOMS * fathom_to_meter,
      vesseltype = f_vessel_type(VESSEL_TYPE),
      Sector = "DomesticAtSea",
      sampled = ifelse(is.na(HAUL_SAMPLED_BY) | HAUL_SAMPLED_BY == 0, 0, 1),
      # Unsampled hauls will have a SPECIES == NA and EXTRAPOLATED_WEIGHT == NA
      SPECIES = ifelse(sampled == 0, species_norpac, SPECIES),
      OFFICIAL_TOTAL_CATCHkg = OFFICIAL_TOTAL_CATCH * 1000,
      # ByCatch (kg)
      ByCatch = OFFICIAL_TOTAL_CATCHkg - EXTRAPOLATED_WEIGHT
    ) |>
    # TODO: remove these filters when we want all the data and when the NA year
    #       is fixed
    dplyr::filter(!is.na(year), year > 2007) |>
    dplyr::group_by(year, month, SPECIES == species_norpac, VESSEL_TYPE) |>
    dplyr::mutate(
      bycatchrate = sum(ifelse(sampled == 1, ByCatch, 0), na.rm = TRUE) /
        sum(ifelse(sampled == 1, OFFICIAL_TOTAL_CATCHkg, 0), na.rm = TRUE),
      Catch.MT = ifelse(
        test = sampled == 0,
        yes = OFFICIAL_TOTAL_CATCHkg * (1 - bycatchrate),
        no = EXTRAPOLATED_WEIGHT
      ) / 1000,
      ByCatch = ifelse(
        test = sampled == 0,
        yes = OFFICIAL_TOTAL_CATCHkg - Catch.MT / 1000,
        no = ByCatch
      ),
      catch = round(Catch.MT, digits = 5)
    ) |>
    dplyr::ungroup()

  # Find monthly rate for un-sampled tows and multiply times OFFICIAL_CATCH
  # to get the amount of hake in the tow based on average bycatch rate
  # bycatch rates are VESSEL_TYPE specific as of 2019 assessment

  # Shift month by 1 if there are not three vessels in a grouping
  test <- calculate_is_confidential(out_ncatch)[1, ]
  while (NROW(test) >= 1) {
    out_ncatch <- out_ncatch |>
      dplyr::mutate(
        month = ifelse(
          year %in% test[["year"]] & month %in% test[["month"]] &
          vesseltype %in% test[["vesseltype"]], month - 1, month
        ),
        month = ifelse(month < 5, 5, month)
      )
    test <- calculate_is_confidential(out_ncatch)
    if (NROW(test) == 0) {
      break
    } else {
      test <- test[1, ]
    }
  }

  # Get monthly summaries by at-sea fleet to save to *catch-by-month.csv files
  monthly_sum <- out_ncatch |>
    dplyr::filter(SPECIES == species_norpac) |>
    dplyr::group_by(Sector, vesseltype, month, year) |>
    dplyr::summarize(catch = sum(catch, na.rm = TRUE)) |>
    dplyr::ungroup()

  cp <- monthly_sum[monthly_sum$vesseltype == "CP", -(1:2)]
  utils::write.table(
    cp |> dplyr::arrange(year, month),
    file = fs::path(savedir, "us-cp-catch-by-month.csv"),
    sep = ",",
    row.names = FALSE,
    quote = FALSE
  )

  ms <- monthly_sum[monthly_sum$vesseltype == "MS", -(1:2)]
  utils::write.table(
    ms |> dplyr::arrange(year, month),
    file = fs::path(savedir, "us-ms-catch-by-month.csv"),
    sep = ",",
    row.names = FALSE,
    quote = FALSE
  )

  hcatch <- out_ncatch |>
    dplyr::filter(SPECIES == species_norpac) |>
    dplyr::mutate(
      months = droplevels(factor(
        month,
        levels = 1:12,
        labels = rep(
          paste(seq(1, 11, by = 2), seq(2, 12, by = 2), sep = "-"),
          each = 2
        )
      ))
    )


  # Unique vessel (either catcher boat if not NA or mothership)
  keeptheseyears <- utils::tail(1:max(hcatch[["year"]], na.rm = TRUE), nyears)

  #### Figures: depths
  utils::write.csv(
    x = get_depth_by_year(
      hcatch,
      type = "bottom",
      yrs = keeptheseyears,
      min_depth_cutoff = 0
    ),
    file = file.path(savedir, "depth-us-atsea-bottom.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    x = get_depth_by_year(
      hcatch,
      type = "gear",
      yrs = keeptheseyears,
      min_depth_cutoff = 0
    ),
    file = file.path(savedir, "depth-us-atsea-fishing.csv"),
    row.names = FALSE
  )

  # catch rate
  # FIXME: Ensure that n_year_months_type is being calculated correctly because
  #        it is currently taking out all CP years.
  data_rate <- hcatch |>
    dplyr::filter(vesseltype %in% c("CP", "MS")) |>
    dplyr::group_by(year, months, vesseltype) %>%
    dplyr::ungroup() |>
    tidyr::nest(gg = -"vesseltype") |>
    mutate_at(
      "gg",
      purrr::map,
      function(x) get_rate_by_month(x)
    ) |>
    dplyr::mutate(
      lower_name = tolower(vesseltype),
      name = fs::path(
        savedir,
        glue::glue("us-{lower_name}-catch-rate-by-month.csv")
      ),
      purrr::walk2(
        .x = gg,
        .y = name,
        .f = \(x, y) utils::write.csv(x = x, file = y, row.names = FALSE)
      )
    )
  return(out_ncatch)
}

#' Print summaries and figures for catches from PacFIN
#'
#' @param pcatch An R object of PacFIN catches. The default value is loaded from
#'   the disk using [get_local()]. Otherwise, the R object is typically stored
#'   in the `hakedata` environment.
#' @param nyears An integer specifying the number of years you want to plot,
#'   where the most recent year will be sequenced back in time `nyears`. The
#'   default is typically five years.
#' @inheritParams process_weight_at_age_survey
#'
#' @return The following files are saved to the disk:
#' * us-shore-catch-by-month.csv
#' * us-research-catch-by-month.csv
#' * us-ti-catch-by-month.csv
#'
process_catch_pacfin <- function(pcatch = get_local(file = "pcatch.Rdat"),
                                 nyears = 5,
                                 savedir = hakedata_wd()) {
  # FLEET XXX is in the hake assessment as shore-based catches,
  # although 1986 differs from data used
  # database  1986 3431.9436
  # assesment 1986 3465.00

  pcatch_by_month_year <- pcatch |>
    # TODO: remove this filter when we want all the data 
    dplyr::group_by(
      sector,
      month,
      year
    ) |>
    dplyr::summarise(catch = sum(MT, na.rm = TRUE)) |>
    dplyr::arrange(sector, year, month) |>
    dplyr::ungroup()

  utils::write.table(
    x = pcatch_by_month_year |>
      dplyr::filter(sector == "USshore") |>
      dplyr::select(-sector) |>
      dplyr::mutate(catch = round(catch, 5)),
    file = file.path(savedir, "us-shore-catch-by-month.csv"),
    sep = ",",
    quote = FALSE,
    row.names = FALSE
  )
  utils::write.table(
    x = pcatch_by_month_year |>
      dplyr::filter(sector == "USresearch") |>
      dplyr::select(-sector) |>
      dplyr::mutate(catch = sprintf("%.9f", catch)),
    file = file.path(savedir, "us-research-catch-by-month.csv"),
    sep = ",",
    quote = FALSE,
    row.names = FALSE
  )

  # Look at tribal catch in shoreside (already added in above)
  utils::write.table(
    x = pcatch |>
      dplyr::filter(FLEET == "TI") |>
      dplyr::group_by(month, year) |>
      dplyr::summarize(
        catch = round(sum(MT, na.rm = TRUE), 5),
        n_v = dplyr::n_distinct(VESSEL_ID)
      ) |>
      dplyr::mutate(
        catch = ifelse(n_v < 3, NA_real_, catch)
      ) |>
      dplyr::select(-n_v) |>
      dplyr::arrange(year, month) |>
      dplyr::ungroup(),
    file = file.path(savedir, "us-ti-catch-by-month.csv"),
    sep = ",",
    quote = FALSE,
    row.names = FALSE
  )
}

#' Format `VESSEL_TYPE` as a character
#'
#' `VESSEL_TYPE` in the NORPAC data base are integers and this function
#' changes these integer values to character strings that provide meaning.
#' The following integers are the available options in NORPAC to indicate
#' whether the vessel processes fish or delivers it to a processing plant:
#' 1. a catcher processor (CP) vessel;
#' 2. a mothership (MS) or ship that receives unsorted codends from
#'   other vessels;
#' 3. a catcher only vessel that delivers unprocessed fish to shoreside,
#'   floating plant, or vessel this only occurred in 2009 and 2010 with a
#'   single vessel, see the notes in the code for more details;
#' 4. a MS that receives sorted codends;
#' 5. a vessel that sells the majority of their catch over the side to other
#'   fishing vessels who will utilize the fish for bait, and
#' 6. vessels that discard all catch from a haul.
#'
#' @param x A vector of integers between one and six.
#'
#' @author Kelli F. Johnson
#' @return A vector of combined integer values and character strings.
#'
f_vessel_type <- function(x) {
  if (!any(x %in% 1:6)) {
    stop("All values in x must be between one and six.")
  }
  x[x == 1] <- "CP"
  x[x == 2] <- "MS"
  # A catcher vessel (A709) named the Stormy C (A709) that did minimal
  # processing at sea and had to have an observer on board.
  x[x == 3] <- "CP"
  return(x)
}

# Internal function to calculate if at-sea catch data is confidential where each
# month is backed up by one if there are not three vessels in the grouping
calculate_is_confidential <- function(data) {
  data |>
    dplyr::ungroup() |>
    dplyr::filter(SPECIES == species_norpac) |>
    dplyr::mutate(
      sixth = dplyr::case_when(
        month %in% 1:2 ~ "1--2",
        month %in% 3:4 ~ "3--4",
        month %in% 5:6 ~ "5--6",
        month %in% 7:8 ~ "7--8",
        month %in% 9:10 ~ "9--10",
        month %in% 11:12 ~ "11--12",
      )
    ) |>
    dplyr::group_by(year, month, vesseltype) |>
    dplyr::mutate(
      n_v = n_distinct(
        ifelse(is.na(CATCHER_BOAT_ADFG), VESSEL, CATCHER_BOAT_ADFG)
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::distinct(year, month, sixth, .keep_all = TRUE) |>
    dplyr::select(
      vesseltype,
      year,
      month,
      sixth,
      dplyr::starts_with("n_v")
    ) |>
    dplyr::arrange(vesseltype, year, dplyr::desc(month)) |>
    dplyr::filter(n_v < 3)
}
