devtools::load_all()
library(dplyr)
library(ggplto2)

load(here::here("data-tables", "extractedData", "page.Rdat"))
load(here::here("data-tables", "extractedData", "nages.Rdat"))
load(here::here("data-tables", "extractedData", "ncatch.Rdat"))

# At-Sea ====================================================
at_sea_ages <- nages |>
  dplyr::filter(Year == 2025) |>
  dplyr::group_by(Month) |>
  dplyr::summarize(
    vessels = length(unique(PERMIT)),
    n = sum(!is.na(AGE))
  )
as <- nages |>
  dplyr::filter(Year == 2025, PERMIT == 2943, !is.na(AGE))

at_sea_catch <- ncatch |>
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
  dplyr::filter(!is.na(year), year == 2025) |>
  dplyr::group_by(year, month, SPECIES == species_norpac, VESSEL_TYPE) |>
  dplyr::mutate(
    bycatchrate = sum(ifelse(sampled == 1, ByCatch, 0), na.rm = TRUE) /
      sum(ifelse(sampled == 1, OFFICIAL_TOTAL_CATCHkg, 0), na.rm = TRUE),
    Catch.MT = ifelse(
      test = sampled == 0,
      yes = OFFICIAL_TOTAL_CATCHkg * (1 - bycatchrate),
      no = EXTRAPOLATED_WEIGHT
    ) /
      1000,
    ByCatch = ifelse(
      test = sampled == 0,
      yes = OFFICIAL_TOTAL_CATCHkg - Catch.MT / 1000,
      no = ByCatch
    ),
    catch = round(Catch.MT, digits = 5)
  ) |>
  dplyr::ungroup()

vessel_n <- dplyr::filter(
  nages,
  Year %in% 2025,
  !is.na(AGE)
) |>
  dplyr::left_join(
    y = at_sea_catch |>
      dplyr::filter(SPECIES == species_norpac) |>
      dplyr::select(HAULJOIN, HAUL, vesseltype),
    by = c(HAUL_JOIN = "HAULJOIN", HAUL_OFFLOAD = "HAUL")
  ) |>
  dplyr::mutate(
    AGE = ifelse(AGE > 15, 15, AGE)
  ) |>
  dplyr::rename(year = Year) |>
  dplyr::group_by(vesseltype, Month) |>
  dplyr::summarise(vessles = length(unique(PERMIT)))

raw_ages <- dplyr::filter(
  nages,
  Year %in% 2025,
  !is.na(AGE),
  Month != 11
) |>
  dplyr::left_join(
    y = at_sea_catch |>
      dplyr::filter(SPECIES == species_norpac) |>
      dplyr::select(HAULJOIN, HAUL, vesseltype),
    by = c(HAUL_JOIN = "HAULJOIN", HAUL_OFFLOAD = "HAUL")
  ) |>
  dplyr::mutate(
    AGE = ifelse(AGE > 15, 15, AGE)
  ) |>
  dplyr::rename(year = Year) |>
  dplyr::group_by(vesseltype, Month) |>
  dplyr::group_by(year, AGE, vesseltype, Month) |>
  dplyr::summarise(
    n = dplyr::n()
  )


at_sea_prop <- raw_ages |>
  dplyr::filter(Month != 11) |>
  group_by(vesseltype, Month) |>
  mutate(
    total = sum(n)
  )  |>
  dplyr::group_by(AGE, vesseltype, Month) |>
  dplyr::summarize(
    prop = n / unique(total)
  ) |>
  dplyr::ungroup()

ggplot(raw_ages, aes(x = as.factor(AGE), y = n)) +
  geom_bar(stat = 'identity') +
  facet_grid(c("Month", "vesseltype"))

ggplot(at_sea_prop[which(at_sea_prop$vesseltype == "MS"), ], aes(x = AGE, y = prop)) +
  geom_bar(stat = 'identity') +
  facet_grid(Month~.) +
  ylab("Proportion by Month") +
  xlab("Age") +
  ggtitle("Mothership Ages Collected in 2025") +
  theme_bw()

ggplot(at_sea_prop[which(at_sea_prop$vesseltype == "CP"), ], aes(x = AGE, y = prop)) +
  geom_bar(stat = 'identity') +
  facet_grid(Month~.) +
  ylab("Proportion by Month") +
  xlab("Age") +
  ggtitle("Catcher-Processor Ages Collected in 2025") +
  theme_bw()

ggplot(at_sea_prop, aes(x = AGE, y = prop)) +
  geom_bar(stat = 'identity') +
  facet_grid(c("Month", "vesseltype")) +
  ylab("Proportion by Month") +
  xlab("Age") +
  theme_bw() +
  ggplot2::theme(
    strip.text = element_text(size = 16),
    axis.text.y = ggplot2::element_text(size = 14),
    axis.text = ggplot2::element_text(size = 16),
    axis.title = ggplot2::element_text(size = 16)
  )
ggsave(filename = here::here("sandbox", "chantel", "srg-request-6-at-sea-age-proportions.png"),
       height = 7,
       width = 12)

length_age <- nages |>
  dplyr::mutate(
    type = dplyr::case_when(is.na(AGE) ~ "length-only", .default = "length-age")
  )

ggplot(length_age, aes(x = LENGTH, color = type)) +
  geom_density(size = 1) +
  theme_bw() +
  xlab("Length (cm)") + ylab("Density")

# Shoreside ================================================================
shoreside <- page |>
  dplyr::filter(SAMPLE_YEAR == 2025) |>
  dplyr::group_by(SAMPLE_MONTH) |>
  dplyr::summarize(
    n_age = sum(!is.na(AGE)),
    n_len = sum(!is.na(FISH_LENGTH)),
    n_weight = sum(!is.na(FISH_WEIGHT))
  )

ss_age_prop <- page |>
  dplyr::filter(SAMPLE_YEAR == 2025) |>
  dplyr::mutate(
    AGE = ifelse(AGE > 15, 15, AGE)
  ) |>
  group_by(SAMPLE_MONTH) |>
  mutate(
    total = sum(!is.na(AGE))
  ) |>
  group_by(AGE, SAMPLE_MONTH) |>
  summarise(
    n = sum(!is.na(AGE)),
    total = unique(total),
    prop = n / total
  ) |>
  dplyr::filter(
    SAMPLE_MONTH %in% 5:10
  )

ggplot(ss_age_prop, aes(x = AGE, y = prop)) +
  geom_bar(stat = 'identity') +
  facet_grid(c("SAMPLE_MONTH")) +
  ylab("Proportion by Month") +
  xlab("Age") +
  theme_bw() +
  ggplot2::theme(
    strip.text = element_text(size = 16),
    axis.text.y = ggplot2::element_text(size = 14),
    axis.text = ggplot2::element_text(size = 16),
    axis.title = ggplot2::element_text(size = 16),
    plot.title = ggplot2::element_text(size = 16)
  ) +
  ggtitle("Shoreside: Age Proportions by Month")

ggsave(filename = here::here("sandbox", "chantel", "srg-request-6-ss-age-proportions.png"),
       height = 7,
       width = 12)

#============================================================================

ggplot(as, aes(x = as.factor(AGE), y = WEIGHT)) +
  geom_point()

as_count <- as |>
  dplyr::group_by(AGE) |>
  dplyr::summarise(
    count = dplyr::n()
  )
ggplot(as_count, aes(x = as.factor(AGE), y = count)) +
  geom_bar(stat = 'identity')
