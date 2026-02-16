# USA ---------

ss_us_prop <- us_ss_age_df |>
  select(-n.fish, -n.trips) |>
  rename(Year = year)

names(ss_us_prop) <- names(ss_us_prop) %>%
  gsub("^a", "", .)

x <- \(d){
  d |>
    select(-num_fish, -num_samples) |>
    rename(Year = year)
}

prop_dfs <- list(can_ss_age_df,
                 can_ft_age_df,
                 us_cp_age_df,
                 us_ms_age_df) |>
  map(~{x(.x)})

prop_dfs <- c(prop_dfs, list(ss_us_prop))
names(prop_dfs) <- c("Canada SS",
                     "Canada FT",
                     "US CP",
                     "US MS",
                     "US SS")

can_ct_ss <- ct |>
  select(Year, `Canada Shoreside`)
can_ss <- prop_dfs$`Canada SS` |>
  left_join(can_ct_ss, by = "Year")
can_ss_ct_age <- ss |>
  mutate(across(-Year, ~{.x = .x * `Canada Shoreside`})) |>
  select(-`Canada Shoreside`)

can_ct_ft <- ct |>
  select(Year, `Canada Freezer-trawler`)
can_ft <- prop_dfs$`Canada FT` |>
  left_join(can_ct_ft, by = "Year")
can_ft_ct_age <- can_ft |>
  mutate(across(-Year, ~{.x = .x * `Canada Freezer-trawler`})) |>
  select(-`Canada Freezer-trawler`)

us_ct_cp <- ct |>
  select(Year, `U.S. Catcher-processor`)
us_cp <- prop_dfs$`US CP` |>
  left_join(us_ct_cp, by = "Year")
us_cp_ct_age <- us_cp |>
  mutate(across(-Year, ~{.x = .x * `U.S. Catcher-processor`})) |>
  select(-`U.S. Catcher-processor`)

us_ct_ms <- ct |>
  select(Year, `U.S. Mothership`)
us_ms <- prop_dfs$`US MS` |>
  left_join(us_ct_ms, by = "Year")
us_ms_ct_age <- us_ms |>
  mutate(across(-Year, ~{.x = .x * `U.S. Mothership`})) |>
  select(-`U.S. Mothership`)

us_ct_ss <- ct |>
  select(Year, `U.S. Shoreside`)
us_ss <- prop_dfs$`US SS` |>
  left_join(us_ct_ss, by = "Year")
us_ss_ct_age <- us_ss |>
  mutate(across(-Year, ~{.x = .x * `U.S. Shoreside`})) |>
  select(-`U.S. Shoreside`)

# Sum Canada and US catch-at-age

can_caa <- can_ss_ct_age |>
  bind_rows(can_ft_ct_age) |>
  group_by(Year) |>
  summarise_all(sum, na.rm = TRUE)

us_caa <- us_cp_ct_age |>
  bind_rows(us_ms_ct_age) |>
  bind_rows(us_ss_ct_age) |>
  group_by(Year) |>
  summarise_all(sum, na.rm = TRUE)

tot_caa <- can_caa |>
  bind_rows(us_caa) |>
  group_by(Year) |>
  summarise_all(sum, na.rm = TRUE)

can_ratio <- can_caa |>
  left_join(tot_caa, by = "Year") %>%
  mutate(across(ends_with(".x"),
                ~ ./get(str_replace(cur_column(),
                                    ".x", ".y")))) |>
  select(-matches(".y$"))

names(can_ratio) <- names(can_ratio) %>%
  gsub(".x", "", .)

can_ratio <- can_ratio |>
  map_df(~{ifelse(is.nan(.x), 0, .x)})

us_ratio <-  us_caa |>
  left_join(tot_caa, by = "Year") %>%
  mutate(across(ends_with(".x"),
                ~ ./get(str_replace(cur_column(),
                                    ".x", ".y")))) |>
  select(-matches(".y$"))

names(us_ratio) <- names(us_ratio) %>%
  gsub(".x", "", .)

us_ratio <- us_ratio |>
  map_df(~{ifelse(is.nan(.x), 0, .x)})

# Check ratio dfs add to 1 by age
# j <- can_ratio |> bind_rows(us_ratio) |> group_by(Year) |>   summarise_all(sum, na.rm = TRUE)

# Get ratios of biomass
prop_biomass_df <- table_survey_by_country(survey_by_country_df, ret_df = T) |>
  select(year, canada.prop, us.prop) |>
  rename(Year = year) |>
  mutate(canada.prop = as.numeric(canada.prop)) |>
  mutate(us.prop = as.numeric(us.prop))

# Get biomass at age combined
biomass_at_age_df <- table_at_age(
  base_model,
  type = "baa",
  end_yr = end_yr,
  ret_df = TRUE) |>
  mutate(Year = as.numeric(Year))

j <- biomass_at_age_df |>
  right_join(prop_biomass_df, by = "Year")

can_baa <- j |>
  mutate(across(!Year & !canada.prop & !us.prop, ~{.x * canada.prop})) |>
  select(-canada.prop, -us.prop)

us_baa <- j |>
  mutate(across(!Year & !canada.prop & !us.prop, ~{.x * us.prop})) |>
  select(-canada.prop, -us.prop)

