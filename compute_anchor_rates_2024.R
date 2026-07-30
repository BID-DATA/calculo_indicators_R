# One-time/periodic script: gets each country's 2024 poverty rates under the
# $8.30/day international line and the $4.20/day extreme international line
# directly from the World Bank's published estimates (rather than computing
# them from our own survey aggregates). These rates are the rank-anchors used
# by var_ANCHOR2024.R: for every other year, we find the income percentile
# that matches this rate in that year's own distribution and classify people
# below it as poor - so the anchor itself should reflect the WB's official
# calibrated rate, not our uncalibrated base survey.

library(dplyr)

wb_path <- "C:/Users/DCOR/Inter-American Development Bank Group/Poverty Group - Documentos/Data/Income Adjust/world bank - poverty ratio.csv"

wb <- read.csv(wb_path, stringsAsFactors = FALSE)

wb_2024 <- wb %>%
  filter(reporting_year == 2024, poverty_line %in% c(4.2, 8.3)) %>%
  # a few countries (e.g. China) report multiple reporting_level rows
  # (national/rural/urban) for the same year - prefer "national", and
  # otherwise fall back to whichever single level is available (e.g.
  # Argentina, whose EPH survey is urban-only), keeping exactly one
  # row per country/poverty_line
  group_by(country_code, poverty_line) %>%
  filter(reporting_level == "national" | !any(reporting_level == "national")) %>%
  slice(1) %>%
  ungroup()

anchor_rate_2024 <- wb_2024 %>%
  filter(poverty_line == 8.3) %>%
  transmute(pais_c = country_code, anchor_rate_2024 = headcount)

anchor_rate_2024_extreme <- wb_2024 %>%
  filter(poverty_line == 4.2) %>%
  transmute(pais_c = country_code, anchor_rate_2024_extreme = headcount)

anchor_rates <- full_join(anchor_rate_2024, anchor_rate_2024_extreme, by = "pais_c") %>%
  arrange(pais_c)

write.csv(anchor_rates, "Inputs/anchor_rates_2024.csv", row.names = FALSE)
message(paste("Done. Wrote Inputs/anchor_rates_2024.csv with", nrow(anchor_rates), "countries from World Bank data."))
