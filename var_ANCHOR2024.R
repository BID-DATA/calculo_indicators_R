##### Rank-anchored poverty variables: percentile cutoffs anchored to each country's 2024
##### poverty rates under the $8.30/day line and the $4.20/day extreme line (see
##### compute_anchor_rates_2024.R), reapplied to the current year's own income distribution.

if (tipo == "encuestas") {

  anchor_rates_2024 <- read.csv("Inputs/anchor_rates_2024.csv")

  anchor_rate <- anchor_rates_2024 %>%
    filter(pais_c == pais) %>%
    pull(anchor_rate_2024)

  if (length(anchor_rate) == 1 && !is.na(anchor_rate)) {

    anchor_threshold <- reldist::wtd.quantile(data_filt$pc_ytot_ch, q = anchor_rate, weight = data_filt$factor_ci, na.rm = TRUE)

    data_filt <- data_filt %>%
      mutate(poor_category_anchored2024 = case_when(
        is.na(pc_ytot_ch) ~ NA_character_,
        pc_ytot_ch <= anchor_threshold ~ "poor_anchored2024",
        TRUE ~ "non_poor_anchored2024"
      ))

  } else {
    data_filt$poor_category_anchored2024 <- NA_character_
  }

  anchor_rate_extreme <- anchor_rates_2024 %>%
    filter(pais_c == pais) %>%
    pull(anchor_rate_2024_extreme)

  if (length(anchor_rate_extreme) == 1 && !is.na(anchor_rate_extreme)) {

    anchor_threshold_extreme <- reldist::wtd.quantile(data_filt$pc_ytot_ch, q = anchor_rate_extreme, weight = data_filt$factor_ci, na.rm = TRUE)

    data_filt <- data_filt %>%
      mutate(extreme_poor_category_anchored2024 = case_when(
        is.na(pc_ytot_ch) ~ NA_character_,
        pc_ytot_ch <= anchor_threshold_extreme ~ "extreme_poor_anchored2024",
        TRUE ~ "non_extreme_poor_anchored2024"
      ))

  } else {
    data_filt$extreme_poor_category_anchored2024 <- NA_character_
  }

}
