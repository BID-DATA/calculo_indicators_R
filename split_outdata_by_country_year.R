# One-time script: splits the master outdata.dta file into one .rds per
# country-year (pais_c/anio_c) so scl_indicators.R doesn't have to read the
# full master file on every run. Run manually via Rscript whenever the
# master file is regenerated. Input/output live under Downloads, not the
# OneDrive-synced Inputs/ folder, to avoid syncing large data files.
#
# .rds (with gzip compression) is used instead of .dta: write_dta() widens
# Stata's compact byte/int/float storage to plain doubles, which bloated the
# split to ~2.4x the size of the master file. saveRDS(..., compress = TRUE)
# keeps R's native types and gzip-compresses on top.

library(haven)
library(dplyr)

input_path <- "C:/Users/DCOR/Downloads/20260602_outdata/20260602_outdata/outdata.dta"
output_dir <- "C:/Users/DCOR/Downloads/20260602_outdata/20260602_outdata_split/"

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

message("Reading master file (this may take a while and a lot of memory)...")
master <- read_dta(input_path)

combos <- master %>% distinct(pais_c, anio_c) %>% arrange(pais_c, anio_c)
message(paste("Splitting into", nrow(combos), "country-year files..."))

for (i in seq_len(nrow(combos))) {
  p <- combos$pais_c[i]
  y <- combos$anio_c[i]
  subset_i <- master %>% filter(pais_c == p, anio_c == y)
  out_file <- file.path(output_dir, paste0(p, "_", y, "_BID.rds"))
  saveRDS(subset_i, out_file, compress = TRUE)
  if (i %% 25 == 0) message(paste("  ...", i, "/", nrow(combos)))
}

message("Done.")
