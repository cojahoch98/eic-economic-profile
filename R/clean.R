# Load and tidy the East India Company trade ledger (1660-1834).
#
# The raw file is already in "long" format: one row per
# year x commodity x port x direction. That is the shape the tidyverse
# works best with. The cleaning is deliberately small and explicit, so every
# change to the historians' numbers is visible and reviewable.

library(dplyr)
library(readr)

RAW_PATH <- "data/raw/eic_trade_1660_1834.csv"

# The two historians' ledgers meet at roughly 1760.
SOURCE_BOUNDARY_YEAR <- 1760

load_raw <- function(path = RAW_PATH) {
  # Read the CSV exactly as published; the text "N/A" means missing.
  read_csv(path, na = c("", "NA", "N/A"), show_col_types = FALSE)
}

clean_trade <- function(raw) {
  # Changes made (each one is a decision you can defend in an interview):
  #   1. Fix a commodity spelling; harmonise "lb"/"lbs" and port names.
  #   2. Rename the misspelt longitude columns ("lang" -> "lon").
  #   3. Turn the 0,0 lat/lon placeholders into real missing values.
  #   4. Rename "type" to "direction" (import/export) - clearer.
  #   5. Add source_era so every chart can show where the ledgers meet.
  #   6. Add is_zero - about 44% of rows record a value of 0. Some are true
  #      "nothing shipped" years, some are probably gaps. Keep, flag, decide
  #      per analysis.
  #   7. Add unit_price_gbp (value / quantity) where both are present.
  raw |>
    rename(direction = type, dest_lon = dest_lang, origin_lon = origin_lang) |>
    mutate(
      commodity = recode(commodity, "New Draparies" = "New Draperies"),
      unit      = recode(unit, "lbs" = "lb"),
      across(c(port, origin, destination), \(x) recode(x, "St_Helena" = "St Helena")),
      dest_unknown   = dest_lat == 0 & dest_lon == 0,
      origin_unknown = origin_lat == 0 & origin_lon == 0,
      dest_lat   = if_else(dest_unknown, NA_real_, dest_lat),
      dest_lon   = if_else(dest_unknown, NA_real_, dest_lon),
      origin_lat = if_else(origin_unknown, NA_real_, origin_lat),
      origin_lon = if_else(origin_unknown, NA_real_, origin_lon),
      source_era = if_else(year < SOURCE_BOUNDARY_YEAR,
                           "Chaudhuri (1660-1759)", "Bowen (1760-1834)"),
      is_zero = value == 0,
      unit_price_gbp = if_else(quantity > 0 & value > 0, value / quantity, NA_real_)
    ) |>
    select(-path_id, -dest_unknown, -origin_unknown)
}

load_clean <- function(path = RAW_PATH) {
  clean_trade(load_raw(path))
}
