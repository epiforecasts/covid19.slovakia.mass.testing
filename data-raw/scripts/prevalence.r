# this script depends on data-raw/scripts/rt.r
# Packages ----------------------------------------------------------------
library(EpiNow2)
library(data.table)
library(here)

# Load samples ------------------------------------------------------------
# load from disk
samples <- get_regional_results(results_dir = here::here("data-raw", "data", "rt", "samples"),
                                samples = TRUE)$estimates$samples
# filter for infection posteriors
samples <- samples[variable %in% "infections"][,.(county = region, date, sample, value, type)]

# Calculate prevalence ----------------------------------------------------
# order by county and date, then taking a rolling sum of the last 7 days excluding today
setorder(samples, county, date)
samples <- samples[, lag_inc := shift(value, 2, "lag"), by = county]
samples <- samples[, prev := frollsum(lag_inc, n = 6, align = "right"), by = county]

prev_summary <- calc_summary_measures(samples, summarise_by = c("date", "county"), CrIs = 0.95)

npn <- prev_summary[county %in% c("pilot", "non-pilot")]

## extract dates of interest
samples <- samples[date == "2020-10-31" | date == "2020-11-07"]
samples <- samples[, .(county, date, sample, prev)]
prev <- dcast(samples, county + sample ~ date, value.var = "prev")

# make ratio
prev <- prev[, prev_ratio := `2020-10-31` / `2020-11-07`]

## save with other EpiNow2 results
fwrite(prev, here::here("data-raw", "data", "rt", "prevalence-samples.csv"))
