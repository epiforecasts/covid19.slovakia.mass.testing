# this script depends on data-raw/scripts/rt.r
# Packages ----------------------------------------------------------------
library(EpiNow2)
library(data.table)

# Load samples ------------------------------------------------------------
# load from disk
samples <- get_regional_results(results_dir = here("data-raw", "data", "rt", "samples"),
                                samples = TRUE)$estimates$samples
# filter for infection posteriors
samples <- samples[variable %in% "infections"][,.(county = region, date, sample, value, type)]

# Calculate prevalence ----------------------------------------------------
# order by county and date, then taking a rolling sum of the last 7 days excluding today
setorder(samples, county, date)
samples <- samples[, lag_inc := shift(value, 1, "lag"), by = county]
samples <- samples[, prev := frollsum(lag_inc, n = 6, align = "right"), by = county]

# extract dates of interest
samples <- samples[date == "2020-10-31" | date == "2020-11-07"]
samples <- samples[, .(county, date, sample, prev)]
prev <- dcast(samples, county + sample ~ date, value.var = "prev")

# make ratio
prev <- prev[, prev_ratio := `2020-10-31` / `2020-11-07`]

# Summarise and save prevalence ratio -------------------------------------
# summary measures and credible intervals
prev_summary <- calc_summary_measures(prev[, value := prev_ratio], 
                                      summarise_by = "county",
                                      CrIs = seq(0.05, 0.95, 0.05))
# save with other EpiNow2 results
fwrite(prev, here("data-raw", "data", "rt", "prevalence-samples.csv"))
fwrite(prev_summary, here("data-raw", "data", "rt", "prevalence.csv"))
