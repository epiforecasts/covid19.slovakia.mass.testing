library(EpiNow2)
library(data.table)
library(here)

samples <- get_regional_results(results_dir = here::here("data-raw", "data", "rt", "samples"), samples = TRUE)$estimates$samples

samples <- samples[variable %in% "growth_rate"][,.(county = region, date, sample, value, type)]

grs <- samples[, list(low_95 = quantile(value, 0.025), high_95 = quantile(value, 0.975), median = median(value)), by = list(county, date, type)]

grs[date == "2020-10-23" & county == "pilot"]
grs[date == "2020-10-30" & county == "non-pilot"]

fwrite(grs, here("data-raw", "data", "rt", "growth-rates.csv"))
