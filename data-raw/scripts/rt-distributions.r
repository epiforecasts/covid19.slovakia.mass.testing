# Packages ----------------------------------------------------------------
library(EpiNow2)
library(data.table)
library(future)
library(here)

# Save incubation period and generation time ------------------------------
# generation time from: https://www.medrxiv.org/content/10.1101/2020.09.04.20188516v2.full.pdf
# gamma distribution in SI: https://www.medrxiv.org/content/medrxiv/suppl/2020/09/16/2020.09.04.20188516.DC2/2020.09.04.20188516-1.pdf
shape <- 6.8004
rate <- 1.2344
generation_time <- list(mean = shape / rate, 
                        mean_sd = 0.5,
                        sd = sqrt(shape) / rate,
                        sd_sd = 0.25,
                        max = 15)
print("Generation time parameters (gamma)")
print(generation_time)
saveRDS(generation_time , here::here("data-raw", "data", "generation_time.rds"))

incubation_period <- get_incubation_period(disease = "SARS-CoV-2", source = "lauer")
print("Incubation period parameters (log normal)")
print(incubation_period)
saveRDS(incubation_period, here::here("data-raw", "data", "incubation_period.rds"))

# Fit Report distribution -------------------------------------------------
# use an approximate delay distribution based on local knowledge
# truncate the distribution at 30 days
reporting_delay <- list(mean = 1.25,
                        means_sd = 0.05,
                        sd = 1,
                        sd_sd = 0.05)
saveRDS(reporting_delay, here::here("data-raw", "data", "reporting_delay.rds"))

print("Reporting delay parameters (log normal)")
print(reporting_delay)
