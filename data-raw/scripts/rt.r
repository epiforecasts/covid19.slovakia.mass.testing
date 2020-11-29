# Packages -----------------------------------------------------------------
library(EpiNow2)
library(data.table)
library(future)
library(here)
library(readxl)

# Update delays -----------------------------------------------------------
generation_time <- readRDS(here::here("data-raw", "data", "generation_time.rds"))
incubation_period <- readRDS(here::here("data-raw", "data", "incubation_period.rds"))
reporting_delay <- readRDS(here::here("data-raw", "data", "reporting_delay.rds"))

# Get cases  ---------------------------------------------------------------
# read in and convert to data.table
cases <- read_excel(here::here("data-raw", "data", "data_covid_okresy.xlsx"), sheet = "data")
cases <- setDT(cases)

# make long format
cases <- melt(cases, id.vars = "Date", variable.name = "region", value.name = "confirm")

# change variable type to required input
cases <- cases[, .(date = as.Date(Date), 
                   region = as.character(region),
                   confirm = as.integer(confirm))]

# get rid of internal total
cases <- cases[!(region %in% "grand_total")]

# add pilot regions
pilot_regions <- c("tvrdosin", "dolny_kubin", "namestovo", "bardejov")
pilot <- copy(cases)[region %in% pilot_regions]
pilot <- pilot[date <= "2020-10-22"]
sum_pilot <- copy(pilot)[, .(confirm = sum(confirm)), by = date]
sum_pilot <- sum_pilot[, region := "pilot"]

# add non-pilot regions
non_pilot <- copy(cases)[!(region %in% pilot_regions)]
sum_non_pilot <- copy(non_pilot)[, .(confirm = sum(confirm)), by = date]
sum_non_pilot <- sum_non_pilot[, region := "non-pilot"]

## all reginos
sum_all <- copy(cases)[, .(confirm = sum(confirm)), by = date]
sum_all <- sum_all[, region := "all"]

# link back together
cases <- rbindlist(list(pilot, sum_pilot, non_pilot, sum_non_pilot, sum_all),
                   use.names = TRUE)
setorder(cases, region, date)

# Run Rt estimation -------------------------------------------------------
# set up parallel cores
setup_future(cases, min_cores_per_worker = 4)
options(mc.cores = 4)

# estimate Rt by region
regional_epinow(reported_cases = cases,
                generation_time = generation_time,
                delays = delay_opts(incubation_period, reporting_delay),
                horizon = 8,
                rt = rt_opts(prior = list(mean = 1.0, sd = 0.2), future = -7),
                stan = stan_opts(samples = 4000, warmup = 1000, chains = 4),
                target_folder = here("data-raw", "data", "rt", "samples"),
                logs = here("data-raw", "data", "rt", "logs"),
                output = c("samples", "plots", "latest"))

# shut down cluster
future::plan("sequential")

# summarise output
regional_summary(reported_cases = cases,
                 results_dir = here("data-raw", "data", "rt", "samples"),
                 summary_dir = here("data-raw", "data", "rt"),
                 region_scale = "county", all_regions = TRUE)
