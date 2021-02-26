library("readxl")
library("here")
library("tidyverse")
library("lubridate")
library("stringi")
library("readr")
library("vroom")
library("janitor")

###########################
# PCR incidence data
read_excel(here::here("data-raw", "data", "COVID_pozitiv_Okr_20201112.xlsx"), skip = 2) %>%
  janitor::clean_names() %>%
  rename(date = datum,
         region = kraj,
         county = okres,
         PCR.pos = pocet_pozitivnych_pripadov_za_dany_den) %>%
  mutate(pil = as.factor(0 + (county %in% c("Námestovo","Tvrdosín","Dolný Kubín","Bardejov")) )) %>%
  mutate(pilot = recode(pil, '0'="Other regions", '1'="Pilot regions")) -> PCR.inc
save(PCR.inc, file = here::here("data", "PCR.inc.rdata"))

###############
# Testing data (prevalence)
ms.tst <- suppressMessages(
  read_excel(here::here("data-raw", "data", "Plosne testovanie.xlsx"), skip = 1, n_max = 81)) %>%
  janitor::clean_names() %>%
  rename(pop = x2,
         attendance_1 = pilot_23_to_25_oct,
         attendance_prop_1 = x4,
         positive_1 = x5,
         positive_prop_1 = x6,
         se_1 = x7,
         min_1 = x8,
         max_1 = x9,
         attendance_2 = first_round_31_oct_and_1_nov,
         attendance_prop_2 = x11,
         positive_2 = x12,
         positive_prop_2 = x13,
         se_2 = x14,
         min_2 = x15,
         max_2 = x16,
         growth_min_2 = x17,
         growth_max_2 = x18,
         R_min_2 = x19,
         R_max_2 = x20,
         attendance_3 = second_round_7_and_8_nov,
         attendance_prop_3 = x22,
         positive_3 = x23,
         positive_prop_3 = x24,
         se_3 = x25,
         min_3 = x26,
         max_3 = x27,
         growth_min_3 = x28,
         growth_max_3 = x29,
         R_min_3 = x30,
         R_max_3 = x31) %>%
  filter(!is.na(county)) %>%
  mutate_at(vars(-county), as.numeric) %>%
  filter(!is.na(pop)) %>%
  mutate(pilot = !is.na(attendance_1)) %>%
  left_join(PCR.inc[!duplicated(PCR.inc$county),c("county","region")],by="county") %>%
  mutate(region = ifelse(str_sub(county,start=1,end=10)=="Bratislava","Bratislavský kraj",region)) %>%
  mutate(region = ifelse(str_sub(county,start=1,end=6)=="Košice","Košický kraj",region))
save(ms.tst, file = here::here("data", "ms.tst.rdata"))

## Google mobility data
read_csv("https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv") %>%
  filter(country_region == "Slovakia",
         date > ymd("2020-01-01"),
         date < ymd("2020-11-20"),
         !is.na(sub_region_1)) -> mob.slo
save(mob.slo, file = here::here("data", "mob.slo.rdata"))

## Reproduction numbers
Rts <- read_csv(here::here("data-raw/data/rt/rt.csv"))

## Rt estimate from the 22nd of October in all counties as the
## day before mass testing
## for some counties this will be as estimated last day of observed data - 7 days
## this assumes no change beyond the support of the data
Rt.county <- Rts %>%
  filter(date == "2020-10-22") %>%
  select(county, R = median)

save(Rt.county, file = here::here("data", "R.county.rdata"))

prevalence.samples <-
  vroom(here::here("data-raw", "data", "rt", "prevalence-samples.csv"))
save(prevalence.samples, file = here::here("data", "prevalence.samples.rdata"))

## Prevalence covariates
unemp <-
  read_excel(here::here("data-raw", "data", "indicators", "MS_2011-1.xlsx"),
             sheet = "Tab1", skip = 9) %>%
  clean_names() %>%
  select(county = x1, active = x12, unemployed = x13)

roma <-
  read_excel(here::here("data-raw", "data", "indicators",
                        "Roma Atlas 2019-1.xlsx"),
             sheet = "RESULTS", skip = 1) %>%
  clean_names() %>%
  mutate(proportion_roma =
           (proportion_roma_upper - proportion_roma_lower) / 2) %>%
  mutate(county = sub("-", " - ", county))

age <-
  read_excel(here::here("data-raw", "data", "indicators",
                        "slovakia_mean_age_by_district.xlsx"),
             skip = 5) %>%
  clean_names() %>%
  select(county = x1, mean_age = x2019) %>%
  filter(grepl("^District of", county)) %>%
  mutate(county = sub("District of ", "", county),
         county = sub(" ", " ", county)) %>%
  mutate(county = recode(county, `Śaľa` = "Šaľa"))

pop_dens <-
  read_excel(here::here("data-raw", "data", "indicators",
                        "slovakia_pop_dens_by_district.xlsx"),
             skip = 5) %>%
  clean_names() %>%
  select(county = x1, pop_dens = x2019) %>%
  filter(grepl("^District of", county)) %>%
  mutate(county = sub("District of ", "", county),
         county = sub(" ", " ", county)) %>%
  mutate(county = recode(county, `Śaľa` = "Šaľa"))

covariates <- list(unemp = unemp,
                   roma = roma,
                   age = age,
                   pop_dens = pop_dens)

save(covariates, file = here::here("data", "covariates.rdata"))

