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
## January update
ms.tst <- suppressMessages(
  read_excel(here::here("data-raw", "data", "January results master.xlsx"), n_max = 81)) %>%
  janitor::clean_names() %>%
  rename(pop = population,
         attendance_1 = jan1_turnout_n,
         attendance_prop_1 = jan1_turnout_percent,
         positive_1 = jan1_poz,
         positive_prop_1 = jan1_tpr,
         attendance_2 = jan2_turnout_n,
         attendance_prop_2 = jan2_turnout_percent,
         positive_2 = jan2_poz,
         positive_prop_2 = jan2_tpr) %>%
  filter(!is.na(county)) %>%
  mutate_at(vars(-county, -code), as.numeric) %>%
  filter(!is.na(pop)) %>%
  left_join(PCR.inc[!duplicated(PCR.inc$county),c("county","region")],by="county") %>%
  mutate(region = ifelse(str_sub(county,start=1,end=10)=="Bratislava","Bratislavský kraj",region)) %>%
  mutate(region = ifelse(str_sub(county,start=1,end=6)=="Kosice","Kosický kraj",region)) %>%
  select(county, code, pop, region, starts_with("attendance_"), starts_with("positive_"))
save(ms.tst, file = here::here("data", "ms.tst.rdata"))

## Google mobility data
read_csv("https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv") %>%
  filter(country_region == "Slovakia",
         date > ymd("2020-10-01"),
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

