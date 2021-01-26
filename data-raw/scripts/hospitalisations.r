library("dplyr")
library("readr")
library("here")

csv_url <-
  paste0("https://opendata.ecdc.europa.eu/covid19/hospitalicuadmissionrates/",
         "csv/data.csv")
slovakia_bed_occupancy <- read_csv(csv_url) %>%
  filter(country == "Slovakia", indicator == "Daily hospital occupancy")

save(slovakia_bed_occupancy, file = here::here("data", "bed_occupancy.rdata"))
