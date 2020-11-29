##' Plot mobility data for Slovakia
##'
##' @return a plot with mobility dat
##' @importFrom dplyr filter select rename mutate
##' @importFrom tidyr pivot_longer
##' @importFrom ggplot2 ggplot aes geom_line facet_wrap ylab theme_classic
##' @importFrom lubridate ymd
##' @export
mobility <- function() {
  mob.slo %>%
    select(date, retail_and_recreation_percent_change_from_baseline,
           grocery_and_pharmacy_percent_change_from_baseline,
           parks_percent_change_from_baseline,
           transit_stations_percent_change_from_baseline,
           workplaces_percent_change_from_baseline,
           residential_percent_change_from_baseline) %>%
    rename('Retail and recreation' = retail_and_recreation_percent_change_from_baseline,
           'Grocery and Pharmacy' = grocery_and_pharmacy_percent_change_from_baseline,
           'Parks' = parks_percent_change_from_baseline,
           'Transit stations' = transit_stations_percent_change_from_baseline,
           'Workplaces' = workplaces_percent_change_from_baseline,
           'Resitential' = residential_percent_change_from_baseline) %>%
    pivot_longer(-date, values_to = "value", names_to = "Location") %>%
    mutate(date = ymd(date)) %>%
    ggplot(aes(x=date, y=value, group=Location)) +
    geom_line() +
    facet_wrap(~Location, scale="free") +
    ylab("percent change from baseline") +
    theme_classic()
}
