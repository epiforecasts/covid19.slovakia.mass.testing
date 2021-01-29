##' Plot bed occupancy in Slovakia
##'
##' To update the data, first run "data-raw/scripts/hospitalisations.r" and
##' update, e.g., with \code{devtools::load_all}
##' @return plot
##' @importFrom dplyr filter
##' @importFrom tibble tibble
##' @importFrom ggplot2 ggplot aes geom_col theme_minimal
bed_occupancy <- function() {
  truncated_occupancy <- slovakia_bed_occupancy %>%
    filter(date >= "2020-09-01")
  mass_testing_dates <-
    tibble(xmin = as.Date(c("2020-10-31", "2020-11-07")),
           xmax = as.Date(c("2020-11-01", "2020-11-08")),
           ymin = rep(0, 2),
           ymax = max(truncated_occupancy$value) * 1.1)
  p <- ggplot() +
    geom_col(data = truncated_occupancy,
             mapping = aes(x = date, y = value)) +
    geom_rect(data = mass_testing_dates,
              mapping = aes(xmin = xmin, xmax = xmax,
                            ymin = ymin, ymax = ymax),
              alpha = 0.35) +
    theme_minimal() +
    xlab("") + ylab("Beds occupied with COVID-19 patients")
  return(p)
}
