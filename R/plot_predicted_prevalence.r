##' Plot infection prevalence as predicted by EpiNow2
##'
##' @return ggplot object
##' @importFrom EpiNow2 calc_summary_measures
##' @param counties counties to plot, including the possibilities of "pilot" (for all pilot counties), "non-pilot" (for all non-pilot counties) and "all"
plot_predicted_prevalence <- function(counties = NULL) {
  prev <- prevalence.samples
  if (!is.null(counties)) {
    prev <- prev %>%
      filter(county %in% counties)
  }

  prev_summary <- calc_summary_measures(prev[, value := prev_ratio],
                                        summarise_by = "county",
                                        CrIs = 0.95)
}
