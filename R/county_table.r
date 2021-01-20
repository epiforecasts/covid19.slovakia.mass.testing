##' Produce a table of prevalence ratios in counties
##'
##' @importFrom dplyr mutate select arrange
##' @importFrom kableExtra kable add_header_above save_kable
##' @param file file name to save the table to (pdf)
##' @return NULL (invisibly)
county_table <- function(file = NULL) {

  ms.R <- regression()$covariates

  options(knitr.kable.NA = "")

  print_table <- ms.R %>%
    mutate(round2_prev = round(round2_prev * 100, 2),
           round3_prev = round(positive_3 / attendance_3 * 100, 2),
           R = round(R, 1)) %>%
    select(county, region, pop, R,
           ends_with("_2"), round2_prev,
           ends_with("_3"), round3_prev) %>%
    arrange(county)

  if (!is.null(file)) {
    col.names <-
      c("County", "Region", "Population", "R",
        rep(c("Positive", "Attendance", "%"), times = 2))

    k <- kable(print_table,
               format = "latex",
               col.names = col.names,
               align=c('l|', 'l|', 'r|', 'r|', rep('r', 3), '|r', rep('r', 2)),
               booktabs = TRUE)

    k <-
      add_header_above(k, c(" " = 4,
                            "Round 1" = 3, "Round 2" = 3))

    save_kable(k, file)
    invisible(NULL)
  } else {
    return(print_table)
  }
}
