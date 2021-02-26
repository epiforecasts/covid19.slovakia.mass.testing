##' Produce a table of prevalence ratios in counties
##'
##' @importFrom dplyr mutate select arrange matches left_join
##' @importFrom kableExtra kable add_header_above save_kable
##' @importFrom tools file_ext
##' @param file file name to save the table to (pdf)
##' @return NULL (invisibly)
##' @export
county_table <- function(file = NULL) {

  options(knitr.kable.NA = "")

  ms.R <- ms.tst %>%
    dplyr::select(county, region, pop,
                  dplyr::matches("attendance_[0-9]"),
                  dplyr::matches("positive_[0-9]")) %>%
    simplify_names() %>%
    dplyr::left_join(Rt.county %>%
                     rename(simple_name = county), by = "simple_name") %>%
    dplyr::select(-simple_name) %>%
    mutate(pop = round(pop))

  additional_rows <- ms.R %>%
    mutate(county = if_else(!is.na(attendance_1),
                            "Pilot and Round 1 & 2", NA_character_),
           county = if_else(is.na(county) & !is.na(attendance_3),
                            "Round 1 & 2", county),
           R = NA_real_) %>%
    replace_na(list(county = "Round 1 only")) %>%
    group_by(county) %>%
    summarise_if(is.numeric, sum) %>%
    mutate(level = "zaggregate")

  print_table <- ms.R %>%
    mutate(level = "acounty") %>%
    bind_rows(additional_rows) %>%
    mutate(pilot_prev = round(positive_1 / attendance_1 * 100, 2),
           round2_prev = round(positive_2 / attendance_2 * 100, 2),
           round3_prev = round(positive_3 / attendance_3 * 100, 2),
           R = round(R, 1)) %>%
    select(level, county, region, pop, R,
           ends_with("_1"), pilot_prev,
           ends_with("_2"), round2_prev,
           ends_with("_3"), round3_prev) %>%
    arrange(level, county) %>%
    select(-level)

  if (!is.null(file)) {
    col.names <-
      c("County", "Region", "Population", "R",
        rep(c("Positive", "Attendance", "%"), times = 3))

    if (file_ext(file) == "pdf") {
      format <- "latex"
    } else if (file_ext(file) == "html") {
      format <- "html"
    }

    k <- kable(print_table,
               format = format,
               col.names = col.names,
               align = c('l', 'l', 'r', 'r', rep('r', 3), 'r',
                         rep('r', 2), 'r', rep('r', 2)),
               booktabs = TRUE) %>%
      pack_rows(index = c("County level" = nrow(ms.R),
                          "Aggregate level" = nrow(additional_rows)))

    k <-
      add_header_above(k, c(" " = 4, "Pilot" =  3,
                            "Round 1" = 3, "Round 2" = 3))

    save_kable(k, file)
    invisible(NULL)
  } else {
    return(print_table)
  }
}
