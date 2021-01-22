##' Regression analysis of prevalence reduction
##'
##' @return the result of a \code{glm} call
##' @importFrom dplyr mutate group_by summarise ungroup left_join select rename
##' @importFrom tidyr pivot_longer separate pivot_wider
##' @importFrom stats glm quasipoisson offset confint
##' @importFrom kableExtra kable add_header_above save_kable
##' @param file a file name to save the table of covariates to, if desired
##' @export
regression <- function(file = NULL) {
  ## convert county names to simple names for merging with Rt
  ms.R <- ms.tst %>%
    mutate(pop = round(pop)) %>%
    simplify_names() %>%
    mutate(region = if_else(grepl("Košice", county), "Košice kraj", region)) %>%
    mutate(region = sub(" kraj$", "", region)) %>%
    left_join(Rt.county %>% rename(simple_name = county), by = "simple_name") %>%
    mutate(pilot =
             if_else(simple_name %in%
                     c("tvrdosin", "namestovo", "dolny_kubin", "bardejov"),
                     "pilot", "non-pilot")) %>%
    select(county, pilot, pop, region, positive_1, attendance_1,
           positive_2, attendance_2, positive_3, attendance_3, R) %>%
    mutate(pilot_prev = positive_1 / attendance_1,
           round2_prev = positive_2 / attendance_2,
           round2_att = attendance_2 / pop,
           round2_mean_prev = mean(round2_prev, na.rm = TRUE),
           round2_mean_att = mean(round2_att, na.rm = TRUE),
           mean_R = mean(R))

  reg_data <- ms.R %>%
    filter(!is.na(attendance_3)) %>%
    pivot_longer(c(-county, -pilot, -pop, -region, -pilot_prev,
                   -starts_with("round2_"), -R, -mean_R)) %>%
    mutate(name = sub("^(.+)_([[:digit:]]+)$", "\\1|\\2", name)) %>%
    separate(name, c("name", "round"), "\\|") %>%
    pivot_wider() %>%
    mutate(prop_attend = attendance / pop,
           round = as.integer(round) - 2, # make integer indicator variable for rd 3
           round_rd2att = ifelse(round > 0, # centre and standardise
           (round2_att - round2_mean_att) / round2_mean_att,
           0),
           round_rd2pos = ifelse(round > 0, #centre and standardise
           (round2_prev - round2_mean_prev) / round2_mean_prev,
           0),
           round_R = ifelse(round > 0, #centre and standardise
           (R - mean_R) / mean_R,
           0))

  glm(positive ~ 0 + round + round_rd2att + round_rd2pos + round_R + round:region + county + offset(log(attendance)),
      family = quasipoisson,
      data = reg_data) -> mod

  round_coeff <- mod$coefficients["round"] %>% exp %>% {1-.} %>% round(2)
  round_coeff_confint <- mod %>% confint("round") %>% exp %>% {1-.} %>% round(2)

  return(list(res = mod, round = c(round_coeff, round_coeff_confint),
              covariates = ms.R))
}
