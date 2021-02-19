#' @title Regression analysis of prevalence
#' @param table_file (optional) file to save parameter table to
#' @return a list of two plots (effect size and posterior predicitons) and a
#' table of effect size estimates
#' @importFrom dplyr select left_join mutate filter group_by ungroup inner_join bind_rows recode_factor summarise arrange if_else
#' @importFrom tidyr pivot_longer pivot_wider separate
#' @importFrom stats as.formula median quantile
#' @importFrom brms brm negbinomial
#' @importFrom tidybayes spread_draws median_qi
#' @importFrom forcats fct_rev
#' @importFrom ggplot2 ggplot aes xlab ylab xlim geom_hline coord_flip theme_minimal geom_point geom_ribbon theme scale_color_brewer scale_y_log10
#' @importFrom ggdist geom_pointinterval
#' @importFrom rstantools posterior_predict
#' @importFrom tidyselect starts_with matches
#' @importFrom tibble as_tibble
#' @importFrom kableExtra kable save_kable
#' @export
regression <- function(table_file = NULL) {
  unemp <- covariates$unemp
  age <- covariates$age
  pop_dens <- covariates$pop_dens
  roma <- covariates$roma

  unemp <- unemp %>%
    dplyr::mutate(county = sub(" [IV]+$",  "", county)) %>%
    group_by(county) %>%
    summarise_all(sum)

  age <- age %>%
    dplyr::mutate(county = sub(" [IV]+$",  "", county)) %>%
    group_by(county) %>%
    summarise_all(mean)

  pop_dens <- pop_dens %>%
    dplyr::mutate(county = sub(" [IV]+$",  "", county)) %>%
    group_by(county) %>%
    summarise_all(mean)

  ## all covariates and outcome
  prev <- ms.tst %>%
    dplyr::select(county, region, pop, dplyr::starts_with("attendance_"),
                  dplyr::starts_with("positive_")) %>%
    dplyr::left_join(unemp, by = "county") %>%
    dplyr::left_join(age, by = "county") %>%
    dplyr::left_join(pop_dens, by = "county") %>%
    dplyr::left_join(roma %>% select(county, proportion_roma), by = "county") %>%
    simplify_names() %>%
    dplyr::left_join(Rt.county %>%
                     rename(simple_name = county), by = "simple_name") %>%
    dplyr::select(-simple_name) %>%
    dplyr::mutate(unemp_rate = unemployed / active) %>%
    dplyr::select(county, region, pop, tidyselect::matches("^attendance_[123]"),
                  tidyselect::matches("^positive_[123]"), mean_age,
                  pop_dens, unemp_rate, proportion_roma, R)

  ## extract pilot variables
  pilot <- prev %>%
    dplyr::filter(!is.na(attendance_1)) %>%
    dplyr::mutate(round_attendance_prec = attendance_1 / pop,
                  round_prev_prec = positive_1 / attendance_1,
                  round_R_prec = R) %>%
    dplyr::select(county, dplyr::starts_with("round_")) %>%
    tidyr::pivot_longer(-county) %>%
    dplyr::group_by(name) %>%
    dplyr::mutate(value = (value - mean(value)) / sd(value)) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_wider() %>%
    dplyr::mutate(`0` = 0, `1` = 1) %>%
    tidyr::pivot_longer(tidyselect::matches("^[012]"),
                        names_to = "round", values_to = "multiplier") %>%
    dplyr::mutate(round = as.integer(round)) %>%
    tidyr::pivot_longer(c(-county, -round, -multiplier)) %>%
    dplyr::mutate(value = value * multiplier) %>%
    tidyr::pivot_wider() %>%
    dplyr::select(-multiplier)

  ## extract round 1 variables
  round1 <- prev %>%
    dplyr::filter(is.na(attendance_1)) %>%
    dplyr::mutate(round_attendance_prec = attendance_2 / pop,
                  round_prev_prec = positive_2 / attendance_2,
                  round_R_prec = R) %>%
    dplyr::select(county, tidyselect::starts_with("round_")) %>%
    tidyr::pivot_longer(-county) %>%
    dplyr::group_by(name) %>%
    dplyr::mutate(value = (value - mean(value)) / sd(value)) %>%
    dplyr::ungroup() %>%
    pivot_wider() %>%
    dplyr::mutate(`0` = 0, `1` = 1) %>%
    tidyr::pivot_longer(tidyselect::matches("^[01]"),
                        names_to = "round", values_to = "multiplier") %>%
    dplyr::mutate(round = as.integer(round)) %>%
    tidyr::pivot_longer(c(-county, -round, -multiplier)) %>%
    dplyr::mutate(value = value * multiplier) %>%
    tidyr::pivot_wider() %>%
    dplyr::select(-multiplier)

  prev_long <- prev %>%
    tidyr::pivot_longer(tidyselect::matches("^(positive|attendance)_[123]$")) %>%
    tidyr::separate(name, c("name", "round"), sep = "_") %>%
    dplyr::select(-R) %>%
    dplyr::filter(!is.na(value)) %>%
    tidyr::pivot_wider() %>%
    tidyr::pivot_longer(c(-county, -region, -positive, -attendance, -round)) %>%
    dplyr::group_by(name) %>%
    dplyr::mutate(value = (value - mean(value, na.rm = TRUE)) /
                    sd(value, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_wider() %>%
    dplyr::group_by(county) %>%
    dplyr::mutate(round = as.integer(round),
                  round = dplyr::if_else(rep(any(round == 1), n()),
                                         round - 1L, round - 2L)) %>%
    dplyr::ungroup()

  pilot_long <- prev_long %>%
    dplyr::inner_join(pilot, by = c("county", "round"))
  round1_long <- prev_long %>%
    dplyr::inner_join(round1, by = c("county", "round"))

  prev_long <- pilot_long %>%
    dplyr::bind_rows(round1_long)

  model <-
    stats::as.formula(positive ~
                        1 + (1 | county) + round + mean_age + pop_dens +
                        unemp_rate + proportion_roma + round_attendance_prec +
                        round_prev_prec +
                        offset(log(attendance)))

  ## Fit model --------------------------------------------------------------
  fit <- brms::brm(model, data = prev_long, family = brms::negbinomial(),
                   iter = 3000)

  effects <- fit %>%
    tidybayes::spread_draws(`^b_.*`, regex = TRUE) %>%
    tidybayes::median_qi(.width = c(.95, .50)) %>%
    tidyr::pivot_longer(dplyr::starts_with("b_")) %>%
    dplyr::mutate(name =
                    dplyr::if_else(grepl("\\.", name), name,
                                   paste(name, .point, sep = "."))) %>%
    tidyr::separate(name, c("variable", "name"), sep = "\\.") %>%
    tidyr::pivot_wider() %>%
    dplyr::mutate(affects = dplyr::if_else(variable %in% c("b_Intercept",
                                                           "b_mean_age",
                                                           "b_pop_dens",
                                                           "b_unemp_rate",
                                                           "b_proportion_roma"),
                                           "Prevalence", "Reduction"),
                  variable =
                    dplyr::recode_factor(
                             variable,
                             b_Intercept = "Intercept",
                             b_mean_age = "Mean age",
                             b_pop_dens =  "Population density",
                             b_unemp_rate = "Unemployment rate",
                             b_proportion_roma = "Size of Marginalised Roma Community",
                             b_round = "Round",
                             b_round_attendance_prec = "Previous attendance",
                             b_round_prev_prec = "Previous prevalence",
                             b_round_R_prec = "Reproduction number"
                           ),
                  variable = forcats::fct_rev(variable))

  ep <-
    ggplot2::ggplot(effects %>%
                    dplyr::filter(variable != "Intercept") %>%
                    dplyr::mutate_at(vars(median, lower, upper), exp),
                    ggplot2::aes(x = variable, y = median,
                                 ymin = lower, ymax = upper,
                                 colour = affects)) +
    ggdist::geom_pointinterval() +
    ggplot2::xlab("") + ggplot2::ylab("Posterior ratio") +
    ggplot2::geom_hline(yintercept = 1, linetype = "dashed") +
    ggplot2::ylim(c(0.25, 1.75)) +
    ggplot2::coord_flip() +
    ggplot2::theme_minimal() +
    ggplot2::scale_color_brewer("", palette = "Set1") +
    ggplot2::theme(legend.position = "none")

  ## Posterior predictive plot -----------------------------------------------
  yhat <- rstantools::posterior_predict(fit)
  colnames(yhat) <- paste(prev_long$county, prev_long$round, sep = "_")

  plotpp <- tibble::as_tibble(yhat) %>%
    tidyr::pivot_longer(everything(), names_to = "county_round") %>%
    tidyr::separate(county_round, c("county", "round"), sep = "_") %>%
    dplyr::mutate(round = as.integer(round)) %>%
    dplyr::group_by(county, round) %>%
    dplyr::summarise(median = stats::median(value),
                     lower = stats::quantile(value, 0.25),
                     upper = stats::quantile(value, 0.75),
                     lowest = stats::quantile(value, 0.05),
                     uppest = stats::quantile(value, 0.95),
                     .groups = "drop") %>%
    dplyr::ungroup() %>%
    dplyr::left_join(prev_long, by = c("county", "round")) %>%
    dplyr::arrange(median) %>%
    dplyr::mutate(id = seq_len(n())) %>%
    dplyr::group_by(county) %>%
    dplyr::mutate(round = dplyr::if_else(rep(any(round == 2), n()),
                                         round + 1, round + 2)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(round = dplyr::if_else(round == 1, "Pilot",
                                         paste("Round", round - 1)))

  pp <- ggplot2::ggplot(plotpp, aes(x = id, y = median, color = round)) +
    ggplot2::geom_point(aes(y = positive), size = 1) +
    ggplot2::geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3,
                         color = NA) +
    ggplot2::geom_ribbon(aes(ymin = lowest, ymax = uppest), alpha = 0.15,
                         color = NA) +
    ggplot2::xlab("County") +
    ggplot2::ylab("Estimated # positive") +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = element_blank(),
                   axis.ticks.x = element_blank()) +
    ggplot2::scale_color_brewer("", palette = "Dark2") +
    ggplot2::scale_y_log10()

  if (!is.null(table_file)) {
    col.names <- c("Variable", "Lower 95%", "Median", "Upper 95%")
    k <- kableExtra::kable(effects %>%
                           dplyr::filter(.width == 0.95) %>%
                           dplyr::select(variable, lower, median, upper) %>%
                           dplyr::mutate_if(is.numeric, signif, digits = 2) %>%
                           dplyr::mutate_if(is.numeric, as.character),
                           format = "latex",
                           col.names = col.names,
                           align = c("l|", "r", "r", "r"),
                           booktabs = FALSE)

    kableExtra::save_kable(k, table_file)
  }


  return(list(plots = list(effects = ep,
                           predictions = pp),
              table = effects,
              covariates = prev))
}
