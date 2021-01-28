# Packages ----------------------------------------------------------------
library("readxl")
library("dplyr")
library("tidyr")
library("janitor")
library("brms")
library("GGally")
library("ggplot2")
library("sjPlot")

options(mc.cores = 4)

# Covariate ---------------------------------------------------------------
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

# Outcome -----------------------------------------------------------------
load(here::here("data", "ms.tst.rdata"))

unemp <- covariates$unemp
age <- covariates$age
pop_dens <- covariates$pop_dens
roma <- covariates$roma

## all covariates and outcome
prev <- ms.tst %>%
  select(county, region, pop, starts_with("attendance_"),
         starts_with("positive_"), pilot) %>%
  left_join(unemp, by = "county") %>%
  left_join(age, by = "county") %>%
  left_join(pop_dens, by = "county") %>%
  mutate(xcounty = sub(" [IV]+$",  "", county)) %>%
  left_join(roma %>% select(xcounty = county, proportion_roma),
            by = "xcounty") %>%
  simplify_names() %>%
  left_join(Rt.county %>%
            rename(simple_name = county), by = "simple_name") %>%
  select(-xcounty, -simple_name) %>%
  mutate(unemp_rate = unemployed / active) %>%
  select(county, region, pop, matches("^attendance_[123]"),
         matches("^positive_[123]"), mean_age,
         pop_dens, unemp_rate, proportion_roma, R)

## extract pilot variables
pilot <- prev %>%
  filter(!is.na(attendance_1)) %>%
  mutate(round_attendance_prec = attendance_1 / pop,
         round_prev_prec = positive_1 / attendance_1,
         round_R_prec = R) %>%
  select(county, starts_with("round_")) %>%
  pivot_longer(-county) %>%
  group_by(name) %>%
  mutate(value = (value - mean(value)) / sd(value)) %>%
  ungroup() %>%
  pivot_wider() %>%
  mutate(`0` = 0, `1` = 1, `2` = 1) %>%
  pivot_longer(matches("^[012]"),
               names_to = "round", values_to = "multiplier") %>%
  mutate(round = as.integer(round)) %>%
  pivot_longer(c(-county, -round, -multiplier)) %>%
  mutate(value = value * multiplier) %>%
  pivot_wider() %>%
  select(-multiplier)

## extract round 1 variables
round1 <- prev %>%
  filter(is.na(attendance_1)) %>%
  mutate(round_attendance_prec = attendance_2 / pop,
         round_prev_prec = positive_2 / attendance_2,
         round_R_prec = R) %>%
  select(county, starts_with("round_")) %>%
  pivot_longer(-county) %>%
  group_by(name) %>%
  mutate(value = (value - mean(value)) / sd(value)) %>%
  ungroup() %>%
  pivot_wider() %>%
  mutate(`0` = 0, `1` = 1) %>%
  pivot_longer(matches("^[01]"),
               names_to = "round", values_to = "multiplier") %>%
  mutate(round = as.integer(round)) %>%
  pivot_longer(c(-county, -round, -multiplier)) %>%
  mutate(value = value * multiplier) %>%
  pivot_wider() %>%
  select(-multiplier)

prev_long <- prev %>%
  pivot_longer(matches("^(positive|attendance)_[123]$")) %>%
  separate(name, c("name", "round"), sep = "_") %>%
  select(-R) %>%
  filter(!is.na(value)) %>%
  pivot_wider() %>%
  pivot_longer(c(-county, -region, -positive, -attendance, -round)) %>%
  group_by(name) %>%
  mutate(value = (value - mean(value, na.rm = TRUE)) /
           sd(value, na.rm = TRUE)) %>%
  ungroup() %>%
  pivot_wider() %>%
  group_by(county) %>%
  mutate(round = as.integer(round),
         round = if_else(rep(any(round == 1), n()), round - 1L, round - 2L)) %>%
  ungroup()

pilot_long <- prev_long %>%
  inner_join(pilot, by = c("county", "round"))
round1_long <- prev_long %>%
  inner_join(round1, by = c("county", "round"))

prev_long <- pilot_long %>%
  bind_rows(round1_long)

scatter <- prev %>%
  mutate(prev = positive / attendance) %>%
  ggplot(aes(y = value, x = prev, col = region)) +
  geom_point() +
  facet_wrap(~name, scales = "free_y") +
  labs(y = "Variable value", x = "Prevalence") +
  guides(col = guide_legend(title = "Region")) +
  theme_classic() +
  theme(legend.position = "bottom")

plot_dir <- here::here("data-raw", "figures")
if (!dir.exists(plot_dir)) {
  dir.create(plot_dir)
}
ggsave(paste0(plot_dir, "/scatter.png"), scatter, width = 9, height = 6)

# plot correlations and data relationships
correlation <- prev %>%
  mutate(prev = positive_2 / attendance_2) %>%
  select(-county, -region, -attendance_2, -positive_2) %>%
  ggpairs()

ggsave(paste0(plot_dir, "/correlation.png"), correlation, width = 9, height = 9)

# Define models --------------------------------------------------------------
bin_models <- list()
nb_models <- list()

# binomial
bin_models[["linear"]] <-
  as.formula(positive | trials(attendance) ~
               1 + mean_age + pop_dens + unemp_rate + proportion_roma +
                 round + round_attendance_prec + round_prev_prec + round_R_prec)

bin_models[["linear_offset"]] <-
  as.formula(positive | trials(attendance) ~
               1 + mean_age + pop_dens + unemp_rate + proportion_roma +
                 round + round_attendance_prec + round_prev_prec +
                 round_R_prec + (1 | county))

nb_models[["stefan"]] <-
  as.formula(positive ~
               0 + county + round + round_attendance_prec + round_prev_prec +
               round_R_prec + round:region + offset(log(attendance)))

nb_models[["linear"]] <-
  as.formula(positive ~
               0 + round + mean_age + pop_dens + unemp_rate +
               proportion_roma + round_attendance_prec + round_prev_prec +
               round_R_prec + offset(log(attendance)))

nb_models[["linear_offset"]] <-
  as.formula(positive ~
               1 + (1 | county) + round + mean_age + pop_dens + unemp_rate +
               proportion_roma + round_attendance_prec + round_prev_prec +
               round_R_prec + offset(log(attendance)))

## Fit models --------------------------------------------------------------
stefan <- brm(nb_models[["stefan"]], family = negbinomial(),
              data = prev_long %>%
                group_by(county) %>%
                add_count() %>%
                ungroup() %>%
                filter(n == 2))

bin_fits <- lapply(bin_models, brm, data = prev_long, family = binomial())
nb_fits <- lapply(nb_models, brm, data = prev_long, family = negbinomial())
poisson_fits <- lapply(nb_models, brm, data = prev_long, family = poisson())

names(bin_fits) <- paste0("bin_", names(bin_fits))
names(nb_fits) <- paste0("nb_", names(nb_fits))
names(poisson_fits) <- paste0("poisson_", names(nb_fits))

fits <- c(bin_fits, nb_fits, poisson_fits)

loos <- fits %>%
  lapply(add_criterion, "loo") %>%
  lapply(loo, save_psis = TRUE)
lc <- loo_compare(loos)
lc

best_fit <- fits[[rownames(lc)[2]]]

saveRDS(fits, here::here("data-raw", "data", "prev-cov-fits.rds"))

# Compare fits ------------------------------------------------------------
loos <- suppressWarnings(lapply(fits, loo))
lc <- loo_compare(loos)

lc
saveRDS(lc, here::here("data-raw", "data", "prev_model_comparison.rds"))

# Effects -----------------------------------------------------------------
# best fitting binomial
best_fit <- fits[[rownames(lc)[1]]]
summary(best_fit)
ranef(best_fit)

# Posterior density check -------------------------------------------------
pp <- lapply(names(fits), function(i) {pp_check(fits[[i]], nsamples = 100)})
names(pp) <- names(fits)
plotted <- lapply(names(pp), function(i) {
              ggsave(paste0(plot_dir, "/pp_", i, ".png"),
                     pp[[i]], height = 7, width = 7)
  })

yhat <- posterior_predict(best_fit)
colnames(yhat) <- paste(prev_long$county, prev_long$round, sep = "_")

plotpp <- as_tibble(yhat) %>%
  pivot_longer(everything(), names_to = "county_round") %>%
  separate(county_round, c("county", "round"), sep = "_") %>%
  mutate(round = as.integer(round)) %>%
  group_by(county, round) %>%
  summarise(median = median(value),
            lower = quantile(value, 0.25),
            upper = quantile(value, 0.75),
            lowest = quantile(value, 0.05),
            uppest = quantile(value, 0.95),
            .groups = "drop") %>%
  ungroup() %>%
  left_join(prev_long, by = c("county", "round")) %>%
  arrange(median) %>%
  mutate(id = seq_len(n())) %>%
  group_by(county) %>%
  mutate(round = if_else(rep(any(round == 2), n()), round + 1, round + 2)) %>%
  ungroup() %>%
  mutate(round = if_else(round == 1, "Pilot", paste("Round", round - 1)))

p <- ggplot(plotpp, aes(x = id, y = median, color = round)) +
  geom_point(aes(y = positive), size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, color = NA) +
  geom_ribbon(aes(ymin = lowest, ymax = uppest), alpha = 0.15, color = NA) +
  xlab("County") +
  ylab("Estimated # positive") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  scale_color_brewer("", palette = "Dark2") +
  scale_y_log10()

p

ggsave(paste0(plot_dir, "/posterior_predictions.png"), p, width = 7, height = 7)

# Plot effects ------------------------------------------------------------
plots <- plot(conditional_effects(best_fit, re_formula = ~ (1 | county)),
              rug = TRUE, ask = FALSE)

plotted <- lapply(1:length(plots), function(i){
  ggsave(paste0(plot_dir, "/conditional_effect_", i, ".png"),
         plots[[i]], height = 7, width = 7)
})

# Plot all effects
p <-
  plot_model(best_fit,
             axis.labels = c(mean_age = "Mean age",
                             pop_dens =  "Population density",
                             unemp_rate = "Unemployment rate",
                             proportion_roma = "Roma population",
                             round_attendance_prec = "Previous attendance",
                             round_prev_prec = "Previous prevalence",
                             round_R_prec = "Reproduction number",
                             round = "Round"),
             axis.title = "Prevalence risk ratio", title = "",
             group.terms = c(1, 1, 1, 1, 2, 1, 1, 1)) +
  theme_minimal() +
  geom_hline(yintercept = 1, linetype = "dashed") +
  ylim(c(0.25, 1.5))

ggsave(paste0(plot_dir, "/coefficients.png"), p, width = 4, height = 3.5)
ggsave(paste0(plot_dir, "/coefficients.pdf"), p, width = 4, height = 3.5)


sdd = prev_long %>%
  group_by(county) %>%
  add_count() %>%
  ungroup() %>%
  filter(n == 2)

yhat <- posterior_predict(stefan)
colnames(yhat) <- paste(sdd$county, sdd$round, sep = "_")

plotpp <- as_tibble(yhat) %>%
  pivot_longer(everything(), names_to = "county_round") %>%
  separate(county_round, c("county", "round"), sep = "_") %>%
  mutate(round = as.integer(round)) %>%
  group_by(county, round) %>%
  summarise(median = median(value),
            lower = quantile(value, 0.25),
            upper = quantile(value, 0.75),
            lowest = quantile(value, 0.05),
            uppest = quantile(value, 0.95),
            .groups = "drop") %>%
  ungroup() %>%
  left_join(sdd, by = c("county", "round")) %>%
  arrange(median) %>%
  mutate(id = seq_len(n())) %>%
  mutate(round = if_else(round == 0, "Round 1", "Round 2"),
         round = if_else(pilot == TRUE & round == "Round 1",
                         "Round 1 (pilot county)", round),
         round = if_else(pilot == TRUE & round == "Round 2",
                         "Round 2 (pilot county)", round))

p <- ggplot(plotpp, aes(x = id, y = median, color = round)) +
  geom_point(aes(y = positive), size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, color = NA) +
  geom_ribbon(aes(ymin = lowest, ymax = uppest), alpha = 0.15, color = NA) +
  xlab("County") +
  ylab("Estimated # positive") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  scale_color_brewer("", palette = "Set1")

p

