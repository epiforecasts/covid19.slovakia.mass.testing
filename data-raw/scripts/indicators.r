# Packages ----------------------------------------------------------------
library("readxl")
library("dplyr")
library("tidyr")
library("janitor")
library("brms")
library("GGally")
library("ggplot2")

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

income <-
  read_excel(here::here("data-raw", "data", "indicators",
                        "slovakia_income_by_region.xlsx"),
             skip = 4) %>%
  clean_names() %>%
  mutate(income = as.integer(sub(",", ".", x2019))) %>%
  select(region = x1, income)

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

# all covariates and outcome
prev_long <- ms.tst %>%
  mutate(region = if_else(grepl("Košice", county), "Košický kraj", region)) %>%
  mutate(pilot = !is.na(attendance_1)) %>% 
  select(county, region, attendance_2, positive_2, pilot) %>%
  left_join(unemp, by = "county") %>%
  left_join(age, by = "county") %>%
  left_join(pop_dens, by = "county") %>%
  mutate(county = sub(" [IV]+$",  "", county)) %>%
  group_by(county, region, pilot) %>%
  summarise(attendance_2 = sum(attendance_2),
            positive_2 = sum(positive_2),
            active = sum(active),
            unemployed = sum(unemployed),
            mean_age = mean(mean_age),
            pop_dens = mean(pop_dens),
            .groups = "drop") %>%
  ungroup() %>%
  mutate(unemp_rate = unemployed / active) %>%
  left_join(roma, by = "county") %>%
  left_join(income, by = "region") %>%
  select(county, region, ends_with("_2"), pilot, mean_age, pop_dens, unemp_rate,
         proportion_roma, income) %>%
  pivot_longer(c(-county, -region, -attendance_2, -positive_2, -pilot)) %>%
  group_by(name) %>%
  mutate(value = (value - mean(value)) / sd(value)) %>%
  ungroup()

# visual prev vs covariate
scatter <- prev_long %>% 
  mutate(prev = positive_2 / attendance_2) %>% 
  ggplot(aes(y = value, x = prev, col = region)) +
  geom_point() +
  facet_wrap(~name, scales = "free_y") +
  labs(y = "Variable value", x = "Prevalence") +
  guides(col = guide_legend(title = "Region")) +
  theme_classic() +
  theme(legend.position = "bottom")

scatter 

plot_dir <- here::here("data-raw", "figures")
if (!dir.exists(plot_dir)) {
  dir.create(plot_dir)
}
ggsave(paste0(plot_dir, "/scatter.png"), scatter, width = 9, height = 6)

# analysis format
prev <- prev_long %>%
  pivot_wider()

# plot correlations and data relationships
correlation <- prev %>% 
  mutate(prev = positive_2 / attendance_2) %>% 
  select(-county, -region, -attendance_2, -positive_2) %>%
  ggpairs()

correlation

ggsave(paste0(plot_dir, "/correlation.png"), correlation, width = 9, height = 9)

# Define beta binomial ----------------------------------------------------
beta_binomial2 <- custom_family(
  "beta_binomial2", dpars = c("mu", "phi"),
  links = c("logit", "log"), lb = c(NA, 2),
  type = "int", vars = "vint1[n]"
)

stan_funs <- "
  real beta_binomial2_lpmf(int y, real mu, real phi, int T) {
    return beta_binomial_lpmf(y | T, mu * phi, (1 - mu) * phi);
  }
  int beta_binomial2_rng(real mu, real phi, int T) {
    return beta_binomial_rng(T, mu * phi, (1 - mu) * phi);
  }
"
stanvars <- stanvar(scode = stan_funs, block = "functions")

# Priors ------------------------------------------------------------------
priors <- c(prior("exponential(1)", class = "phi"))

# Define models --------------------------------------------------------------
models <- list()

# binomial
models[["linear"]] <- 
  as.formula(positive_2 | trials(attendance_2) ~ pilot + mean_age + pop_dens + unemp_rate + 
               proportion_roma + income + (1 | region))

models[["linear_by_region"]] <-
  as.formula(positive_2 | trials(attendance_2) ~  pilot + mean_age + pop_dens + unemp_rate + 
               proportion_roma + income + (pilot + mean_age + pop_dens + unemp_rate + proportion_roma | region))


models[["spline"]]  <- 
  as.formula(positive_2 | trials(attendance_2) ~  pilot + s(mean_age, k = 3) + s(pop_dens, k = 3) + 
               s(unemp_rate, k = 3) + s(proportion_roma, k = 3) + s(income, k = 3) + (1 | region))

# beta binomial
models[["beta_linear"]] <- 
  as.formula(positive_2 | vint(attendance_2) ~ pilot + mean_age + pop_dens + unemp_rate + 
               proportion_roma + income + (1 | region))

models[["beta_linear_by_region"]] <-
  as.formula(positive_2 | vint(attendance_2) ~  pilot + mean_age + pop_dens + unemp_rate + 
               proportion_roma + income + (pilot + mean_age + pop_dens + unemp_rate + proportion_roma | region))


models[["beta_spline"]]  <- 
  as.formula(positive_2 | vint(attendance_2) ~  pilot + s(mean_age, k = 3) + s(pop_dens, k = 3) + 
               s(unemp_rate, k = 3) + s(proportion_roma, k = 3) + s(income, k = 3) + (1 | region))

# Fit models --------------------------------------------------------------

bin_fits <- lapply(models[!grepl("beta_", names(models))], brm, data = prev, family = binomial(), 
               control = list(adapt_delta = 0.99, max_treedepth = 12))
beta_fits <- lapply(models[grepl("beta_", names(models))], brm, data = prev, family = beta_binomial2, prior = priors,
               control = list(adapt_delta = 0.99, max_treedepth = 12),stanvars = stanvars)

fits <- c(bin_fits, beta_fits)

# Log lik and prediction --------------------------------------------------
expose_functions(fits[[1]], vectorize = TRUE)

log_lik_beta_binomial2 <- function(i, prep) {
  mu <- brms:::get_dpar(prep, "mu", i = i)
  phi <- brms:::get_dpar(prep, "phi", i = i)
  trials <- prep$data$vint1[i]
  y <- prep$data$Y[i]
  beta_binomial2_lpmf(y, mu, phi, trials)
}

posterior_predict_beta_binomial2 <- function(i, prep, ...) {
  mu <- prep$dpars$mu[, i]
  phi <- prep$dpars$phi
  trials <- prep$data$vint1[i]
  beta_binomial2_rng(mu, phi, trials)
}

# Compare fits ------------------------------------------------------------
loos <- lapply(fits, loo)
loo_compare(loos)

# Model diagnostics -------------------------------------------------------
best_fit <- fits[["linear"]]

yhat <- posterior_predict(best_fit)
colnames(yhat) <- prev$county

plotpp <- as_tibble(yhat) %>%
  pivot_longer(everything(), names_to = "county") %>%
  group_by(county) %>%
  summarise(median = median(value),
            lower = quantile(value, 0.25),
            upper = quantile(value, 0.75),
            lowest = quantile(value, 0.05),
            uppest = quantile(value, 0.95),
            .groups = "drop") %>%
  ungroup() %>%
  left_join(prev, by = "county") %>%
  arrange(median) %>%
  mutate(id = seq_len(n()))

p <- ggplot(plotpp, aes(x = id, y = median)) +
  geom_point(aes(y = positive_2), size = 0.25) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3) +
  geom_ribbon(aes(ymin = lowest, ymax = uppest), alpha = 0.15) +
  xlab("County") +
  ylab("Estimated # positive") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

p

ggsave(paste0(plot_dir, "/posterior_predictions.png"), p, width = 7, height = 7)

# Plot effects ------------------------------------------------------------
plots <- plot(conditional_effects(best_fit, re_formula = NULL), 
              rug = TRUE, ask = FALSE)

plotted <- lapply(1:length(plots), function(i){
  ggsave(paste0(plot_dir, "/conditional_effect_", i, ".png"), 
         plots[[i]], height = 7, width = 7)
})


