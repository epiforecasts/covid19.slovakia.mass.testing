library("vroom")
library("DescTools")

df <- vroom(here::here("data-raw", "data", "rt", "prevalence-samples.csv")) %>%
  filter(county %in% c("pilot", "non-pilot")) %>%
  select(county, sample,
         pred_prev1 = "2020-10-31", pred_prev2 = "2020-11-07") %>%
  nest(samples = c(sample, pred_prev1, pred_prev2))

data("ms.tst")

uncertain_saRR <- function(x1, n1, x2, n2, samples,
                           quantiles = c(0.025, 0.5, 0.975)) {
  x1_sum <- sum(x1)
  n1_sum <- sum(n1)
  x2_sum <- sum(x2)
  n2_sum <- sum(n2)

  samples_sum <- bind_rows(samples) %>%
    group_by(sample) %>%
    summarise_all(sum)

  ## take beta sample
  samples_sum$cprev1 <-
    rbeta(length(unique(samples_sum$sample)), x1_sum, n1_sum - x1_sum)
  samples_sum$cprev2 <-
    rbeta(length(unique(samples_sum$sample)), x2_sum, n2_sum - x2_sum)

  ratios <- samples_sum %>%
    group_by(sample) %>%
    summarise(cratio = (cprev2 / cprev1),
              pred_ratio = (pred_prev2 / pred_prev1), .groups = "drop") %>%
    ungroup() %>%
    mutate(sarr = cratio / pred_ratio) %>%
    summarise(sarr = quantile(sarr, quantiles), quantile = quantiles,
              cratio = quantile(cratio, quantiles), quantile = quantiles,
              pred_ratio = quantile(pred_ratio, quantiles), quantile = quantiles,
              .groups = "drop")

  return(list(ratios))
}

sarr_by_pilot <- ms.tst %>%
  filter(!is.na(attendance_3)) %>%
  simplify_names() %>%
  mutate(county =
           if_else(simple_name %in%
                   c("tvrdosin", "namestovo", "dolny_kubin", "bardejov"),
                   "pilot", "non-pilot")) %>%
  group_by(county) %>%
  summarise_if(is.numeric, sum, na.rm = TRUE) %>%
  ungroup() %>%
  left_join(df, by = "county") %>%
  group_by(county) %>%
  summarise(sapr = uncertain_saRR(positive_2, attendance_2,
                                  positive_3, attendance_3, samples),
            .groups = "drop") %>%
  unnest(cols = c(sapr))

sarr_total <- ms.tst %>%
  filter(!is.na(attendance_3)) %>%
  simplify_names() %>%
  group_by(county) %>%
  mutate(county =
           if_else(simple_name %in%
                   c("tvrdosin", "namestovo", "dolny_kubin", "bardejov"),
                    "pilot", "non-pilot")) %>%
  group_by(county) %>%
  summarise_if(is.numeric, sum, na.rm = TRUE) %>%
  left_join(df, by = "county") %>%
  ungroup() %>%
  summarise(sapr = uncertain_saRR(positive_2, attendance_2,
                                  positive_3, attendance_3, samples),
            .groups = "drop") %>%
  unnest(cols = c(sapr))

