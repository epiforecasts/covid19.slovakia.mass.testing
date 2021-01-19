##' Self-adjusted risk ratios
##'
##' @importFrom dplyr bind_rows group_by summarise_all summarise mutate select filter left_join
##' @importFrom tidyr nest unnest
##' @return a list of risk ratios by county, pilot vs. non-pilot, and overall
self_adjusted_risk_ratios <- function() {

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

  pvs <- prevalence.samples %>%
    select(county, sample,
           pred_prev1 = "2020-10-31", pred_prev2 = "2020-11-07") %>%
    nest(samples = c(sample, pred_prev1, pred_prev2))

  prevalences <- ms.tst %>%
    filter(!is.na(attendance_3)) %>%
    simplify_names()

  sarr <- list()

  sarr[["county"]] <- prevalences %>%
    group_by(county, simple_name) %>%
    summarise_if(is.numeric, sum, na.rm = TRUE) %>%
    ungroup() %>%
    left_join(pvs %>% rename(simple_name = county), by = "simple_name") %>%
    group_by(county) %>%
    summarise(sapr = uncertain_saRR(positive_2, attendance_2,
                                    positive_3, attendance_3, samples),
              .groups = "drop") %>%
    unnest(cols = c(sapr))

  sarr[["pilot"]] <- prevalences %>%
    mutate(county =
             if_else(simple_name %in%
                     c("tvrdosin", "namestovo", "dolny_kubin", "bardejov"),
                     "pilot", "non-pilot")) %>%
    group_by(county) %>%
    summarise_if(is.numeric, sum, na.rm = TRUE) %>%
    ungroup() %>%
    left_join(pvs, by = "county") %>%
    group_by(county) %>%
    summarise(sapr = uncertain_saRR(positive_2, attendance_2,
                                    positive_3, attendance_3, samples),
              .groups = "drop") %>%
    unnest(cols = c(sapr))

  sarr[["all"]] <- prevalences %>%
    summarise_if(is.numeric, sum, na.rm = TRUE) %>%
    mutate(county = "all") %>%
    left_join(pvs, by = "county") %>%
    group_by(county) %>%
    summarise(sapr = uncertain_saRR(positive_2, attendance_2,
                                    positive_3, attendance_3, samples),
              .groups = "drop") %>%
    unnest(cols = c(sapr))

  sarr <- lapply(sarr, function(x) {
    x %>%
      select(county, sarr, quantile) %>%
      pivot_wider(names_from = "quantile", values_from = "sarr") %>%
      rename(saRR = `0.5`, saRR.lo = `0.025`, saRR.hi = `0.975`)
  })

  return(sarr)
}
