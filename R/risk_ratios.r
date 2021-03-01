##' Calculate risk ratios for mass testing in Slovakia
##'
##' @return alist of two lists: "figures" (containing Fig. 2a and 2b in the manuscript) and "tables" (containing the total prevalence risk ratios in pilot regions ("pilot") and those regions that had two rounds of testing ("nation")
##' @importFrom dplyr filter rowwise mutate select
##' @importFrom tidyr pivot_longer separate pivot_wider
##' @importFrom ggplot2 ggplot aes geom_errorbar geom_point xlab scale_color_brewer scale_y_continuous position_dodge coord_flip theme_classic theme ylab geom_hline scale_color_manual scale_x_discrete
##' @importFrom scales percent
##' @importFrom epitools epitab
##' @export
risk_ratios <- function() {
  ret_fig <- list()
  ret_rr <- list()

  ms.tst %>%
    filter(pilot == T) %>%
    rowwise() %>%
    mutate(pRR_12 = epitab(c(attendance_1-positive_1,positive_1,attendance_2-positive_2,positive_2),method = "riskratio")$tab[2,c("riskratio")],
           pRR.lo_12 = epitab(c(attendance_1-positive_1,positive_1,attendance_2-positive_2,positive_2),method = "riskratio")$tab[2,c("lower")],
           pRR.hi_12 = epitab(c(attendance_1-positive_1,positive_1,attendance_2-positive_2,positive_2),method = "riskratio")$tab[2,c("upper")],
           pRR_23 = epitab(c(attendance_2-positive_2,positive_2,attendance_3-positive_3,positive_3),method = "riskratio")$tab[2,c("riskratio")],
           pRR.lo_23 = epitab(c(attendance_2-positive_2,positive_2,attendance_3-positive_3,positive_3),method = "riskratio")$tab[2,c("lower")],
           pRR.hi_23 = epitab(c(attendance_2-positive_2,positive_2,attendance_3-positive_3,positive_3),method = "riskratio")$tab[2,c("upper")],
           pRR_13 = epitab(c(attendance_1-positive_1,positive_1,attendance_3-positive_3,positive_3),method = "riskratio")$tab[2,c("riskratio")],
           pRR.lo_13 = epitab(c(attendance_1-positive_1,positive_1,attendance_3-positive_3,positive_3),method = "riskratio")$tab[2,c("lower")],
           pRR.hi_13 = epitab(c(attendance_1-positive_1,positive_1,attendance_3-positive_3,positive_3),method = "riskratio")$tab[2,c("upper")]) %>%
    select(county, pRR_12, pRR.lo_12, pRR.hi_12, pRR_23, pRR.lo_23, pRR.hi_23, pRR_13, pRR.lo_13, pRR.hi_13) -> pRR_pilot

  ret_fig[["b"]] <- pRR_pilot %>%
    pivot_longer(c(-county)) %>%
    separate(name, c("name", "round"), "_") %>%
    pivot_wider(names_from = name, values_from = value) %>%
    ggplot(aes(x=county, y=1-pRR, ymin=1-pRR.lo, ymax=1-pRR.hi, color = round)) +
    geom_errorbar(width = 0.0, position = position_dodge(width = 0.3)) +
    geom_point(position = position_dodge(width = 0.3)) +
    xlab("\n\n\n\n") + ylab("Reduction in prevalence\n(1 - prevalence risk ratio)") +
    scale_color_brewer(palette = "Dark2",
                       name = "Comparisons",
                       labels = c("pilot vs round 1", "pilot vs round 2", "round 1 vs 2")) +
    scale_y_continuous(labels = scales::percent,
                       breaks = c(0,.25,.5,.75,1),
                       limits = c(0,1)) +
    coord_flip() +
    theme_classic() +
    theme(legend.position = c(.15,.7))

  ## pilot prevalence RR overall
  ms.tst %>%
    filter(pilot == T) %>%
    select(attendance_1, attendance_2, attendance_3, positive_1, positive_2, positive_3) %>%
    summarise_all(sum) %>%
    mutate(pRR_12 = epitab(c(attendance_1-positive_1,positive_1,attendance_2-positive_2,positive_2),method = "riskratio")$tab[2,c("riskratio")],
           pRR.lo_12 = epitab(c(attendance_1-positive_1,positive_1,attendance_2-positive_2,positive_2),method = "riskratio")$tab[2,c("lower")],
           pRR.hi_12 = epitab(c(attendance_1-positive_1,positive_1,attendance_2-positive_2,positive_2),method = "riskratio")$tab[2,c("upper")],
           pRR_23 = epitab(c(attendance_2-positive_2,positive_2,attendance_3-positive_3,positive_3),method = "riskratio")$tab[2,c("riskratio")],
           pRR.lo_23 = epitab(c(attendance_2-positive_2,positive_2,attendance_3-positive_3,positive_3),method = "riskratio")$tab[2,c("lower")],
           pRR.hi_23 = epitab(c(attendance_2-positive_2,positive_2,attendance_3-positive_3,positive_3),method = "riskratio")$tab[2,c("upper")],
           pRR_13 = epitab(c(attendance_1-positive_1,positive_1,attendance_3-positive_3,positive_3),method = "riskratio")$tab[2,c("riskratio")],
           pRR.lo_13 = epitab(c(attendance_1-positive_1,positive_1,attendance_3-positive_3,positive_3),method = "riskratio")$tab[2,c("lower")],
           pRR.hi_13 = epitab(c(attendance_1-positive_1,positive_1,attendance_3-positive_3,positive_3),method = "riskratio")$tab[2,c("upper")]) %>%
    select(pRR_12, pRR.lo_12, pRR.hi_12, pRR_23, pRR.lo_23, pRR.hi_23, pRR_13, pRR.lo_13, pRR.hi_13) -> pRR_pilot_sum
  ret_rr[["pilot"]] <- 1-pRR_pilot_sum %>% round(2)

  ## mass test prevalence RR by county
  ms.tst %>%
    filter(!is.na(positive_2) & !is.na(positive_3) & !is.na(attendance_2) & !is.na(attendance_3)) %>%
    rowwise() %>%
    mutate(pRR = epitab(c(attendance_2-positive_2,positive_2,attendance_3-positive_3,positive_3),method = "riskratio")$tab[2,c("riskratio")],
           pRR.lo = epitab(c(attendance_2-positive_2,positive_2,attendance_3-positive_3,positive_3),method = "riskratio")$tab[2,c("lower")],
           pRR.hi = epitab(c(attendance_2-positive_2,positive_2,attendance_3-positive_3,positive_3),method = "riskratio")$tab[2,c("upper")]) %>%
    select(county, region, pRR, pRR.lo, pRR.hi) -> pRR_mass
  ret_rr[["county"]] <- pRR_mass

  ms.tst %>%
    filter(!is.na(positive_2) & !is.na(positive_3) & !is.na(attendance_2) & !is.na(attendance_3)) %>%
    select(positive_2, attendance_2, positive_3, attendance_3) %>%
    summarise_all(sum) %>%
    mutate(pRR = epitab(c(attendance_2-positive_2,positive_2,attendance_3-positive_3,positive_3),method = "riskratio")$tab[2,c("riskratio")],
           pRR.lo = epitab(c(attendance_2-positive_2,positive_2,attendance_3-positive_3,positive_3),method = "riskratio")$tab[2,c("lower")],
           pRR.hi = epitab(c(attendance_2-positive_2,positive_2,attendance_3-positive_3,positive_3),method = "riskratio")$tab[2,c("upper")]) %>%
    select(pRR, pRR.lo, pRR.hi) %>%
    mutate(county = "Overall")-> pRR_mass_sum
  ret_rr[["all"]] <- 1-(pRR_mass_sum %>% select(-county)) %>% round(2)

  ret_fig[["a"]] <- pRR_mass %>%
    ggplot(aes(x=interaction(region,county,lex.order = T), y=1-pRR, ymin=1-pRR.lo, ymax=1-pRR.hi,
               color = factor(region, rev(sort(unique(pRR_mass$region))))
    )) +geom_hline(yintercept = 1-pRR_mass_sum$pRR, alpha=.7, color="red") +
    geom_hline(yintercept = 1-pRR_mass_sum$pRR.lo, alpha=.5, color="red", lty="dotted") +
    geom_hline(yintercept = 1-pRR_mass_sum$pRR.hi, alpha=.5, color="red", lty="dotted") +
    geom_errorbar(width = 0.0) +
    geom_point() +
    xlab("") + ylab("Reduction in prevalence between mass testing rounds\n(1 - prevalence risk ratio)") +
    scale_color_manual(
      values = {
        clrs = rev(c("#636363","#3182bd","#969696","#6baed6","#bdbdbd","#9ecae1","#d9d9d9"))
        names(clrs) = rev(sort(unique(pRR_mass$region))); clrs
      },
      name = "Region"
    ) +
    scale_y_continuous(labels = scales::percent,
                       limits = c(0,1)) +
    scale_x_discrete(labels = make_labels) +
    coord_flip() +
    theme_classic() +
    theme(legend.position = c(.15,.85))

  ret_fig[["c"]] <- ggplot(slovakia_shape)+geom_sf(aes(fill = value.y), size=0.1)+facet_wrap(
    factor(testround, c("positive_prop_2", "positive_prop_3"), c("First round", "Second round"))~., ncol = 1
  )+theme_minimal()+scale_fill_gradient(
    "% Test positive", low = "#fff4f3", high = "#a00500", na.value="#EEEEEE", guide = "colourbar",
    breaks=seq(0, 0.04, 0.01), limits=c(0, 0.04), labels = scales::percent_format(accuracy = 1)
  )+theme(panel.grid = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(),
    legend.position = c(0.8, 0.01), strip.text = element_text(size = 12, hjust = 0.1, vjust=0)
  )+guides(fill = guide_colorbar(title.hjust = 0.3, title.position = "top", barwidth = 12, direction = "horizontal"))

  return(list(figures = ret_fig, tables = ret_rr))
}
