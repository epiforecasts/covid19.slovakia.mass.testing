##' Plot prevalence with confidence intervals as measured in national mass-testing in Slovakia
##'
##' @return plot of prevalence with exact binomial confidence intervals
##' @importFrom binom binom.confint
##' @importFrom dplyr select mutate
##' @importFrom ggplot2 ggplot geom_linerange geom_point geom_hline xlab ylab coord_flip theme_classic scale_colour_manual
##' @importFrom forcats fct_reorder
##' @export
prevalence <- function() {
  ms.tst %>%
    mutate(round_2 =
             if_else(is.na(attendance_3), "Not included", "Included")) %>%
    select(county, round_2, attendance_2, positive_2) %>%
    mutate(prev = positive_2 / attendance_2,
           prev.lo = binom.confint(positive_2, attendance_2,
                                   methods = "exact")$lower,
           prev.hi = binom.confint(positive_2, attendance_2,
                                   methods = "exact")$upper) %>%
    ggplot(aes(x = fct_reorder(county, prev),
               y = prev, ymin = prev.lo, ymax = prev.hi, colour = round_2)) +
    geom_linerange() +
    geom_point() +
    geom_hline(yintercept = 0.007, lty = "dashed") +
    xlab("") +
    ylab("Proportion of tests positive\nin the first round of mass testing") +
    coord_flip() +
    theme_classic() +
    scale_colour_manual("Round 2", values = c("red", "black"))
}
