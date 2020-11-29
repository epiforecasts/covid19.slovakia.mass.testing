##' Plot PCR positive incidence and mass testing timings for Slovakia
##'
##' @return a plot with PCR positive incidence and mass testing timings
##' @importFrom dplyr filter group_by summarise
##' @importFrom ggplot2 ggplot aes geom_bar annotate geom_vline facet_wrap ylab xlab xlim theme_classic
##' @importFrom lubridate dmy
##' @export
pcr_incidence <- function() {
  PCR.inc %>%
    filter(!(pil == 1 & date>dmy("23/10/2020"))) %>%
    filter(!(pil == 0 & date>dmy("31/10/2020"))) %>%
    filter(date>=dmy("1/9/2020")) -> data

  ## plot by pilot vs non pilot
  data %>%
    group_by(date,pilot) %>% summarise(PCR.pos = sum(PCR.pos)) %>%
    ggplot(aes(x = date, y = PCR.pos)) +
    geom_bar(stat="identity", fill = "grey", alpha = 0.7, color = "white") +
    annotate("segment", x=as.POSIXct(dmy("01/10/2020")),
             xend=as.POSIXct(dmy("14/10/2020")),
             y=0, yend=0, color = "#F4B183", alpha=0.6, lwd = 1.3) +
    annotate("segment", x=as.POSIXct(dmy("15/10/2020")),
             xend=as.POSIXct(dmy("23/10/2020")),
             y=0, yend=0, color = "#ED7D31", alpha=0.6, lwd = 1.3) +
    annotate("segment", x=as.POSIXct(dmy("24/10/2020")),
             xend=as.POSIXct(dmy("1/11/2020")),
             y=0, yend=0, color = "#C55A11", alpha=0.6, lwd = 1.3) +
    annotate("segment", x=as.POSIXct(dmy("2/11/2020")),
             xend=as.POSIXct(dmy("10/11/2020")),
             y=0, yend=0, color = "#ED7D31", alpha=0.6, lwd = 1.3) +
    geom_vline(xintercept = as.POSIXct(dmy("23/10/2020")), alpha=0.4, color = "#8FAADC", lwd = 1.3) +
    geom_vline(xintercept = as.POSIXct(dmy("24/10/2020")), alpha=0.4, color = "#8FAADC", lwd = 1.3) +
    geom_vline(xintercept = as.POSIXct(dmy("25/10/2020")), alpha=0.4, color = "#8FAADC", lwd = 1.3) +
    geom_vline(xintercept = as.POSIXct(dmy("31/10/2020")), alpha=0.4, color = "#4472C4", lwd = 1.3) +
    geom_vline(xintercept = as.POSIXct(dmy("01/11/2020")), alpha=0.4, color = "#4472C4", lwd = 1.3) +
    geom_vline(xintercept = as.POSIXct(dmy("07/11/2020")), alpha=0.4, color = "#2F5597", lwd = 1.3) +
    geom_vline(xintercept = as.POSIXct(dmy("08/11/2020")), alpha=0.4, color = "#2F5597", lwd = 1.3) +
    facet_wrap(pilot~., scale = "free_y") +
    ylab("SARS-CoV-2 incidence") + xlab("date of report") +
    xlim(as.POSIXct(dmy("01/9/2020")),as.POSIXct(dmy("10/11/2020"))) +
    theme_classic()
}
