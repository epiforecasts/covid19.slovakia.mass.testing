##' Helper function for lables in plot
##'
##' @param labels lables
##' @return a list of split labels
##' @importFrom stringr str_split
##' @keywords internal
make_labels <- function(labels) {
  result <- str_split(labels, "\\.")
  unlist(lapply(result, function(x) x[2]))
}
