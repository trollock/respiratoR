#' Calculates baseline window stats
#'
#' Determines the beginning and end of each time the multiplexer measures baseline data.
#'This is primarily a function for plotting and initial visial exploratory  analysis
#'of respirometry data.
#'
#' This is a generic function:
#'
#' @param dat dataframe containing respirometry data
#' @param col column containing information on multiplexer chambers
#' @return a dataframe with the beginning and end of each baseline sampling period
#' @examples
#' base_bg_rect(test_resp, "Marker")
#'
base_bg_rect <- function(dat, col){

  dat[[col]][dat[[col]] == "Baseline"] <- 1

  summary <- dat %>%
    mutate("Switch" = cumsum(c (0, (diff (as.numeric(dat[[col]])) !=0 )))) %>%
    filter((as.numeric(dat[[col]])) == 1) %>%
    group_by(Marker, Switch) %>%
    summarise(min = min(Sample), max = max(Sample))

  return(summary)
}
