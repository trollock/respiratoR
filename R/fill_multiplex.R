#' Replaces missing multiplexer channel marker values
#'
#' Replaces missing values that are typically from Expedata multiplexer output with
#' the actual sample channel data.
#'
#' This is a generic function:
#'
#' @param dat dataframe containing respirometry data
#' @param col column containing the information on multiplexer chambers
#' @param value The values in the original dataframe that are uninformative e.g.
#' (-999 or NA) that will be replaced with the actual sampled chamber value for each
#' row. This accepts single or multiple values e.g. ( -1 or c(-1, -999)).
#' @param baseline the value for the baseline channel
#' @return The original dataframe with formatted and corrected multiplexer channel IDs
#' @examples
#' fill_multiplex(testResp, Oxygen, -1, 66)
fill_multiplex <- function(dat, col, value, baseline){
  #Change values to NAs
  dat[[col]][dat[[col]] %in% value] <- NA

  #add a column with the sample number
  #pad the NAs with the previous number until it changes
  dat <- dat %>%
    mutate("Sample" = 1:nrow(dat)) %>%
    select("Sample", everything()) %>%
    fill(col)

  dat[[col]][dat[[col]] %in% baseline] <- "Baseline"

  return(dat)
}


