#' Baseline drift correction via spline interpolation
#'
#' When provided user defined baseline data, this function corrects for sensor drift using a cubic spline interpolation. It is intended for the
#' user to compare multiple fits from linear, cubic, and
#'
#' This is a generic function:
#'
#' @param dat dataframe containing respirometry data
#' @param col column containing information on multiplexer chambers
#' @param col1 column containing oxygen data
#' @param col2 column containing carbon dioxide data
#' @param col3 column containing sample number data
#' @return a dataframe containing drift correct oxygen and carbon dioxide data corrected using a
#' spline fit
#' @examples
#' baseline_spl(test_resp, "Marker", "Oxygen", "CO2", "Sample")
#'
baseline_spl <- function(dat, col, col1, col2, col3){
  dat[[col]][dat[[col]] == "Baseline"] <- 1

  summary <- dat %>%
    mutate("Switch" = cumsum(c (0, (diff (as.numeric(dat[[col]])) !=0 )))) %>%
    filter((as.numeric(dat[[col]])) == 1) %>%
    group_by(Marker, Switch) %>%
    summarise(min = min(Sample), max = max(Sample))

  summary <- summary[, -(1:2)]

  #summary <- as.data.frame(sequence(dat_points) + rep(summary$max - dat_points, dat_points))

  dr <- select(dat, c(col1, col2, col3)) %>%
    filter(dat[[col3]] %in% summary$max)

  dr_seq <- as.data.frame(seq(from=1, to=nrow(dat), by=1))
  colnames(dr_seq) <- c("Sample")

  dr <-  merge(dr_seq, dr, by="Sample", all=TRUE)
  dr[[col1]][1] <- 20.950
  dr[[col2]][1] <- 0.065

  dat$O2_spl <- spline(dr[[col1]], y = NULL, method = "natural", ties = mean)
  dat$CO2_spl <- spline(dr[[col2]], y = NULL, method = "natural", ties = mean)

  #dat$O2_spl <- na.spline(dr[[col1]])
  #dat$CO2_spl <- na.spline(dr[[col2]])

  dat$O2_corr <- dat[[col1]] + (20.950 - dat$O2_spl)
  dat$CO2_corr <- (dat[[col2]] - dat$CO2_spl) + 0.065

  return(dat)
}
