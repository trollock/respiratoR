#' Baseline drift correction via spline interpolation
#'
#' When provided user defined baseline data, this function corrects for sensor drift i a specified channel
#' using three types of interpolation; linear, fmm spline, and natural spline. It is intended for the
#' user to compare multiple fits and determine which makes the most sense for drift correction.
#'
#' This is a generic function:
#'
#' @param dat dataframe containing respirometry data
#' @param col1 column containing information on multiplexer chambers
#' @param col2 column containing oxygen data
#' @param col3 column containing sample number data
#' @param conc1 a number specifying the known concetration or value for a channel. e.g. atmospheric O2 concentration is typically ~20.95%
#' @return a dataframe containing drift correct oxygen and carbon dioxide data corrected using a
#' spline fit
#' @examples
#' baseline_spl(test_resp, "Marker", "Oxygen","Sample")
#'
baseline_corr <- function(dat, col1, col2, col3, conc1){

  dat[[col1]][dat[[col1]] == "Baseline"] <- 1

  summary <- dat %>%
    mutate("Switch" = cumsum(c (0, (diff (as.numeric(dat[[col1]])) !=0 )))) %>%
    filter((as.numeric(dat[[col1]])) == 1) %>%
    group_by(Marker, Switch) %>%
    summarise(min = min(Sample), max = max(Sample))

  summary <- summary[, -(1:2)]

  dr <- select(dat, c(col2, col3)) %>%
    filter(dat[[col3]] %in% summary$max)

  dr_seq <- as.data.frame(seq(from=1, to=nrow(dat), by=1))
  colnames(dr_seq) <- c("Sample")

  dr <-  merge(dr_seq, dr, by="Sample", all=TRUE)
  dr[[col2]][1] <- conc1

  corr_dat <- data.frame(Sample = dat[[col3]], Original = dat[[col2]])

  corr_dat$lin <-approx(dr[[col2]], method="linear", n = nrow(dr))[[2]]

  #Fortsythe, Malcolm, and Moler method of spline fit
  #Forsythe, G. E., Malcolm, M. A. and Moler, C. B. (1977) Computer Methods for Mathematical Computations.
  corr_dat$fmm <-spline(dr[[col2]], method="fmm", xmin= min(dr[[col3]]), xmax = max(dr[[col3]]), n = nrow(dr))[[2]]

  corr_dat$nat <-spline(dr[[col2]], method="natural", xmin= min(dr[[col3]]), xmax = max(dr[[col3]]), n = nrow(dr))[[2]]

  corr_dat$corr_lin <- dat[[col2]] + (conc1 - corr_dat$lin)
  corr_dat$corr_fmm <- dat[[col2]] + (conc1 - corr_dat$fmm)
  corr_dat$corr_nat <- dat[[col2]] + (conc1 - corr_dat$nat)

  names(corr_dat)[names(corr_dat) == "lin"] <- paste(col2,"lin", sep="_")
  names(corr_dat)[names(corr_dat) == "fmm"] <- paste(col2,"fmm", sep="_")
  names(corr_dat)[names(corr_dat) == "nat"] <- paste(col2,"nat", sep="_")

  return(corr_dat)
}
