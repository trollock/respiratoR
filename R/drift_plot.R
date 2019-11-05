#' Baseline drift correction insepction plot
#'
#' This function provides a basic plot of the different spline and linear interpolation fits for the visual inspection of
#' the data.
#'
#' This is a generic function:
#'
#' @param dat dataframe containing spline fits of respirometry data, this is the output from the baseline_corr function
#' @param dat1 dataframe containing the baseline fit data, this is the output from the base_bg_rect function
#' @param val the channel being drift correct, e.g. "Oxygen"
#' @return a dataframe containing drift correct oxygen and carbon dioxide data corrected using a
#' spline fit
#' @examples
#' drift_plot (o2_corr, base_rect, "Oxygen")
#'

drift_plot <- function(dat, dat2, val){

  ggplot(data=dat, aes(Sample, Original))+
    #geom_hline(yintercept=conc1, linetype="dashed")+
    geom_line(data=dat, aes(dat[[1]], dat[[val]]))+
    theme_cowplot()+
    geom_line(data=dat, aes(dat[[1]], dat[[3]]), color="firebrick3", alpha=0.75)+
    geom_line(data=dat, aes(dat[[1]], dat[[4]]), color="royalblue3", alpha=0.75)+
    geom_line(data=dat, aes(dat[[1]], dat[[5]]), color="palegreen3", alpha=0.75)+
    geom_point(data=dat2, aes(x=dat2[[1]], y=dat2[[val]]), color="red", size=2)+
    ylim(min(dat[, 3:5], na.rm=T)* 0.9999, max(dat[, 3:5], na.rm=T) * 1.0001)+
    ylab(lab=val)
}
