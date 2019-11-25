#' Baseline drift correction inspetion plot
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
#'
equation_9_7 <- function(dat, val1, val2){
  #Equation 8.6 in Lighton et al. 2012
  dat$FRe <- dat$Flow * ( dat$kPa - dat$WVP) / dat$kPa

  #Convert to fractional concentrations
  dat$O2_Fe <- dat$O_corr / 100
  dat$CO2_Fe <- dat$CO2_corr / 100
  dat$H2O_Fe <- dat$WVP / dat$kPa

  #Equations 9.7, 9.8, and 9.9 in Lighton et al. 2012

  dat$VO2 <- dat$FRe * (0.2095 - (dat$O2_Fe * (1 - 0.2095 - val1 - val2) / (1 - dat$O2_Fe - dat$CO2_Fe - dat$H2O_Fe)))

  dat$VCO2 <- dat$FRe * ((dat$CO2_Fe * (1 - 0.2095 - val1 - val2) / (1 - dat$O2_Fe - dat$CO2_Fe - dat$H2O_Fe)) - val1)

  dat$VH2O <- dat$FRe * ((dat$H2O_Fe * (1 - 0.2095 - val1 - val2) / (1 - dat$O2_Fe - dat$CO2_Fe - dat$H2O_Fe)) - val2)

  dat$RER <-  dat$VCO2 / dat$VO2

  return(dat)
}
