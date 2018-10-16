###############################
### long-term effective "internal" leaf-to-ambient CO2 (chi_o)###
###############################

# ' # chi_o Metrics
#' @title Calculation of chi_o
#'
#' @description Calculation of chi_o (see Wang et al., 2017; Plant Nature).
#'
#' @param data      Data.frame or matrix containing all required variables.
#' @param ColPhotos column name of numeric vector containing time series of photosynthesis data (umol CO2 m-2 s-1)
#' @param ColVPD    column name of numeric vector containing time series of vapor pressure deficit (kPa).
#' @param ColTair   column name of numeric vector containing time series of air temperature (deg C).
#' @param Z         Z- numeric value defining elevation (km).
#' @param C         Empirical coeficient for C3 species.

#' @export
#' @importFrom stats quantile
#' @details the following metrics are calculated:
#'
#'          logistic_chi_o:
#'
#'            \deqn{logistic_chi_o = 0.0545*(Tair_g-25)-0.58*log(VPD_g)-0.0815*Z+C}
#'
#'            \deqn{chi_o <- exp(logistic_chi_o)/(1+exp(logistic_chi_o))}
#'
#'          Tair_g and VPD_g are calculated based on the mean value of the growing period.
#'          The growing period is estimated as those periods over the 85 quantile of Photos.
#'
#' @return a numeric value:
#'         \item{chi_o}{long-term effective "internal" leaf-to-ambient CO2 (r.u)}
#'
#' @note chi_o is unitless.
#'       Photos is calculated based on night-time NEE partitioning approach (Reichstein et al., 2005).
#'
#' @references Wang, H., I. C. Prentice, et al., (2017), Towards a universal model
#'             for carbon dioxide uptake by plants, Nature Plants, 3(9), 734-741.
#'
#'             Reichstein, M., et al. (2005), On the separation of net ecosystem exchange
#'             into assimilation and ecosystem respiration: review and improved algorithm,
#'             Global Change Biology, 11(9), 1424-1439.
#'
#'
#' @examples
#' ## filter data for dry periods and daytime at DE-Tha in June 2014
#'
#' calculate_chi_o(data= EddySample
#',ColPhotos = "GPP_NT_VUT_MEAN"
#',ColVPD = "VPD_F"
#',ColTair = "TA_F"
#',C = 1.189 ##<< Empirical coeficient for C3 species (see Wang et al., 2017; Plant Nature).
#',Z=0.27)

calculate_chi_o <- function(
   data
  ,ColPhotos
  ,ColVPD
  ,ColTair
  ,C
  ,Z
  )
  {

  iMissing <- which( !(c(ColPhotos, ColVPD, ColTair) %in% names(data)))
  if (length(iMissing)) stop(
    "Need to provide columns ", paste(c(ColPhotos, ColVPD, ColTair)[iMissing], collapse = ", "))

  ## Matching variable names according to the variable names in the function
  names(data)[names(data) == ColPhotos] <- "Photos"
  names(data)[names(data) == ColVPD] <- "VPD"
  names(data)[names(data) == ColTair] <- "Tair"

  ## Converting VPD units (hPa -> kPa)
  data$VPD <- data$VPD/10

  ## Defining an optimal growing period according to quantiles of photosynthesis
  Growth.Threshold <- quantile(data$Photos, probs = 0.85, na.rm=T)
  tmp <- data[data$Photos>Growth.Threshold, ,drop=FALSE]

  Tair_g <- mean(tmp$Tair, na.rm=T)
  VPD_g <- mean(tmp$VPD/10, na.rm=T)

  logistic_chi_o = 0.0545*(Tair_g-25)-0.58*log(VPD_g)-0.0815*Z+C

  chi_o <- exp(logistic_chi_o)/(1+exp(logistic_chi_o)) ## Long-term effective CiCa
  return(chi_o)

}


# WUE_o <- (390*(1-Xo)*96)/(1.6*Do)*0.001 ## 0.001 to convert umol/mol into umol/mmol


