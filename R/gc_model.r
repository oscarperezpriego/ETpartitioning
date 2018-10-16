###############################
### stomatal conductance model ###
###############################

# ' # gc Metrics
#' @title Calculation of gc
#'
#' @description Calculation of canopy stomatal conductance using Jarvis's approach.
#'
#' @param par       Set of parameter for the respective sensitivity funciton.
#' @param data      Data.frame or matrix containing all required variables.
#' @param Q         Vector containing time series of photosynthetic active radiation (umol CO2 m-2 s-1)
#' @param VPD       Vector containing time series of vapor pressure deficit (kPa).
#' @param Tair      Vector containing time series of air temperature (deg C).
#' @param gcmax     Empirical parameter defining maximum conductance (m s-1 or mol m-2 s-1).
#'
#' @export
#' @details the following metrics are calculated:
#'
#'
#'            \deqn{gc_model <- gcmax*f(Q)*f(VPD)*f(Tair)}
#'
#'
#' @return a numeric value:
#'         \item{gc_model}{canopy leaf conductance (untis refer to that given by gmax)}
#'
#'
#' @references Perez-Priego, O., G. Katul, M. Reichstein et al. Partitioning eddy covariance
#'             water flux components using physiological and micrometeorological approaches,
#'             Journal of Geophysical Research: Biogeosciences. In press
#'
#'
#' @examples
#'
#'  ## Selecting a single day (e.g. 15-05-2011)
#'  tmp <-  EddySample[ EddySample$TIMESTAMP_START>  201105150000,]
#'  tmp <-  tmp[tmp$TIMESTAMP_START<  201105160000,]

#' gc_model(par=c(200, 0.2, 25)
#' ,data= tmp
#' ,Q = "PPFD_IN"
#' ,VPD = "VPD_F"
#' ,Tair = "TA_F"
#' ,gcmax = 1)

gc_model <- function(
  par
  ,data
  ,Q
  ,VPD
  ,Tair
  ,gcmax
)
{

  iMissing <- which( !(c(Q, VPD, Tair) %in% names(data)))
  if (length(iMissing)) stop(
    "Need to provide columns ", paste(c(Q, VPD, Tair)[iMissing], collapse = ", "))

  #-- Defining variables
  ## Matching variable names according to the variable names in the function
  names(data)[names(data) == Q] <- "Q"
  names(data)[names(data) == VPD] <- "VPD"
  names(data)[names(data) == Tair] <- "Tair"

  Q <- data$Q
  VPD <- data$VPD
  Tair <- data$Tair

  #-- Fitting parameters
  a1 <- par[1] ##<< radiation curvature
  D0 <- par[2] ##<< D0 empirical coefficient related to the response of stomatal closure to VPD.
  Topt <-  par[3] ##<< optimum temperature

  # ---- light response curve ----
  FQ <- (Q)/(Q+a1) ##<<  See Baldocchi et al., 1991 AFM and Jarvis 1976

  # ---- VPD sensitivity ----
  Fd <- exp(-D0*VPD)

  # ---- Optimum Temperature curve ----

  Tl <- 0 ##<<  minimum temperature [deg C]
  Th <- 50 ##<<  max temperature [deg C]
  b4 <- (Th-Topt)/(Th-Tl) ##<< parameter
  b3 <- 1/((Topt-Tl)*(Th-Topt)^b4) ##<< parameter

  Ftemp <- b3*(Tair-Tl)*(Th-Tair)^b4


  #______________________________________________________________________________
  ## ----  Calculating conductance
  #______________________________________________________________________________

  sensitivity_function <- FQ*Fd*Ftemp
  sensitivity_function_scaled <- (FQ*Fd*Ftemp)/max(sensitivity_function, na.rm=T) ##<< scaling between 0 and 1.
  gc_model <- gcmax*sensitivity_function_scaled
  return(gc_model)

}




