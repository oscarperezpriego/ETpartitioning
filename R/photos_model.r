###############################
### Photosynthesis model using Fick's law
###############################

# ' # Photos Metrics
#' @title  Photosynthesis model
#'
#' @description Modelling eddy covariance derived photosynthesis via diffusion of CO2 through stomata and plant optimization theory (see Perez-Priego et al., 2018).
#'
#' @param par       A vector containing optimal parameters (a1,Do,To,beta).
#' @param data      Data.frame or matrix containing all required variables.
#' @param ColPhotos Column name of numeric vector containing time series of photosynthesis data (umol CO2 m-2 s-1).
#' @param ColH      Column name of numeric vector containing time series of sensible heat flux (W m-2).
#' @param ColVPD    Column name of numeric vector containing time series of vapor pressure deficit (hPa).
#' @param ColTair   Column name of numeric vector containing time series of air temperature (deg C).
#' @param ColPair   Column name of numeric vector containing time series of atmospheric pressure (kPa).
#' @param ColQ      Column name of numeric vector containing time series of photosynthetic active radiation (umol m-2 s-1).
#' @param ColCa     Column name of numeric vector containing time series of atmospheric CO2 concentration (umol Co2 mol air-1).
#' @param ColUstar  Column name of numeric vector containing time series of wind friction velocity (m s-1).
#' @param ColWS     Column name of numeric vector containing time series of wind velocity (m s-1).
#' @param ColSW_in  Column name of numeric vector containing time series of incoming short-wave radiation (W m-2).
#' @param Chi_o     Long-term effective chi.
#'
#' @export
#' @details The following metrics are calculated as:
#'
#'
#'          \deqn{Photos <- (gc_bulk*Ca*(1-Chi)}
#'
#'          gc_bulk accounts for stomatal and aerodynamic effects.
#'          While chi is estimated using optimality theory as:
#'
#'          \deqn{Chi <- (Chi_o*1/(1+beta*VPD^0.5)}
#'
#' @return a numeric vector containing estimates of photosyntesis rates (umolCO2 m-2 s-1)
#'
#'
#' @note   Model is constrained with estimates of Photos from the night-time NEE partitioning approach (Reichstein et al., 2005).
#'         chi is optimized following Perez-Priego et al., 2018.
#'
#' @references Perez-Priego, O., G. Katul, M. Reichstein et al. Partitioning eddy covariance
#'             water flux components using physiological and micrometeorological approaches,
#'             Journal of Geophysical Research: Biogeosciences. In press
#'
#'
#'             Reichstein, M., et al. (2005), On the separation of net ecosystem exchange
#'             into assimilation and ecosystem respiration: review and improved algorithm,
#'             Global Change Biology, 11(9), 1424-1439.
#'
#' @examples
#'  ## Selecting a single day (e.g. 15-05-2011)
#'  tmp <-  EddySample[ EddySample$TIMESTAMP_START>  201105150000,]
#'  tmp <-  tmp[tmp$TIMESTAMP_START<  201105160000,]
#'  ## Defining parameter values
#'  par <- c(382, 0.05, 23, 0.62)
#'
#'  photos_model(par = par
#'  ,data = tmp
#'  ,ColPhotos = "GPP_NT_VUT_MEAN"
#'  ,ColH = "H_F_MDS"
#'  ,ColVPD = "VPD_F"
#'  ,ColTair = "TA_F"
#'  ,ColPair = "PA_F"
#'  ,ColQ = "PPFD_IN"
#'  ,ColCa = "CO2_F_MDS"
#'  ,ColUstar = "USTAR"
#'  ,ColWS = "WS_F"
#'  ,ColSW_in = "SW_IN_F"
#'  ,Chi_o = 0.88
#')

photos_model <- function(
   par
  ,data
  ,ColPhotos
  ,ColH
  ,ColVPD
  ,ColTair
  ,ColPair
  ,ColQ
  ,ColCa
  ,ColUstar
  ,ColWS
  ,ColSW_in
  ,Chi_o
)
{

  iMissing <- which( !(c(ColPhotos, ColH, ColVPD, ColTair, ColPair, ColQ, ColCa, ColUstar, ColWS ) %in% names(data)))
  if (length(iMissing)) stop(
    "Need to provide columns ", paste(c(ColPhotos, ColH, ColVPD, ColTair, ColPair, ColQ, ColCa, ColUstar, ColWS, ColSW_in)[iMissing], collapse = ", "))

  ## Matching variable names according to the variable names in the functions
  names(data)[names(data) == ColPhotos] <- "Photos"
  names(data)[names(data) == ColH] <- "H"
  names(data)[names(data) == ColVPD] <- "VPD"
  names(data)[names(data) == ColTair] <- "Tair"
  names(data)[names(data) == ColPair] <- "Pair"
  names(data)[names(data) == ColQ] <- "Q"
  names(data)[names(data) == ColSW_in] <- "Q_in"
  names(data)[names(data) == ColCa] <- "Ca"
  names(data)[names(data) == ColUstar] <- "Ustar"
  names(data)[names(data) == ColWS] <- "WS"


  # names(data)[names(data) == "GPP_NT_VUT_USTAR50"] <- "Photos"
  # names(data)[names(data) == "H_F_MDS"] <- "H"
  # names(data)[names(data) == "VPD_F"] <- "VPD"
  # names(data)[names(data) == "TA_F"] <- "Tair"
  # names(data)[names(data) == "PA_F"] <- "Pair"
  # names(data)[names(data) == "PPFD_IN"] <- "Q"
  # names(data)[names(data) == "SW_IN_F"] <- "Q_in"
  # names(data)[names(data) == "CO2_F_MDS"] <- "Ca"
  # names(data)[names(data) == "USTAR"] <- "Ustar"
  # names(data)[names(data) == "WS"] <- "WS"


  ## Converting VPD units (hPa -> kPa)
  data$VPD <- data$VPD/10

  ##-- putting variables in a readable format
  Photos <- data$Photos
  H <- data$H
  VPD <- data$VPD
  Tair <- data$Tair
  Pair <- data$Pair
  Q <- data$Q
  Q_in <- data$Q_in*2 # convertion factor between W m2 to umol m-2 s-1
  Q <- ifelse(is.na(Q)== "TRUE", Q_in, Q) #If PAR is not provided we use SW_in instead as an approximation of PAR
  Ca <- data$Ca
  landa <- (3147.5-2.37*(Tair+273.15))*1000 # Latent heat of evaporisation [J kg-1]
  Ustar <- data$Ustar
  WS <- data$WS

  #-- Defining constants
  Cp = 1003.5 ##<< heat capacity [J kg-1 K-1].
  R_gas_constant <- 0.287058 ##<< [J kg-1 deg K-1].
  M = 0.0289644 ##<< molar mass, [kg mol-1].
  dens <- Pair/(R_gas_constant*(Tair+273.15))# Air density [kg m-3].
  Mden <- dens/M ##<< molar air density [mol m-3].

  #-- Fitting parameters
  a1 <- par[1] ##<< radiation curvature
  D0 <- par[2] ##<< D0 empirical coefficient related to the response of stomatal closure to VPD.
  Topt <-  par[3] ##<< optimum temperature
  beta <- par[4] ##<< A plant state variable defining the carbon cost of water.

  #--  Aerodynamic conductance
  ra_m <- WS/Ustar^2 ##<< aerodynamic resistance to momentum transfer.
  ra_b <- 6.2*Ustar^-0.67 ##<< aerodynamic resistance to heat transfer.
  ra <- ra_m+ra_b ##<< Monteith and Unsworth [2013]
  ra_w <- ra_m+2*(1.05/0.71/1.57)^(2/3)*ra_b ##<< originally by Hicks et al., 1987.
  ra_c <- ra_m+2*(1.05/0.71)^(2/3)*ra_b

  #--  plant temperature (Tplant) and plant to air vapor pressure deficit (VPD_plant)
  Tplant <- (H*ra/(Cp*dens))+Tair ##<< Approaximation of a surface temperature as canopy temperature (deg C)
  es_plant <- 0.61078*exp((17.269*Tplant)/(237.3+ Tplant)) ##<< saturated vapor presure deficit at the plant surface.
  es_air <- 0.61078*exp((17.269*Tair)/(237.3+ Tair)) ##<< saturated vapor presure deficit at the plant surface.
  ea <- es_air-VPD ##<< atmospheric vapor pressure [kPa]
  VPD_plant <- es_plant-ea ##<< Plant-to-air vapor pressure deficit [kPa]
  # VPD_plant <- VPD ## Here we consider VPD_plant = VPD

  #--model structure

  #-- Defining optimum parameters
  Photos_max <-quantile(Photos, probs=c(0.90), na.rm=T)
  Dmax <- mean(data$VPD[data$Photos>Photos_max], na.rm=T)
  Chimax <- Chi_o*(1/(1+beta*Dmax^0.5)) ##<< Chi_o is calculated using calculate_chi_o function
  gcmax <- median(Photos_max/(Mden*Ca*(1-Chimax)), na.rm=T) ##<< We assume that a max conductance is achieved at Photos_max under optimum conditions.

   #--  Calculating canopy stomatal conductance to CO2 [m s-1]
  gc_mod <- gc_model(par=par
                     ,data= data
                     ,Q = "Q"
                     ,VPD = "VPD"
                     ,Tair = "Tair"
                     ,gcmax = gcmax)

  # gw_mod <- 1.6*gc_mod ## leaf canopy conductance to water vapor [m s-1]

  #--  Calculating "bulk" surface conductance
  gc_bulk <- Mden/(1/(gc_mod)+ra_c) ## bulk canopy conductance to CO2 [mol m-2 s-1]

  # gw_bulk <- Mden/(1/(gw_mod)+ra_w) ## bulk canopy conductance to water vapor [mol m-2 s-1]

  #-- Estimating Chi

  Chi <- Chi_o*(1/(1+beta*VPD_plant^0.5))

  Photos_mod <- gc_bulk*Ca*(1-Chi)

  return(Photos_mod)

}


# par <- c(200, 0.2, 25, 0.6)
# tmp <-  EddySample[ EddySample$TIMESTAMP_START>  201105150000,]
# tmp <-  tmp[tmp$TIMESTAMP_START<  201105160000,]
#
# photos_model(
#   data=tmp
#   ,par=par
#   ,ColPhotos="GPP_NT_VUT_MEAN"
#   ,ColH="H_F_MDS"
#   ,ColVPD="VPD_F"
#   ,ColTair="TA_F"
#   ,ColPair="PA_F"
#   ,ColQ="PPFD_IN"
#   ,ColCa="CO2_F_MDS"
#   ,ColUstar="USTAR"
#   ,ColWS="WS"
#   ,ColSW_in="SW_IN_F"
#   ,Chi_o = 0.88)
#

