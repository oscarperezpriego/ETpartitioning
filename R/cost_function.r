###############################
### Cost function defining plant optimality arguments
###############################

# ' # Cost function metrics
#' @title  Multi-objective optimality-based cost function
#'
#' @description Cost function to be implemented in an optimization routine of the ETpartitioning package (see Perez-Priego et al., 2018).
#'
#' @param data          Data.frame or matrix containing all required variables.
#' @param par           A vector containing 4 parameters (a1,Do,To,beta)
#' @param ColPhotos     Column name of numeric vector containing time series of photosynthesis data (umol CO2 m-2 s-1).
#' @param ColPhotos_unc Column name of numeric vector containing time series of photosynthesis uncertainties (umol CO2 m-2 s-1).
#' @param ColH          Column name of numeric vector containing time series of sensible heat flux (W m-2).
#' @param ColVPD        Column name of numeric vector containing time series of vapor pressure deficit (hPa).
#' @param ColTair       Column name of numeric vector containing time series of air temperature (deg C).
#' @param ColPair       Column name of numeric vector containing time series of atmospheric pressure (kPa).
#' @param ColQ          Column name of numeric vector containing time series of photosynthetic active radiation (umol m-2 s-1).
#' @param ColCa         Column name of numeric vector containing time series of atmospheric CO2 concentration (umol Co2 mol air-1).
#' @param ColUstar      Column name of numeric vector containing time series of wind friction velocity (m s-1).
#' @param ColWS         Column name of numeric vector containing time series of wind velocity (m s-1).
#' @param ColSW_in      Column name of numeric vector containing time series of incoming short-wave radiation (W m-2).
#' @param Chi_o         Long-term effective chi
#' @param WUE_o         Long-term effective WUE
#'
#' @export
#'
#'
#' @details the multi-objective function is defined as:
#'
#'
#'            \deqn{OF <- sum((photos-photosy_mod)/photos_unc)^2)/n + phi}
#'
#'
#'          where phi invokes optimality theory by minimizing the following term
#'
#'          \deqn{phi <- (sum(transpiration_mod)/sum(photos_mod)*WUE_o}
#'
#' @return a numeric value:
#'         \item{OF}{the summed cost to minimize both the mismatch between observed and modeled Photos and the unit cost of transpiration}
#'
#' @note   The 4 model parameters (a1, Do, Topt and beta, see Perez-Priego et al., 2018) are estimated using a multi-constraint Markov Chain Monte Carlo (MCMC).The objective function (OF) is to find those numerical solutions that minimize not only the mismatch between observed and modeled Photos but also the unit cost of transpiration by introducing a conditional factor demand (phi), which invokes the optimality hypothesis. The phi term is to be defined as the integrated cost of transpiration (i.e. transpiration_mod/photos_mod) over a time period (5 days) normalized by a factor describing the long-term effective water use efficiency (WUE_o).
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
#'  par <- c(200, 0.2, 25, 0.6)
#'
#'cost_function(
#'  data=tmp
#'  ,par=par
#'  ,ColPhotos="GPP_NT_VUT_MEAN"
#'  ,ColPhotos_unc ="NEE_VUT_USTAR50_JOINTUNC"
#'  ,ColH="H_F_MDS"
#'  ,ColVPD="VPD_F"
#'  ,ColTair="TA_F"
#'  ,ColPair="PA_F"
#'  ,ColQ="PPFD_IN"
#'  ,ColCa="CO2_F_MDS"
#'  ,ColUstar="USTAR"
#'  ,ColWS="WS_F"
#'  ,ColSW_in="SW_IN_F"
#'  ,Chi_o = 0.88
#'  ,WUE_o = 24.25
#')

cost_function <- function(
  par
  ,data
  ,ColPhotos
  ,ColPhotos_unc
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
  ,WUE_o
)
{

  iMissing <- which( !(c(ColPhotos, ColPhotos_unc, ColH, ColVPD, ColTair, ColPair, ColQ, ColCa, ColUstar, ColWS, ColSW_in) %in% names(data)))
  if (length(iMissing)) stop(
    "Need to provide variables ", paste(c(ColPhotos, ColPhotos_unc, ColH, ColVPD, ColTair, ColPair, ColQ, ColCa, ColUstar, ColWS, ColSW_in)[iMissing], collapse = ", "))

  ## Matching variable names according to the variable names in the functions
  names(data)[names(data) == ColPhotos] <- "Photos"
  names(data)[names(data) == ColPhotos_unc] <- "Photos_unc"
  names(data)[names(data) == ColH] <- "H"
  names(data)[names(data) == ColVPD] <- "VPD"
  names(data)[names(data) == ColTair] <- "Tair"
  names(data)[names(data) == ColPair] <- "Pair"
  names(data)[names(data) == ColQ] <- "Q"
  names(data)[names(data) == ColSW_in] <- "Q_in"
  names(data)[names(data) == ColCa] <- "Ca"
  names(data)[names(data) == ColUstar] <- "Ustar"
  names(data)[names(data) == ColWS] <- "WS"


  # names(data)[names(data) == "GPP_NT_VUT_MEAN"] <- "Photos"
  # names(data)[names(data) == "NEE_VUT_USTAR50_JOINTUNC"] <- "Photos_unc"
  # names(data)[names(data) == "H_F_MDS"] <- "H"
  # names(data)[names(data) == "VPD_F"] <- "VPD"
  # names(data)[names(data) == "TA_F"] <- "Tair"
  # names(data)[names(data) == "PA_F"] <- "Pair"
  # names(data)[names(data) == "PPFD_IN"] <- "Q"
  # names(data)[names(data) == "SW_IN_F"] <- "Q_in"
  # names(data)[names(data) == "CO2_F_MDS"] <- "Ca"
  # names(data)[names(data) == "USTAR"] <- "Ustar"
  # names(data)[names(data) == "WS_F"] <- "WS"
  #


  ## Converting VPD units (hPa -> kPa)
  data$VPD <- data$VPD/10

  ## Rejecting bad data and filtering for daytime data
  data <- data[data$Photos>0, ,drop=FALSE]
  data <- data[data$Q>0, ,drop=FALSE]
  data <- data[data$Q_in>0, ,drop=FALSE]
  # data <- na.omit(data)

  ## put variables in readable format
  Photos <- data$Photos
  Photos_unc <- data$Photos_unc
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

  #-- Defined constants
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
  # Tplant <- (H*ra/(Cp*dens))+Tair ##<< Approaximation of a surface temperature as canopy temperature (deg C)
  # es_plant <- 0.61078*exp((17.269*Tplant)/(237.3+ Tplant)) ##<< saturated vapor presure deficit at the plant surface.
  # es_air <- 0.61078*exp((17.269*Tair)/(237.3+ Tair)) ##<< saturated vapor presure deficit at the plant surface.
  # ea <- es_air-VPD ##<< atmospheric vapor pressure [kPa]
  # VPD_plant <- es_plant-ea ##<< Plant-to-air vapor pressure deficit [kPa]

  VPD_plant <- VPD

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

  gw_mod <- 1.6*gc_mod ## leaf canopy conductance to water vapor [m s-1]

  #--  Calculating "bulk" surface conductance
  gc_bulk <- Mden/(1/(gc_mod)+ra_c) ## bulk canopy conductance to CO2 [mol m-2 s-1]

  gw_bulk <- Mden/(1/(gw_mod)+ra_w) ## bulk canopy conductance to water vapor [mol m-2 s-1]

  #-- Estimating Chi

  Chi <- Chi_o*(1/(1+beta*VPD_plant^0.5))

  #-- Estimating photosynthesis and transpiration rates

  Photos_mod <- gc_bulk*Ca*(1-Chi)

  transpiration_mod <- gw_bulk*VPD_plant/Pair*1000 ##<< [mmol H2O m-2 s-1]

 ##-- Objective function

  WaterCost_i <- sum(transpiration_mod, na.rm=T)/sum(Photos_mod, na.rm=T)

  Phi <- WaterCost_i*WUE_o

  Photos_unc_threshold <- ifelse(Photos*0.1 > Photos_unc, Photos*0.1, Photos_unc) ##<< We define the threshold of the uncertinties as the 1% of the magnitude of the observation.

  FO <- sum(((Photos_mod-Photos)/Photos_unc_threshold)^2, na.rm=T)/length(Photos_mod)
  # FO <- sum((Photos_mod-Photos)^2, na.rm=T)/length(Photos_mod)
  ## Summed cost
  solution <- FO+Phi
  return(solution)

}


# par <- c(200, 0.2, 25, 0.6)
# tmp <-  EddySample[ EddySample$TIMESTAMP_START>  201105150000,]
# tmp <-  tmp[tmp$TIMESTAMP_START<  201105160000,]
#  WUE_o <-  calculate_WUE_o(EddySample, ColPhotos = "GPP_NT_VUT_MEAN", ColVPD = "VPD_F", ColTair = "TA_F", C = 1.189, Z=0.27 )
#  Chi_o <-  calculate_chi_o(EddySample, ColPhotos = "GPP_NT_VUT_MEAN", ColVPD = "VPD_F", ColTair = "TA_F", C = 1.189, Z=0.27 )
#
# cost_function(
#   data=tmp
#   ,par=par
#   ,ColPhotos="GPP_NT_VUT_MEAN"
#   ,ColPhotos_unc="NEE_VUT_USTAR50_JOINTUNC"
#   ,ColH="H_F_MDS"
#   ,ColVPD="VPD_F"
#   ,ColTair="TA_F"
#   ,ColPair="PA_F"
#   ,ColQ="PPFD_IN"
#   ,ColCa="CO2_F_MDS"
#   ,ColUstar="USTAR"
#   ,ColWS="WS_F"
#   ,ColSW_in="SW_IN_F"
#   ,Chi_o = Chi_o
#   ,WUE_o= WUE_o)
#
#
