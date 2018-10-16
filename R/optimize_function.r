###############################
### Optimize function
###############################

# ' # Find the optimal paramerers
#' @title  Model optimization routine
#'
#' @description Routine to estimate optimal parameters of a photosynthesis model using a multi-constraint Markov Chain Monte Carlo (MCMC) (see Perez-Priego et al., 2018).
#'
#' @param par_lower     A vector containing the lower bound of the parameters (a1,Do,To,beta)
#' @param par_upper     A vector containing the upper bound of the parameters (a1,Do,To,beta)
#' @param data          Data.frame or matrix containing all required variables.
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
#'
#' @export
#' @importFrom stats median
#' @importFrom FME Latinhyper
#' @importFrom  FME modMCMC
#'
#' @return a numeric vector containing 4 optimal parameters (a1,Do,To,beta):
#'
#' @note   The 4 model parameters (a1, Do, Topt and beta, see Perez-Priego et al., 2018) are estimated using a multi-constraint Markov Chain Monte Carlo (MCMC).The objective function (OF) is to find those numerical solutions that minimize not only the mismatch between observed and modeled Photos but also the unit cost of transpiration by introducing a conditional factor demand (phi), which invokes the optimality hypothesis. The phi term is to be defined as the integrated cost of transpiration (i.e. transpiration_mod/photos_mod) over a time period (5 days) normalized by a factor describing the long-term effective water use efficiency (WUE_o).
#'
#' @references Perez-Priego, O., G. Katul, M. Reichstein et al. Partitioning eddy covariance
#'             water flux components using physiological and micrometeorological approaches,
#'             Journal of Geophysical Research: Biogeosciences. In press
#'
#'
#'
#' @examples
#'  ## Selecting a single day (e.g. 15-05-2011)
#'  tmp <-  EddySample[ EddySample$TIMESTAMP_START>  201105150000,]
#'  tmp <-  tmp[tmp$TIMESTAMP_START<  201105160000,]
#'  ## Defining parameter values
#'
#'  optimal_parameters(par_lower = c(0, 0, 10, 0)
#'                     ,par_upper = c(400,0.4, 30, 1)
#'                    ,data = tmp
#'                    ,ColPhotos = "GPP_NT_VUT_MEAN"
#'                    ,ColPhotos_unc = "NEE_VUT_USTAR50_JOINTUNC"
#'                    ,ColH = "H_F_MDS"
#'                    ,ColVPD = "VPD_F"
#'                    ,ColTair = "TA_F"
#'                    ,ColPair = "PA_F"
#'                    ,ColQ = "PPFD_IN"
#'                    ,ColCa = "CO2_F_MDS"
#'                    ,ColUstar = "USTAR"
#'                    ,ColWS = "WS_F"
#'                    ,ColSW_in = "SW_IN_F"
#'                    ,Chi_o = 0.88
#'                    ,WUE_o = 24.25)
#'
#'

optimal_parameters <- function(par_lower
                              ,par_upper
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
                              ,WUE_o){

  num = 1 ##<< numeber of sampled parameters
  pars <- Latinhyper(cbind(par_lower,par_upper),num = num)

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



  min.RSS <- function(p) {cost_function(
    par=p
    ,data=data
    ,ColPhotos="Photos"
    ,ColPhotos_unc="Photos_unc"
    ,ColH="H"
    ,ColVPD="VPD"
    ,ColTair="Tair"
    ,ColPair="Pair"
    ,ColQ="Q"
    ,ColCa="Ca"
    ,ColUstar="Ustar"
    ,ColWS="WS"
    ,ColSW_in="Q_in"
    ,Chi_o = Chi_o
    ,WUE_o = WUE_o )
  }

  parMCMC <- try(modMCMC(f = min.RSS
                         ,p = as.numeric(pars)
                         ,niter = 20000
                         ,updatecov = 500, ntrydr = 3
                         ,lower = par_lower , upper = par_upper
                         ,burninlength = 10000))

  out <- summary(parMCMC)

  a1 <- out[1,1]
  Do <- out[1,2]
  Topt <- out[1,3]
  beta <- out[1,4]

  return(data.frame(a1, Do, Topt, beta))

}



## Selecting a single day (e.g. 15-05-2011)
# tmp <-  EddySample[ EddySample$TIMESTAMP_START>  201105150000,]
# tmp <-  tmp[tmp$TIMESTAMP_START<  201105160000,]
# ## Defining parameter values
#
# optimal_parameters(par_lower= c(0,0, 10, 0)
#                   ,par_upper = c(400,0.4, 30, 1)
#                   ,data=tmp
#                   ,ColPhotos="GPP_NT_VUT_USTAR50"
#                   ,ColPhotos_unc ="NEE_VUT_USTAR50_JOINTUNC"
#                   ,ColH="H_F_MDS"
#                   ,ColVPD="VPD_F"
#                   ,ColTair="TA_F"
#                   ,ColPair="PA_F"
#                   ,ColQ="PPFD_IN"
#                   ,ColCa="CO2_F_MDS"
#                   ,ColUstar="USTAR"
#                   ,ColWS="WS_F"
#                   ,ColSW_in="SW_IN_F"
#                   ,Chi_o = 0.88
#                   ,WUE_o = 24.25)


