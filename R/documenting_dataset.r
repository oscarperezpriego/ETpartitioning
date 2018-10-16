###############################
### Documenting dataset
###############################
#'  Example of eddy covariance data.
#'
#'  A dataset containing eddy covariance data and meteorological data collected at the Puechabon site. Data is included in the FLUXNET2015 dataset.
#'
#' @format A data frame containing eddy covariance data.
#' \describe{
#' \item{TIMESTAMP_START}{ISO timestamp start of averaging period - short format (YYYYMMDDHHMM)}
#' \item{TIMESTAMP_END}{ISO timestamp end of averaging period - short format (YYYYMMDDHHMM)}
#' \item{TA_F}{Air temperature, consolidated from TA_F_MDS and TA_ERA (deg C)}
#' \item{SW_IN_F}{Shortwave radiation, incoming consolidated from SW_IN_F_MDS and SW_IN_ERA (negative values set to zero) (W m-2)}
#' \item{SW_IN_F_QC}{Quality flag for SW_IN_F; 0= measured; 1 = good quality gapfill; 2 =downscaled from ERA}
#' \item{LW_IN_F}{Longwave radiation, incoming (W m-2)}
#' \item{LW_IN_F_QC}{Quality flag for LW_IN_F; 0= measured; 1 = good quality gapfill; 2 =downscaled from ERA}
#' \item{VPD_F}{Air vapor presure deficit (hPa)}
#' \item{VPD_F_QC}{Quality flag for VPD_F; 0 = measured; 1 = good quality gapfill; 2 = downscaled from ERA}
#' \item{PA_F}{Atmospheric pressure consolidated from PA and PA_ERA (kPa)}
#' \item{PA_F_QC}{Quality flag for PA_F; 0 = measured; 1 = good quality gapfill; 2 = downscaled from ERA}
#' \item{WS_F}{Wind speed (m s-1) consolidated from WS and WS_ERA, 0 = measured; 1 = good quality gapfill; 2 = downscaled from ERA}
#' \item{WS_F_QC}{Quality flag for WS_F; 0 = measured; 1 = good quality gapfill; 2 = downscaled from ERA}
#' \item{WD}{Wind direction (decimal degree)}
#' \item{P_F}{Precipitation consolidated from P and P_ERA}
#' \item{P_F_QC}{Quality flag for P_F; 0 = measured; 2 = downscaled from ERA}
#' \item{USTAR}{Friction velocity (m s-1)}
#' \item{NETRAD}{Net radiation (w m-2)}
#' \item{PPFD_IN}{Photosynthetic photon flux density, incoming (umol m-2 s-1)}
#' \item{PPFD_IN_QC}{Quality flag of PPFD_IN (adimensional)}
#' \item{PPFD_DIF}{Photosynthetic photon flux density, diffuse incoming (umol m-2 s-1)}
#' \item{PPFD_OUT}{Photosynthetic photon flux density, outcoming (umol m-2 s-1)}
#' \item{SW_DIF}{Shortwave radiation, diffuse outcoming (W m-2)}
#' \item{SW_OUT}{Shortwave radiation, outcoming (W m-2)}
#' \item{LW_OUT}{Longwave radiation, outcoming (W m-2)}
#' \item{CO2_F_MDS}{CO2 mole fraction, gapfilled with MDS (ppm)}
#' \item{CO2_F_MDS_QC}{Quality flag for CO2_F_MDS. 0 = measured; 1 = good quality gapfill; 2 = medium; 3 = poor}
#' \item{TS_F_MDS_1}{Soil temperature, gapfilled with MDS (numeric index 1 indicates the shallowest}
#' \item{TS_F_MDS_1_QC}{Quality flag for TS_F_MDS_1}
#' \item{SWC_F_MDS_1}{Soil water content; gapfilled with MDS (numeric index 1 indicates the shallowest)}
#' \item{SWC_F_MDS_1_QC}{Quality flag for SWC_F_MDS_1}
#' \item{G_F_MDS}{Soil heat flux (W m-2)}
#' \item{G_F_MDS_QC}{Quality flag for G_F_MDS; 0 = measured; 1 = good quality gapfill; 2 = medium; 3 = poor}
#' \item{LE_F_MDS}{Latent heat flux, gapfilled using MDS method (W m-2)}
#' \item{LE_F_MDS_QC}{Quality flag for LE_F_MDS, LE_CORR, LE_CORR25, and LE_CORR75. Note: this variable is incomplete, a complete version and explanation can be found in the known issues document}
#' \item{LE_CORR}{Latent heat flux, corrected LE_F_MDS by energy balance closure correction factor}
#' \item{LE_RANDUNC}{Random uncertainty of LE, from measured only data point where LE_F_MDS_QC is 0 and two hierarchical methods (see header and H_RANDUNC_METHOD)}
#' \item{H_F_MDS}{sensible heat flux, gapfilled using MDS method (W m-2)}
#' \item{H_F_MDS_QC}{Quality flag for H_F_MDS, H_CORR, H_CORR25, and H_CORR75. Note: this variable is incomplete, a complete version and explanation can be found in the known issues document: http://fluxnet.fluxdata.org/data/fluxnet2015-dataset/known-issues/}
#' \item{H_CORR}{Sensible heat flux, corrected H_F_MDS by energy balance closure correction factor}
#' \item{H_RANDUNC}{Random uncertainty of H, from measured only data point where H_F_MDS_QC is 0 and two hierarchical methods (see header and H_RANDUNC_METHOD)}
#' \item{NIGHT}{Flag indicating nighttime interval based on SW_IN_POT 0 = daytime, 1 = nighttime}
#' \item{NEE_VUT_MEAN}{Net Ecosystem Exchange (umolCO2 m-2 s-1), using Constant Ustar Threshold (CUT) across years, average from 40 NEE_CUT_XX versions}
#' \item{NEE_VUT_MEAN_QC}{Quality flag for NEE_VUT_MEAN, fraction between 0-1 indicating percentage of good quality data (adimensional)}
#' \item{NEE_VUT_USTAR50_JOINTUNC}{Joint uncertainty estimation for NEE_CUT_USTAR50, including random uncertainty and USTAR filtering uncertainty}
#' \item{RECO_NT_VUT_MEAN}{Ecosystem respiration, from Nighttime partitioning method, average from RECO versions, each from corresponding NEE_VUT_XX version}
#' \item{RECO_NT_VUT_SE}{Standard Error for ecosystem respiration, calculated as (SD(GPP_NT_VUT_XX) / SQRT(40))}
#' \item{GPP_NT_VUT_MEAN}{Gross Primary Production, from Nighttime partitioning method, based on NEE_VUT_MEAN (umol Co2 m-2 s-1)}
#' \item{GPP_NT_VUT_SE}{Standard Error for Gross Primary Production,calculated as (SD(GPP_NT_VUT_XX) / SQRT(40))}
#' \item{RECO_DT_VUT_MEAN}{Ecosystem respiration, from dayttime partitioning method, average from RECO versions, each from corresponding NEE_VUT_XX version}
#' \item{RECO_DT_VUT_SE}{Standard Error for ecosystem respiration, calculated as (SD(GPP_DT_VUT_XX) / SQRT(40))}
#' \item{GPP_DT_VUT_MEAN}{Gross Primary Production, from dayttime partitioning method, based on NEE_VUT_MEAN (umol Co2 m-2 s-1)}
#' \item{GPP_DT_VUT_SE}{Standard Error for Gross Primary Production,calculated as (SD(GPP_DT_VUT_XX) / SQRT(40))}
#' ...
#' }
#' @source \url{http://fluxnet.fluxdata.org/data/fluxnet2015-dataset/}
"EddySample"

