% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculate_WUE_o.r
\name{calculate_WUE_o}
\alias{calculate_WUE_o}
\title{Calculation of WUE_o}
\usage{
calculate_WUE_o(data, ColPhotos, ColVPD, ColTair, C, Z)
}
\arguments{
\item{data}{Data.frame or matrix containing all required variables.}

\item{ColPhotos}{column name of numeric vector containing time series of photosynthesis data (umol CO2 m-2 s-1)}

\item{ColVPD}{column name of numeric vector containing time series of vapor pressure deficit (kPa).}

\item{ColTair}{column name of numeric vector containing time series of air temperature (deg C).}

\item{C}{Empirical coeficient for C3 species.}

\item{Z}{Z- numeric value defining elevation (km).}
}
\value{
a numeric value:
        \item{WUE_o}{long-term effective "Water use efficiency (umolCO2 mmol-1)}
}
\description{
Calculation of WUE_o (see Perez-Priego et al., 2018).
}
\details{
the following metrics are calculated:

         WUE_o:

           \deqn{WUE_o <- (390*(1-chi_o)*96)/(1.6*VPD_g)*0.001}

         VPD_g is calculated based on the mean value of the growing period.
         While chi_o is calculated using calculate_chi_o function
         The growing period is estimated as those periods over the 85 quantile of Ph.
}
\note{
Photos is calculated based on night-time NEE partitioning approach (Reichstein et al., 2005).
        chi_o is calculated following Perez-Priego et al., 2018.
}
\examples{

calculate_WUE_o(data= EddySample
,ColPhotos = "GPP_NT_VUT_MEAN"
,ColVPD = "VPD_F"
,ColTair = "TA_F"
,C = 1.189 ##<< Empirical coeficient for C3 species (see Wang et al., 2017; Plant Nature).
,Z=0.27)
}
\references{
Perez-Priego, O., G. Katul, M. Reichstein et al. Partitioning eddy covariance
            water flux components using physiological and micrometeorological approaches,
            Journal of Geophysical Research: Biogeosciences. In press


            Reichstein, M., et al. (2005), On the separation of net ecosystem exchange
            into assimilation and ecosystem respiration: review and improved algorithm,
            Global Change Biology, 11(9), 1424-1439.
}
