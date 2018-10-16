###############################
### ET partitioning
###############################

library(ETpartitioning)
library(FME)


#-- Estimating long-term effective Chi and WUE parameters

Chi_o <- calculate_chi_o(data= EddySample
                         ,ColPhotos = "GPP_NT_VUT_MEAN"
                         ,ColVPD = "VPD_F"
                         ,ColTair = "TA_F"
                         ,C = 1.189 ##<< Empirical coeficient for C3 species (see Wang et al., 2017; Plant Nature).
                         ,Z=0.27) ##<< altitude (km)


WUE_o <- calculate_WUE_o(data= EddySample
                         ,ColPhotos = "GPP_NT_VUT_MEAN"
                         ,ColVPD = "VPD_F"
                         ,ColTair = "TA_F"
                         ,C = 1.189 ##<< Empirical coeficient for C3 species (see Wang et al., 2017; Plant Nature).
                         ,Z=0.27)

##-- put data format and creating an index for optimazing the model in a 5-days window
ds <- EddySample


ds$year <- (substr(ds$TIMESTAMP_END, 1, 4))
ds$Month <- as.numeric(substr(ds$TIMESTAMP_END, 5, 6))
ds$DD <- as.numeric(substr(ds$TIMESTAMP_END, 7, 8))
ds$Hour <- (substr(ds$TIMESTAMP_END, 9, 10))
ds$Min <-(substr(ds$TIMESTAMP_END, 11, 12))
ds$rDate <- strptime(paste0(ds$year,"/",ds$Month,"/",ds$DD," ", ds$Hour,":",ds$Min) , format="%Y/%m/%d %H:%M", tz='GMT')
ds$date <- strptime(paste0(paste0(ds$year,"/",ds$Month,"/",ds$DD)) , format="%Y/%m/%d", tz='GMT')
ds$loop <- as.numeric(as.Date(ds$date))-min(as.numeric(as.Date(ds$date)), na.rm=T)+1




##-- ET Partitioning into transpiration and evaporation components
##-- The optimization is expensive and only one month is provided as an example.

ds <- subset(ds, year == 2014 & Month == 5)

var_list2 <- unique(ds$loop)
Liston <-  list()

for (i in var_list2){

  # i <- 5240

  tmp <- subset(ds,loop %in% c(i-2,i-1, i,i+1, i+2)) ##<< Defining 5 days window in a loop

  tmpp <- subset(tmp, NIGHT == 0)
  # ds <- subset(ds, daytime==1)

  #-- optimzazing model parameters

  ans <-  optimal_parameters(par_lower= c(0,0, 10, 0)
                              ,par_upper = c(400,0.4, 30, 1)
                              ,data=tmpp
                              ,ColPhotos="GPP_NT_VUT_MEAN"
                              ,ColPhotos_unc="NEE_VUT_USTAR50_JOINTUNC"
                              ,ColH="H_F_MDS"
                              ,ColVPD="VPD_F"
                              ,ColTair="TA_F"
                              ,ColPair="PA_F"
                              ,ColQ="PPFD_IN"
                              ,ColCa="CO2_F_MDS"
                              ,ColUstar="USTAR"
                              ,ColWS="WS_F"
                              ,ColSW_in="SW_IN_F"
                              ,Chi_o = Chi_o
                              ,WUE_o= WUE_o)

 par <- as.numeric(ans)

  #-- Estimating transpiration rates
transpiration_mod <- transpiration_model(
  par=par
  ,data=tmp
  ,ColPhotos="GPP_NT_VUT_MEAN"
  ,ColH="H_F_MDS"
  ,ColVPD="VPD_F"
  ,ColTair="TA_F"
  ,ColPair="PA_F"
  ,ColQ="PPFD_IN"
  ,ColCa="CO2_F_MDS"
  ,ColUstar="USTAR"
  ,ColWS="WS_F"
  ,ColSW_in="SW_IN_F"
  ,Chi_o = Chi_o
  ,WUE_o= WUE_o)

  #-- Estimating evaporation rates


  landa <- (3147.5-2.37*(tmp$TA_F+273.15))*1000 # Latent heat of evaporisation [J kg-1]
  ET <- tmp$LE_F_MDS/landa*1000000/18 # from Wm-2 to mmol m-2 s-1
  evaporation_mod <- ET-transpiration_mod

  #-- ET partitioning

  tmp$ET <- ET
  tmp$transpiration_mod <- transpiration_mod
  tmp$evaporation_mod <- evaporation_mod

  tmp <- tmp[tmp$loop == i,] ## selecting the central day
  Liston[[i]] <- tmp

}

out <- do.call(rbind, Liston)


##-- Plotting montly mean diurnal cycle

ET <- aggregate(out$ET, by=list(out$Hour, out$Month), FUN=mean, na.rm=TRUE)
transpiration <- aggregate(out$transpiration_mod, by=list(out$Hour, out$Month), FUN=mean, na.rm=TRUE)
evaporation <- aggregate(out$evaporation_mod, by=list(out$Hour, out$Month), FUN=mean, na.rm=TRUE)
par(oma=c(1,1,1,1),mar=c(5, 5, 5, 4))
plot(ET$Group.1, ET$x, type="l", ylab=expression(water~flux~(mmol~m^-2~s^-1)), xlab="hour")
lines(transpiration$Group.1, transpiration$x, col="green")
lines(evaporation$Group.1, evaporation$x, col="red")
legend("topleft", legend = c("ET", "plant transpiration",  "evaporation"), bty = "n",
lwd=c(1,1,1), cex = 1, col = c( "black","green","red"), pch = c(1, 1,1)) 





