#require(testthat)
context("transpiration_model")

test_that("transpiration_model",{
  par <- c(200, 0.2, 25, 0.6)
   WUE_o <-  calculate_WUE_o(EddySample, ColPhotos = "GPP_NT_VUT_MEAN", ColVPD = "VPD_F", ColTair = "TA_F", C = 1.189, Z=0.27 )
   Chi_o <-  calculate_chi_o(EddySample, ColPhotos = "GPP_NT_VUT_MEAN", ColVPD = "VPD_F", ColTair = "TA_F", C = 1.189, Z=0.27 )
  tmp <-  EddySample[ EddySample$TIMESTAMP_START>  201405150000,]
  tmp <-  tmp[tmp$TIMESTAMP_START<  201405160000,]
  #
  ans <- transpiration_model(
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
			expect_equal(length(ans), 47)
			# answer from previous run, might be wrong
			expect_equal( as.numeric(mean(ans, na.rm=TRUE)), 0.36, tolerance = 0.1 )
		})
