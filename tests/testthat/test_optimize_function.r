#require(testthat)
context("optimize_function")

test_that("optimize_function",{
  tmp <-  EddySample[ EddySample$TIMESTAMP_START>  201105150000,]
  tmp <-  tmp[tmp$TIMESTAMP_START<  201105160000,]
			ans <-  optimal_parameters(par_lower= c(0,0, 10, 0)
			                                ,par_upper = c(400,0.4, 30, 1)
			                                ,data=tmp
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
			                                ,Chi_o = 0.88
			                                ,WUE_o= 22.5)

			expect_equal(length(ans), 4)
			# answer from previous run, might be wrong
			expect_equal( as.numeric(ans[1]), 382.0955, tolerance = 20 )
		})
