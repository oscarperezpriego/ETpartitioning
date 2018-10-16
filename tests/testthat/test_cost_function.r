#require(testthat)
context("cost_function")

test_that("cost_funciton",{
  tmp <-  EddySample[ EddySample$TIMESTAMP_START>  201405150000,]
  tmp <-  tmp[tmp$TIMESTAMP_START<  201405160000,]
			ans <- cost_function(par=c(200, 0.2, 25, 0.6)
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
			expect_true(is.vector(ans))
			expect_true(is.numeric(ans))
			# expect_equal(length(ans), 1)
			# answer from previous run, might be wrong
			expect_equal( mean(ans, na.rm=T), 4, tolerance = 1 )
		})

test_that("data frame with wrong column names",{
  expect_error(
    ans <- cost_funciton(par=c(200, 0.2, 25, 0.5), EddySample, "GPP_NT_VUT_USTAR50_bla", "VPD_F?_foobar", "TA_F",  Chi_o= 1, WUE_o)
)
    })

