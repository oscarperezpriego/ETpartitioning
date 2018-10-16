#require(testthat)
context("photos_model")

test_that("photos_model",{
  tmp <-  EddySample[ EddySample$TIMESTAMP_START>  201405150000,]
  tmp <-  tmp[tmp$TIMESTAMP_START<  201405160000,]
			ans <- photos_model(par=c(200, 0.2, 25, 0.6), data=tmp
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
			                    ,Chi_o = 0.88)
			expect_true(is.vector(ans))
			expect_true(is.numeric(ans))
			# expect_equal(length(ans), 1)
			# answer from previous run, might be wrong
			expect_equal( mean(ans, na.rm=T), 2.16, tolerance = 1 )
		})

test_that("photos_model with wrong column names",{
  expect_error(
    ans <- photos_model(par=c(200, 0.2, 25, 0.5), EddySample, "GPP_NT_VUT_MEAN_bla", "VPD_F?_foobar", "TA_F",  Chi_o= 1)
)
    })
