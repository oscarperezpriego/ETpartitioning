#require(testthat)
context("gc_model")

test_that("gc_model",{
  tmp <-  EddySample[ EddySample$TIMESTAMP_START>  201405150000,]
  tmp <-  tmp[tmp$TIMESTAMP_START<  201405160000,]
			ans <- mean(gc_model(par=c(200, 0.2, 25), data=tmp, Q="PPFD_IN", VPD="VPD_F", Tair="TA_F", gcmax = 1))
			expect_true(is.vector(ans))
			expect_true(is.numeric(ans))
			# expect_equal(length(ans), 1)
			# answer from previous run, might be wrong
			expect_equal( ans, 0.26, tolerance = 0.1 )
		})

test_that("gc_model with wrong column names",{
  expect_error(
    ans <- gc_model(par=c(200, 0.2, 25), EddySample, "PPFD_IN_bla", "VPD_F?_foobar", "TA_F", gmax = 1)
)
    })
