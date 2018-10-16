#require(testthat)
context("calculate_chi_o")

test_that("calculate_chi_o",{
  # dataTest = structure(list(Photos = c(-2.40437006950378, -2.74130988121033,
  #                                  -2.457279920578, -2.62785005569458, -2.72841000556946, -2.72528004646301
  # ), VPD = c(0.132400000095367, 0.233400011062622, 0.25220000743866,
  #            0.21470000743866, 0.225699996948242, 0.367700004577637)
  # , Tair = c(6.19799995422363,
  #            3.50399994850159, 3.53900003433228, 3.46900010108948, 4.38700008392334,
  #            6.20100021362305))
  # , .Names = c("Photos", "VPD", "Tair"), row.names = c(NA,
  #                                                      6L), class = "data.frame")
			ans <- calculate_chi_o(EddySample, "GPP_NT_VUT_MEAN", "VPD_F", "TA_F", C = 1.1, Z = 0.2)
			expect_true(is.vector(ans))
			expect_true(is.numeric(ans))
			expect_equal(length(ans), 1)
			# answer from previous run, might be wrong
			expect_equal( ans, 0.88, tolerance = 0.1 )
		})

test_that("calculate_chi_o with wrong column names",{
  expect_error(
    ans <- calculate_chi_o(EddySample, "GPP_NT_VUT_USTAR50_bla", "VPD_F?_foobar", "TA_F", C = 1.1, Z = 0.2)
    ,"GPP_NT_VUT_USTAR50_bla")
})
