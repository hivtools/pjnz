test_that("can read DP file from PJNZ", {
  pjnz <- system_file(
    "pjnz", "bwa_aim-adult-art-no-special-elig_v6.13_2022-04-18.PJNZ")
  dp <- read_dp(pjnz)

  expect_equal(names(dp), c("data", "dim_vars"))
  expect_true("big_pop" %in% names(dp$data))
})
