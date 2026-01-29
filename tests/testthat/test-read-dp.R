test_that("can read DP file from PJNZ", {
  pjnz <- system_file(
    "pjnz", "bwa_aim-adult-art-no-special-elig_v6.13_2022-04-18.PJNZ")
  dp <- read_dp(pjnz)

  expect_equal(names(dp), c("data", "dim_vars"))

  ## All data has data and tag or is null (null if not found in data)
  nms <- lapply(dp$data, names)
  expect_true(all(vlapply(nms, function(x) setequal(x, c("data", "tag")) | is.null(x))))

  ## Years are set correctly
  expect_equal(dp$dim_vars$years, as.character(1970:2030))

  ## Fixed dims are set
  fixed_dims <- get_static_dim_vars()
  expect_setequal(names(dp$dim_vars),
                  c(names(fixed_dims), "years"))

  ## Data types are set correctly int and real
  expect_equal(dp$data$final_year$data, 2030)
  expect_equal(dp$data$final_year$tag, "FinalYear MV2")
  expect_true(is.integer(dp$data$final_year$data))
  expect_null(dimnames(dp$data$final_year$data))

  expect_true(is.numeric(dp$data$big_pop$data))
  expect_equal(dimnames(dp$data$big_pop$data),
               list(fixed_dims$a, fixed_dims$s, dp$dim_vars$years))

  ## data with skip read correctly cd4_distribution_15_49
  ## this is a bit of a weak check, but will do
  expect_true(!any(is.na(dp$data$cd4_distribution_15_49$data)))

  ## data with start offset read correctly aids_deaths_no_art_single_age
  expect_true(!any(is.na(dp$data$aids_deaths_no_art_single_age$data)))
})

test_that("error thrown if trying to read invalid file format", {
  t <- tempfile(fileext = ".txt")

  expect_error(
    read_dp(t),
    "Invalid file format. This function can only read '.pjnz' or '.zip' files."
  )
})

test_that("error thrown if no DP file found in zip", {
  t <- tempfile()
  dir.create(t, FALSE, TRUE)
  utils::write.csv(c(1, 2, 3), file.path(t, "myfile.csv"))
  zip_file <- tempfile(fileext = ".zip")
  zip::zip(zip_file, "myfile.csv", root = t)


  expect_error(
    read_dp(zip_file),
    "0 '.DP' files found. Expected 1."
  )
})

test_that("message raised when an expected tag is not present", {
  pjnz <- system_file(
    "pjnz", "bwa_aim-adult-art-no-special-elig_v6.13_2022-04-18.PJNZ")

  expect_warning(
    dp <- read_dp(pjnz),
    "Tag not found in DP for 'incidence_input', returning NULL."
  )
})

test_that("error raised if variable with allow_null false is NULL", {
  pjnz <- system_file(
    "pjnz", "bwa_aim-adult-art-no-special-elig_v6.13_2022-04-18.PJNZ")
  dp_raw <- read_dp_raw(pjnz)

  dim_vars <- get_dim_vars(dp_raw)
  metadata <- c(get_years_cfg(), get_pars_metadata(dim_vars))

  metadata$incidence_input$allow_null <- FALSE

  expect_error(get_data_from_cfg("incidence_input",
                                 metadata$incidence_input,
                                 dim_vars,
                                 dp_raw),
               "No tag found for 'incidence_input'")
})
