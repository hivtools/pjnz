test_that("metadata is well formed", {
  pjnz <- system_file(
    "pjnz", "bwa_aim-adult-art-no-special-elig_v6.13_2022-04-18.PJNZ")
  dp_raw <- read_dp_raw(pjnz)

  dim_vars <- get_dim_vars(dp_raw)
  metadata <- c(get_years_cfg(), get_pars_metadata(dim_vars))

  schema <- system_file("metadata.schema.json")
  json_data <- jsonlite::toJSON(metadata, auto_unbox = TRUE)
  valid <- jsonvalidate::json_validate(json_data, schema,
                                       verbose = TRUE,
                                       strict = TRUE,
                                       engine = "ajv")

  expect_true(valid)

  ## Check all dims are valid
  all_dims <- unique(unlist(lapply(metadata, function(meta) {
    lapply(meta$read, function(x) {
      x$dims
    })
  }), recursive = TRUE, use.names = FALSE))

  fixed_dims <- get_static_dim_vars()

  expect_true(all(all_dims %in% c(names(fixed_dims), "years")))
})
