
jsondf <- sampleOpenFIGIdf()

jsonrst <- OpenFIGI(jsondf)


test_that("number of returned json list", {
  expect_equal(length(jsonrst), 2L)
  expect_equal(names(jsonrst[[1L]]),
               c("figi", "name", "ticker",
                 "exchCode", "compositeFIGI", "uniqueID",
                 "securityType", "marketSector", "shareClassFIGI",
                 "uniqueIDFutOpt", "securityType2", "securityDescription"))
})

finalrst <- OpenFIGI_MappingCreator(jsondf)

test_that("number of returned json list", {
  expect_equal(ncol(finalrst), 32L )
  expect_equal(finalrst$ID_ISIN[1L], "US4592001014" )
})


test_that("connections via proxy work", {

  # The following web proxy was available at 'https://hidemy.name/en/proxy-list/' as of the
  # written of this test. This may need to be changed overtime.
  p <- httr::use_proxy("157.245.27.9", 3128)
  # Do not run in CRAN, just in case the proxy becomes unavailable.
  skip_on_cran()

  jsonrst2 <- OpenFIGI(jsondf, proxy = p)
  expect_identical(jsonrst, jsonrst2)

  jsonrst3 <- OpenFIGI(jsondf, proxy = "auto")
  expect_identical(jsonrst, jsonrst3)

  expect_error(OpenFIGI(jsondf, proxy = "bad_input"))

  finalrst2 <- OpenFIGI_MappingCreator(jsondf, proxy = p)
  expect_identical(finalrst, finalrst2)

  finalrst3 <- OpenFIGI_MappingCreator(jsondf, proxy = "auto")
  expect_identical(finalrst, finalrst3)

  expect_error(OpenFIGI_MappingCreator(jsondf, proxy = "bad_input"))

})
