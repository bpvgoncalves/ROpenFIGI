
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
  p <- httr::use_proxy("5.75.134.209", 40000)
  # Do not run in CRAN, just in case the proxy becomes unavailable.
  skip_on_cran()

  jsonrst2 <- OpenFIGI(jsondf, proxy = p)
  expect_identical(jsonrst, jsonrst2)

  finalrst2 <- OpenFIGI_MappingCreator(jsondf, proxy = p)
  expect_identical(finalrst, finalrst2)

})
