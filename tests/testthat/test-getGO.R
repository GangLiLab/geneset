context("test-getGO")

test_that("getGO works", {
  expect_equal(getGO(org = "human",ont = "mf", data_dir = tempdir()) %>% .[[4]], "mf")
})
