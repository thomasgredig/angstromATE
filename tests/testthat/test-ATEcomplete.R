test_that("read ATE data sets", {
  fileName <- ATE.sampleFiles('_Complete_')
  d1 <- ATE.complete(fileName, TRUE)
  expect_equal(nrow(d1),1)
  d2 <- ATE.complete(fileName)
  expect_equal(nrow(d2),103)
})
