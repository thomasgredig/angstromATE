test_that("Test Status File", {
  fileName <- ATE.sampleFiles('_Status')
  df <- ATE.status(fileName)

  expect_equal(nrow(df), 19)
})
