test_that("check sample files are available", {
  fileList <- ATE.sampleFiles()
  expect_equal(length(fileList), 5)
})


test_that("ATE import", {
  fileName <- ATE.sampleFiles('csv')
  expect_equal(length(fileName), 1)

  df <- ATE.import(fileName)
  m <- ATE.info(fileName)

  expect_equal(dim(df), c(1094, 50))
})


test_that("ATE log file is not found", {
  fileName <- "nonexistant.filename"
  expect_error(ATE.import(fileName),
               "ATE log file is not found.", fixed=TRUE)
})
