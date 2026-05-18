test_that("loading recipe file", {
  filename <- ATE.sampleFiles("rcp$")[1]
  df <- ATE.readRecipe(filename)
  expect_equal(nrow(df),2)
  expect_equal(df$pre_vacuum_pressure[1],1e-6)
})
