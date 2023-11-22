test_that("conversion to seconds from string", {
  noSeconds = conv2seconds("02:35:40.18163")
  expect_equal(noSeconds, 2*3600 + 35*60 + 40.18163)

  noSeconds = conv2seconds("00:00:40.18163")
  expect_equal(noSeconds, 40.18163)

  noSeconds = conv2seconds("01:00:40.18163")
  expect_equal(noSeconds, 3600 + 40.18163)

  noSeconds = conv2seconds("1.19:07:06") - conv2seconds("19:07:06")
  expect_equal(noSeconds, 24*3600)
})
