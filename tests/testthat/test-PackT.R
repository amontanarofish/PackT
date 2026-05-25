test_that("m50() multiplies value by 50", {
  expect_equal(m50(2), 100)
  expect_equal(m50(0), 0)
  expect_equal(m50(-1), -50)
}
)
