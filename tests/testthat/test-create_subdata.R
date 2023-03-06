test_that("remove_lasts_value works", {
  # setup
  inputs <- list(
    a = matrix(1:10, nrow = 10, ncol = 1),
    b = matrix(c(1:9, -1), nrow = 10, ncol = 1),
    c = matrix(c(1:8, -1, -1), nrow = 10, ncol = 1),
    d = matrix(c(1:5, -1, 7, -1, -1, -1), nrow = 10, ncol = 1),
    e = matrix(rep(-1, 10), nrow = 10, ncol = 1)
  )

  expected <- list(
    a = matrix(1:10, nrow = 10, ncol = 1),
    b = matrix(c(1:9), nrow = 9, ncol = 1),
    c = matrix(c(1:8), nrow = 8, ncol = 1),
    d = matrix(c(1:5, -1, 7), nrow = 7, ncol = 1),
    e = matrix(rep(-1L, 0), nrow = 0, ncol = 1)
  )


  remove_lasts_value(inputs[[2]])

  # eval
  res <- purrr::map(inputs, remove_lasts_value)

  # test
  expect_equal(res, expected)

})
