test_that("multiplication works", {
  # setup
  a_tensor <- array(as.double(c(1:12)), dim = c(3, 4)) |>
    tensorflow::as_tensor()
  scalars <- 1:4

  # eval
  res <- layer_rescale_1d(a_tensor, scalars = scalars)

  # test
  expected <- array(
    c(1, 2, 3, 8, 10, 12, 21, 24, 27, 40, 44, 48),
    dim = c(3, 4)
  ) |>
    tensorflow::as_tensor()
  res |> expect_equal(expected)
})
