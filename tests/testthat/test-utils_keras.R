test_that("layer_rescale_1d works", {
  # setup
  tensor_float <- tf$Variable(
    as_tensor(c(1, 2, 3, 4, 5, 6, 7, 8), shape = c(2, 4))
  )
  tensor_int <- as_tensor(1:8, shape = c(2, 4))
  scalars <- 1:4

  # eval
  res_float <- tensor_float |>
    layer_rescale_1d(scalars)
  res_int <- tensor_float |>
    layer_rescale_1d(scalars)

  # test
  expected_values <- c(1, 4, 9, 16, 5, 12, 21, 32)
  expected_flt <- as.array(as_tensor(expected_values, shape = c(2, 4)))
  expected_int <- as_tensor(
    as.integer(expected_values), shape = c(2, 4)
  ) |> as.array()

  as.array(res_float) |> expect_equal(expected_flt)
  as.array(res_int) |> expect_equal(expected_int)
})

test_that("layer_rescale_1d works on higher dimensions", {
  # setup
  tensor <- tf$Variable(as_tensor(1:24, shape = c(2, 3, 4)))
  scalars <- 1:4

  # eval
  res <- tensor |>
    layer_rescale_1d(scalars)

  # test
  expected_values <- t(t(1:24) * 1:4)
  expected <- as.array(as_tensor(expected_values, shape = c(2, 3, 4)))

  as.array(res) |> expect_equal(expected)
})

