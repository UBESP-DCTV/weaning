test_that("share_objects works", {
  # setup
  test_proj_shared_path <- tempdir(check = TRUE) |>
    normalizePath()
  output_data_folder <- Sys.getenv("OUTPUT_DATA_FOLDER")

  test_output_data_path <- file.path(
    test_proj_shared_path,
    "/../",
    Sys.getenv("OUTPUT_DATA_FOLDER")
    ) |>
    normalizePath(mustWork = FALSE)

  fs::dir_create(test_output_data_path)
  withr::defer(fs::dir_delete(test_output_data_path))

  withr::local_envvar(
    list(PRJ_SHARED_PATH = test_proj_shared_path)
  )

  sample_object <- 1
  second_object <- 2

  objects_list <- list(
    sample_object = sample_object,
    second_object = second_object
  )


  # evaluation
  res <- share_objects(objects_list, use_date_version = FALSE)

  # tests
  expect_error(
    share_objects(sample_object),
    "Must be of type 'list'"
  )




  sample_object_shared_path <- c(
    sample_object = normalizePath(file.path(
      test_output_data_path, "sample_object.rds"
    )),
    second_object = normalizePath(file.path(
      test_output_data_path, "second_object.rds"
    ))
  )

  expect_equal(res, sample_object_shared_path)




  sample_object_shared_path |>
    purrr::walk(expect_file_exists)



  parsed_now <- stringr::str_replace_all(lubridate::now(), "\\W", "") |>
    stringr::str_trunc(12, ellipsis = "")

  expect_equal(
    share_objects(objects_list),
    c(
      sample_object = normalizePath(file.path(
        test_output_data_path, paste0(parsed_now, "-sample_object.rds")
      )),
      second_object = normalizePath(file.path(
        test_output_data_path, paste0(parsed_now, "-second_object.rds")
      ))
    )
  )
})
