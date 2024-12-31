test_that("Directory can be set using a virtual path", {
  expect_no_error({
    my_device <- demo_adf()
    target <- virtual_path(my_device, "s")
    adf_directory(my_device) <- target
  })
})