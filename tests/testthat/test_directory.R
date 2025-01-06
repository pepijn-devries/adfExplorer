test_that("Directory can be set using a virtual path", {
  expect_no_error({
    my_device <- demo_adf()
    target <- virtual_path(my_device, "s")
    adf_directory(my_device) <- target
    close(my_device)
  })
})

test_that("Directory existence can be tested for multiple directories", {
  expect_no_error({
    my_device <- demo_adf()
    vp <- virtual_path(my_device, c("s", "devs"))
    adf_dir_exists(vp)
    close(my_device)
  })
})

test_that("File existence can be tested for multiple directories", {
  expect_no_error({
    my_device <- demo_adf()
    vp <- virtual_path(my_device, c("s", "devs"))
    adf_file_exists(vp)
    close(my_device)
  })
})