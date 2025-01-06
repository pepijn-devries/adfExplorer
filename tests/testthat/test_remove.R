test_that("Current directory can be removed", {
  expect_no_error({
    my_device <- demo_adf(write_protected = FALSE)
    adf_directory(my_device) <- "s"
    remove_adf_entry(my_device, "startup-sequence")
    remove_adf_entry(my_device)
    close(my_device)
  })
})

test_that("Files can be removed using a virtual path", {
  expect_no_error({
    my_device <- demo_adf(write_protected = FALSE)
    vp <- virtual_path(my_device, "s/startup-sequence")
    remove_adf_entry(my_device, vp)
    close(my_device)
  })
})