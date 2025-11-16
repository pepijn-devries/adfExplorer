test_that("Compress and decompress work", {
  expect_no_error({
    my_file1 <- tempfile(fileext = ".adf")
    my_device <- create_adf_device(my_file1, write_protected = FALSE)
    prepare_adf_device(my_device, ffs = TRUE, international = TRUE, dircache = TRUE)
    close(my_device)
    my_device <- create_adf_device(my_file1, write_protected = FALSE)
    prepare_adf_device(my_device, ffs = FALSE, international = FALSE, dircache = FALSE)
    close(my_device)
  })
})
