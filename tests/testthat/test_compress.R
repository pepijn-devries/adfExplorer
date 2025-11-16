test_that("Compress and decompress work", {
  expect_no_error({
    my_file1 <- tempfile(fileext = ".adf")
    my_file2 <- tempfile(fileext = ".adz")
    create_adf_device(my_file1, connect = FALSE)
    compress_adf(my_file1, my_file2)
    my_device <- connect_adf(my_file2)
    close(my_device)
  })
})
