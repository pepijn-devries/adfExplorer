test_that("Device bitmap info makes sense", {
  expect_true({
    my_device <- demo_adf()
    bm <- get_adf_bitmap(my_device)
    result <- sum(!bm)*512 == bytes_free(my_device)
    close(my_device)
    result
  })
})
