test_that("Blocks can be read and written", {
  expect_no_error({
    my_device <- demo_adf(write_protected = FALSE)
    bl <- read_adf_block(my_device, 0)
    write_adf_block(my_device, 1700, bl)
    write_adf_block(my_device, 1700, raw(512))
    new_adf_block()
    close(my_device)
  })
})
