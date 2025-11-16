test_that("Device type can be obtained", {
  expect_equal({
    my_device <- demo_adf()
    result <- adf_dev_type(my_device)
    close(my_device)
    result
  }, "Floppy DD")
})

test_that("Device capacity can be obtained", {
  expect_equal({
    my_device <- demo_adf()
    result <- device_capacity(my_device)
    close(my_device)
    result
  }, 901120L)
})

test_that("Volume capacity can be obtained", {
  expect_equal({
    my_device <- demo_adf()
    result <- volume_capacity(my_device)
    close(my_device)
    result
  }, 901120L)
})

test_that("Volume name can be obtained", {
  expect_equal({
    my_device <- demo_adf()
    result <- volume_name(my_device)
    close(my_device)
    result
  }, "adfExampleOFS")
})

test_that("Volume name can be set", {
  expect_equal({
    my_device <- demo_adf(write_protected = FALSE)
    volume_name(my_device) <- "foobar"
    result <- volume_name(my_device)
    close(my_device)
    result
  }, "foobar")
})

test_that("Number of volumes can be obtained", {
  expect_equal({
    my_device <- demo_adf()
    result <- n_volumes(my_device)
    close(my_device)
    result
  }, 1L)
})

test_that("Bootability can be checked", {
  expect_true({
    my_device <- demo_adf()
    result <- is_bootable(my_device)
    close(my_device)
    result
  })
})

test_that("Filesystem can be checked", {
  expect_false({
    my_device <- demo_adf()
    result <- is_fast_file_system(my_device)
    close(my_device)
    result
  })
})

test_that("System mode can be checked", {
  expect_true({
    my_device <- demo_adf()
    result <- is_international(my_device)
    close(my_device)
    result
  })
})

test_that("Dircache can be checked", {
  expect_false({
    my_device <- demo_adf()
    result <- is_dircache(my_device)
    close(my_device)
    result
  })
})

test_that("Protection can be checked", {
  expect_true({
    my_device <- demo_adf()
    result <- is_write_protected(my_device)
    close(my_device)
    result
  })
})
