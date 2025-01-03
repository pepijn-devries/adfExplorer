test_that("Virtual path can be tested for existence", {
  expect_false({
    my_device <- demo_adf()
    vp <- virtual_path(my_device, "idontexist")
    result <- adf_file_exists(my_device, vp)
    close(my_device)
    result
  })
})

test_that("Virtual paths can be tested for existence", {
  expect_no_error({
    my_device <- demo_adf()
    vp <- virtual_path(my_device, c("s", "idontexist"))
    adf_file_exists(my_device, vp)
    close(my_device)
  })
})

test_that("Virtual paths can be tested for dir existence", {
  expect_no_error({
    my_device <- demo_adf()
    vp <- virtual_path(my_device, c("s", "idontexist"))
    adf_dir_exists(my_device, vp)
    close(my_device)
  })
})

test_that("Virtual paths can produce information", {
  expect_no_error({
    my_device <- demo_adf()
    vp <- virtual_path(my_device, c("s", "devs"))
    adf_entry_info(my_device, vp)
    close(my_device)
  })
})

test_that("Multiple paths can produce information", {
  expect_no_error({
    my_device <- demo_adf()
    adf_entry_info(my_device, c("s", "devs"))
    close(my_device)
  })
})

test_that("Virtual paths can produce information directly", {
  expect_no_error({
    my_device <- demo_adf()
    vp <- virtual_path(my_device, c("s", "devs"))
    adf_entry_info(vp)
    adf_entry_info(vp[[1]])
    close(my_device)
  })
})

test_that("File connections can produce information", {
  expect_no_error({
    my_device <- demo_adf()
    con <- adf_file_con(virtual_path(my_device, "s/startup-sequence"))
    adf_entry_info(con)
    close(con)
    close(my_device)
  })
})

test_that("Name can be obtained for both files and directories", {
  expect_no_error({
    my_device <- demo_adf()
    adf_entry_name(my_device, c("df0:", "s", "s/startup-sequence"))
    close(my_device)
  })
})

test_that("Name can be set with a connection", {
  expect_no_error({
    my_device <- demo_adf(write_protected = FALSE)
    con <- adf_file_con(my_device, "s/startup-sequence")
    adf_entry_name(con) <- "a"
    close(my_device)
  })
})

test_that("Name can be set with virtual paths 1", {
  expect_no_error({
    my_device <- demo_adf(write_protected = FALSE)
    vp <- virtual_path(my_device, c("df0:", "s/startup-sequence", "s"))
    adf_entry_name(vp) <- c("a", "b", "c")
    close(my_device)
  })
})

test_that("Name can be set with virtual paths 2", {
  expect_no_error({
    my_device <- demo_adf(write_protected = FALSE)
    vp <- virtual_path(my_device, c("df0:", "s/startup-sequence", "s"))
    adf_entry_name(my_device, vp) <- c("a", "b", "c")
    close(my_device)
  })
})

test_that("Multiple names can be set at once", {
  expect_no_error({
    my_device <- demo_adf(write_protected = FALSE)
    adf_entry_name(my_device, c("df0:", "s/startup-sequence", "s")) <- c("a", "b", "c")
    close(my_device)
  })
})
