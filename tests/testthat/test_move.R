test_that("Copy vp to vp works", {
  expect_no_error({
    my_device <- demo_adf(write_protected = FALSE)
    vp1 <- virtual_path(my_device, "s/startup-sequence")
    vp2 <- virtual_path(my_device, "DF0:")
    copy_adf_entry(vp1, vp2)
    close(my_device)
  })
})

test_that("Copy vp to character works", {
  expect_no_error({
    my_device <- demo_adf()
    vp <- virtual_path(my_device, "s/startup-sequence")
    check_dest <- file.path(tempdir(), adf_entry_name(vp))
    if (!file.exists(check_dest))
      copy_adf_entry(vp, tempdir())
    close(my_device)
  })
})

test_that("Move character to vp works", {
  expect_no_error({
    my_device <- demo_adf(write_protected = FALSE)
    f <- tempfile()
    file.create(f)
    vp <- virtual_path(my_device, "DF0:")
    move_adf_entry(f, vp)
    close(my_device)
  })
})

test_that("Move vp to vp works", {
  expect_no_error({
    my_device <- demo_adf(write_protected = FALSE)
    vp1 <- virtual_path(my_device, "s/startup-sequence")
    vp2 <- virtual_path(my_device, "DF0:")
    move_adf_entry(vp1, vp2)
    close(my_device)
  })
})
