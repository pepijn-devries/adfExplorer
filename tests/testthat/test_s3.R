test_that("adf_file_con can be summarised", {
  expect_no_error({
    my_device <- demo_adf()
    con <- adf_file_con(my_device, "s/startup-sequence")
    summary(con)
    close(con)
    close(my_device)
  })
})

test_that("Can seek an adf_file_con", {
  expect_true({
    my_device <- demo_adf()
    con <- adf_file_con(my_device, "s/startup-sequence")
    txt1 <- readLines(con, 1L) |> suppressWarnings()
    seek(con, 0)
    txt2 <- readLines(con, 1L) |> suppressWarnings()
    close(con)
    close(my_device)
    all(txt1 == txt2)
  })
})

test_that("Can specify `what` as value", {
  expect_no_error({
    my_device <- demo_adf()
    con <- adf_file_con(my_device, "devs/system-configuration")
    i <- readBin(con, 1L)
    close(con)
    close(my_device)
  })
})

test_that("`writeLines` works without errors", {
  expect_no_error({
    my_device <- demo_adf(write_protected = FALSE)
    con <- adf_file_con(my_device, "foobar", writable = TRUE)
    writeLines("No errors here", con)
    close(con)
    close(my_device)
  })
})