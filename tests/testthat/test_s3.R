sink(tempfile("sink"))

test_that("adf_file_con can be summarised", {
  expect_no_error({
    my_device <- demo_adf()
    con <- adf_file_con(my_device, "s/startup-sequence")
    summary(con)
    fm <- format(con)
    print(con)
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

test_that("`readBin` can read different types without problems", {
  expect_no_error({
    my_device <- demo_adf()
    con <- adf_file_con(my_device, "devs/system-configuration")
    char   <- readBin(con, "character")
    cmplx  <- readBin(con, "complex")
    cmplx  <- readBin(con, "complex", endian = "swap")
    intngr <- readBin(con, "integer")
    intngr <- readBin(con, "integer", endian = "swap")
    dbl    <- readBin(con, "double")
    dbl    <- readBin(con, "double", endian = "swap")
    lgcl   <- readBin(con, "logical")
    rw     <- readBin(con, "raw")
    close(con)
    close(my_device)
  })
})

test_that("`writeBin` can write different types without problems", {
  expect_no_error({
    my_device <- demo_adf(write_protected = FALSE)
    con <- adf_file_con(my_device, "foobar", writable = TRUE)
    writeBin("text", con)
    writeBin(1i, con)
    writeBin(1i, con, endian = "swap")
    writeBin(1L, con)
    writeBin(1L, con, endian = "swap")
    writeBin(1, con)
    writeBin(1, con, endian = "swap")
    writeBin(TRUE, con)
    writeBin(raw(1), con)
    close(con)
    close(my_device)
  })
})

test_that("We can format and print the virtual disk representation", {
  expect_no_error({
    my_device <- demo_adf()
    md <- format(my_device)
    print(my_device)
    close(my_device)
  })
})

test_that("We can format and print block", {
  expect_no_error({
    block <- new_adf_block()
    bl <- format(block)
    print(block)
  })
})

test_that("We can format and print a virtual path", {
  expect_no_error({
    my_device <- demo_adf()
    vp <- virtual_path(my_device, "s")
    vpf <- format(vp)
    vpf <- format(c(vp, vp))
    print(vp)
    close(my_device)
  })
})

sink()