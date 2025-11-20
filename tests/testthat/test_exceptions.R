test_that("Blocks cannot have sizes other than 512", {
  expect_error({
    as_adf_block(raw(1))
  })
})

test_that("A writable connection to an ADZ file will produce a warning", {
  expect_warning({
    my_device <- system.file("example.adz", package = "adfExplorer") |>
      connect_adf(write_protected = FALSE)
    close(my_device)
  })
})

test_that("A file connection cannot be opened to multiple virtual paths", {
  on.exit(close_all_devices(), add = TRUE, after = FALSE)
  expect_error({
    my_device <- demo_adf()
    vp <- virtual_path(my_device, c("s/startup-sequence", "devs/system-configuration"))
    con <- adf_file_con(vp)
  })
})

test_that("Creating a device without connecting returns NULL", {
  expect_null({
    dev_file <- tempfile(fileext = ".adf")
    create_adf_device(dev_file, connect = FALSE)
  })
})

test_that("Setting volume name to NA fails", {
  on.exit(close_all_devices(), add = TRUE, after = FALSE)
  expect_error({
    my_device <- demo_adf(write_protected = FALSE)
    volume_name(my_device) <- NA_character_
  })
})

test_that("Cannot make multiple directories at once 1", {
  on.exit(close_all_devices(), add = TRUE, after = FALSE)
  expect_error({
    my_device <- demo_adf(write_protected = FALSE)
    vp <- virtual_path(my_device, c("a", "b"))
    make_adf_dir(my_device, vp)
  })
})

test_that("Cannot change directory when path points to a file", {
  on.exit(close_all_devices(), add = TRUE, after = FALSE)
  expect_error({
    my_device <- demo_adf(write_protected = FALSE)
    adf_directory(my_device) <- "mods/mod.intro"
  })
})

test_that("Cannot make multiple directories at once 2", {
  on.exit(close_all_devices(), add = TRUE, after = FALSE)
  expect_error({
    my_device <- demo_adf(write_protected = FALSE)
    vp <- virtual_path(my_device, c("a", "b"))
    make_adf_dir(vp)
  })
})

test_that("Path argument should be missing when calling `make_adf_dir` with virtual path", {
  on.exit(close_all_devices(), add = TRUE, after = FALSE)
  expect_error({
    my_device <- demo_adf(write_protected = FALSE)
    make_adf_dir(virtual_path(my_device, c("a")), "a")
  })
})

test_that("Path argument should be missing when calling `list_adf_entries` with virtual path", {
  on.exit(close_all_devices(), add = TRUE, after = FALSE)
  expect_error({
    my_device <- demo_adf()
    list_adf_entries(virtual_path(my_device, c("a")), "a")
  })
})

test_that("Cannot `list_adf_entries` for multiple directories 1", {
  on.exit(close_all_devices(), add = TRUE, after = FALSE)
  expect_error({
    my_device <- demo_adf()
    list_adf_entries(my_device, c("s", "devs"))
  })
})

test_that("Cannot `list_adf_entries` for multiple directories 2", {
  on.exit(close_all_devices(), add = TRUE, after = FALSE)
  expect_error({
    my_device <- demo_adf()
    list_adf_entries(virtual_path(my_device, c("s", "devs")))
  })
})

test_that("Cannot `list_adf_entries` for multiple directories 3", {
  on.exit(close_all_devices(), add = TRUE, after = FALSE)
  expect_error({
    my_device <- demo_adf()
    list_adf_entries(my_device, virtual_path(my_device, c("s", "devs")))
  })
})

test_that("Path argument should be missing when calling `adf_file_exists` with virtual path", {
  on.exit(close_all_devices(), add = TRUE, after = FALSE)
  expect_error({
    my_device <- demo_adf()
    adf_file_exists(virtual_path(my_device, c("a")), "a")
  })
})

test_that("Path argument should be missing when calling `adf_dir_exists` with virtual path", {
  on.exit(close_all_devices(), add = TRUE, after = FALSE)
  expect_error({
    my_device <- demo_adf()
    adf_dir_exists(virtual_path(my_device, c("a")), "a")
  })
})

test_that("Path argument should be missing when calling `adf_entry_info` with virtual path", {
  on.exit(close_all_devices(), add = TRUE, after = FALSE)
  expect_error({
    my_device <- demo_adf()
    adf_entry_info(virtual_path(my_device, c("s")), "s")
  })
})

test_that("`adf_entry_info` does not work for non-existing path", {
  on.exit(close_all_devices(), add = TRUE, after = FALSE)
  expect_error({
    my_device <- demo_adf()
    adf_entry_info(my_device, "df0:foobar")
  })
})

test_that("Path argument should be missing when calling `adf_entry_info` with file connection", {
  on.exit(close_all_devices(), add = TRUE, after = FALSE)
  expect_error({
    my_device <- demo_adf()
    con <- adf_file_con(virtual_path(my_device, "s/startup-sequence"))
    adf_entry_info(con, "s")
  })
})

test_that("Path argument should be missing when calling `adf_entry_name<-ac` with file connection", {
  on.exit(close_all_devices(), add = TRUE, after = FALSE)
  expect_error({
    my_device <- demo_adf(write_protected = FALSE)
    con <- adf_file_con(my_device, "s/startup-sequence")
    adf_entry_name(con, "a") <- "a"
  })
})

test_that("Path argument should be missing when calling `adf_entry_name<-vp` with file connection", {
  on.exit(close_all_devices(), add = TRUE, after = FALSE)
  expect_error({
    my_device <- demo_adf(write_protected = FALSE)
    vp <- virtual_path(my_device, "s/startup-sequence")
    adf_entry_name(vp, "a") <- "a"
  })
})

test_that("Replacement names should have same length as virtual path size", {
  on.exit(close_all_devices(), add = TRUE, after = FALSE)
  expect_error({
    my_device <- demo_adf(write_protected = FALSE)
    vp <- virtual_path(my_device, "s/startup-sequence")
    adf_entry_name(vp) <- c("a", "b")
  })
})

test_that("Replacement names should not contain NA values", {
  on.exit(close_all_devices(), add = TRUE, after = FALSE)
  expect_error({
    my_device <- demo_adf(write_protected = FALSE)
    vp <- virtual_path(my_device, "s/startup-sequence")
    adf_entry_name(vp) <- NA_character_
  })
})

test_that("Cannot mix and match different devices", {
  on.exit(close_all_devices(), add = TRUE, after = FALSE)
  expect_error({
    my_device1 <- demo_adf(write_protected = FALSE)
    my_device2 <- demo_adf(write_protected = FALSE)
    vp <- virtual_path(my_device1, "s/startup-sequence")
    adf_entry_name(my_device2, vp) <- "a"
  })
})

test_that("Writing nothing returns NULL", {
  expect_null({
    adfExplorer:::.write_data(NULL, raw())
  })
})

test_that("Cannot overwrite existing file with move", {
  on.exit(close_all_devices(), add = TRUE, after = FALSE)
  expect_error({
    my_device <- demo_adf(write_protected = FALSE)
    adf_entry_name(my_device, "devs/system-configuration") <-
      "startup-sequence"
    move_adf_entry(virtual_path(my_device, "devs/startup-sequence"),
                   virtual_path(my_device, "s"))
  })
})

test_that("Path argument should be missing when calling `remove_adf_entry` with virtual path", {
  on.exit(close_all_devices(), add = TRUE, after = FALSE)
  expect_error({
    my_device <- demo_adf(write_protected = FALSE)
    remove_adf_entry(virtual_path(my_device, c("a")), "a")
  })
})

test_that("Invalid endianess throws error for readBin", {
  on.exit(close_all_devices(), add = TRUE, after = FALSE)
  expect_error({
    my_device <- demo_adf()
    con <- adf_file_con(my_device, "devs/system-configuration")
    foo <- readBin(con, "integer", endian = "bar")
  })
})

test_that("Invalid endianess throws error for writeBin", {
  on.exit(close_all_devices(), add = TRUE, after = FALSE)
  expect_error({
    my_device <- demo_adf(write_protected = FALSE)
    con <- adf_file_con(my_device, "foobar", writable = TRUE)
    foo <- writeBin(1L, con, endian = "bar")
  })
})

test_that("Non-vector objects in call throws error for writeBin", {
  on.exit(close_all_devices(), add = TRUE, after = FALSE)
  expect_error({
    my_device <- demo_adf(write_protected = FALSE)
    con <- adf_file_con(my_device, "foobar", writable = TRUE)
    writeBin(NULL, con)
  })
})

test_that("`writeLines` cannot be called on anything other than characters", {
  on.exit(close_all_devices(), add = TRUE, after = FALSE)
  expect_error({
    my_device <- demo_adf(write_protected = FALSE)
    con <- adf_file_con(my_device, "foobar", writable = TRUE)
    writeLines(1L, con)
  })
})

test_that("`dev` in `virtual_path` should be of class `adf_device`", {
  expect_error({
    virtual_path(1L, "DF0:")
  })
})

test_that("`path` in `virtual_path` should be of type `character`", {
  on.exit(close_all_devices(), add = TRUE, after = FALSE)
  expect_error({
    my_device <- demo_adf()
    virtual_path(my_device, 1L)
  })
})

test_that("`path` in `virtual_path` should not be `NA`", {
  on.exit(close_all_devices(), add = TRUE, after = FALSE)
  expect_error({
    my_device <- demo_adf()
    virtual_path(my_device, NA_character_)
  })
})

test_that("`list_adf_entries` throws an error when path doesn't exist", {
  on.exit(close_all_devices(), add = TRUE, after = FALSE)
  expect_error({
    my_device <- demo_adf()
    list_adf_entries(my_device, "q")
  })
})

test_that("`list_adf_entries` throws an error when path doesn't exist", {
  on.exit(close_all_devices(), add = TRUE, after = FALSE)
  expect_error({
    my_device <- demo_adf(write_protected = FALSE)
    make_adf_dir(my_device, "s/startup-sequence")
  })
})

test_that("Cannot move the device's root to another directory", {
  on.exit(close_all_devices(), add = TRUE, after = FALSE)
  expect_error({
    my_device <- demo_adf(write_protected = FALSE)
    move_adf_entry(virtual_path(my_device, "DF0:"),
                   virtual_path(my_device, "s"))
  })
})

test_that("Cannot move an entry to a file", {
  on.exit(close_all_devices(), add = TRUE, after = FALSE)
  expect_error({
    my_device <- demo_adf(write_protected = FALSE)
    move_adf_entry(virtual_path(my_device, "s/startup-sequence"),
                   virtual_path(my_device, "devs/system-configuration"))
  })
})

test_that("Cannot move an entry to a location where it already exists", {
  on.exit(close_all_devices(), add = TRUE, after = FALSE)
  expect_error({
    my_device <- demo_adf(write_protected = FALSE)
    make_adf_dir(my_device, "devs/s")
    move_adf_entry(virtual_path(my_device, "s"),
                   virtual_path(my_device, "devs"))
  })
})
