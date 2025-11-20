test_that("Current directory can be removed", {
  expect_no_error({
    my_device <- demo_adf(write_protected = FALSE)
    adf_directory(my_device) <- "s"
    remove_adf_entry(my_device, "startup-sequence")
    remove_adf_entry(my_device)
    close(my_device)
  })
})

test_that("Files can be removed using a virtual path", {
  expect_no_error({
    my_device <- demo_adf(write_protected = FALSE)
    vp <- virtual_path(my_device, "s/startup-sequence")
    remove_adf_entry(my_device, vp, flush = TRUE)
    close(my_device)
  })
})

test_that("Files can be salvaged", {
  expect_no_error({
    my_device <- demo_adf(write_protected = FALSE)
    salvageable <- adf_dumpster_dive(my_device)
    my_device <- salvage_adf_entry(
      my_device, sector =
        salvageable$sect[salvageable$name == "easter egg.txt"])
    remove_adf_entry(my_device, "mods/mod.intro")
    remove_adf_entry(my_device, "mods")
    salvageable <- adf_dumpster_dive(my_device)
    my_device <- salvage_adf_entry(
      my_device, sector =
        salvageable$sect[salvageable$name == "mods"])
    my_device <- salvage_adf_entry(
      my_device, sector =
        salvageable$sect[salvageable$name == "mod.intro"])
    close(my_device)
  })
})
