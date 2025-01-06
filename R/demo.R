#' Connect with a demonstration ADF file
#' 
#' Opens a connection to a virtual device for demonstration purposes
#' It is used in examples.
#' @param write_protected A `logical` value. When `TRUE` you can only read
#' from the virtual device. If `FALSE` the demonstration disk will be
#' copied to the `tempdir()` and you can write to it.
#' @returns Returns a connection to a virtual device of class `adf_device`
#' @export
demo_adf <- function(write_protected = TRUE) {
  if (write_protected) {
    system.file("example.adz", package = "adfExplorer") |>
      connect_adf(write_protected = TRUE)
  } else {
    adz_file  <- system.file("example.adz", package = "adfExplorer")
    adf_file  <- tempfile(fileext = ".adf")
    decompress_adz(adz_file, adf_file)
    connect_adf(adf_file, write_protected = FALSE)
  }
}