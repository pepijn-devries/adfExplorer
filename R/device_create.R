#' Create and format a virtual ADF device
#' 
#' These functions help you to create an empty virtual device that can be used
#' in Commodore Amiga emulation. `create_adf_device()` simply creates a file
#' of the proper size (the file size represents the device capacity) and fills it
#' with `raw` zeros. In order to use the device in the Amiga operating system, a
#' file system needs to be installed on the device. This can be achieved with
#' `prepare_adf_device()`. Note that the file system itself will also consume
#' disk space on the virtual device.
#' 
#' @param destination File path where the virtual device needs to be stored.
#' @param type Specify the type of virtual device you wish to create.
#' Should be one of `"DD"` (double density floppy disk) or `"HD"` (high
#' density floppy disk).
## #' @param size Not yet implemented
#' @param ... Passed on from `create_adf_device()` to [`connect_adf()`],
#' which is called if `connect` is set to `TRUE`.
#' @param connect A `logical` value. If set to `TRUE` a connection is opened to the
#' newly created virtual device and is returned as a `adf_device` class object. If
#' it is set to `FALSE`, the file is just created and no connection is opened. In the
#' latter case `NULL` is returned invisibly.
#' @returns Either an `adf_device` connection or `NULL` depending on the value of
#' `connect`.
#' @examples
#' ## Filepath to store the virtual device:
#' dest <- tempfile(fileext = ".adf")
#' 
#' ## Create a blank unformated virtual device (a double density floppy disk):
#' my_device <- create_adf_device(dest, "DD", connect = TRUE, write_protected = FALSE)
#' 
#' print(my_device)
#' 
#' ## Format the floppy and create a file system on the device:
#' prepare_adf_device(my_device, name = "foobar")
#' 
#' print(my_device)
#' 
#' ## don't forget to close the device connection after use:
#' close(my_device)
#' @author Pepijn de Vries
#' @rdname create_adf_device
#' @export
create_adf_device <- function(destination, type = "DD", ..., connect = TRUE) {
  # create_adf_device <- function(destination, type = "DD", size, ..., connect = TRUE) {
  # type <- match.arg(type, c("DD", "HD", "DH"))
  type <- match.arg(type, c("DD", "HD"))
  # if (missing(size)) {
  #   if (type == "DH") stop("Size is required but missing for device `DH`.")
  # } else {
  #   if (type != "DH") warning("Size is predefined for devices other than `DH` and ignored.")
  # }
  size <- switch(
    type,
    DD = 1760L*512L,
    HD = 1760L*512L*2L)
  con <- file(destination, "w+b")
  tryCatch({
    writeBin(raw(size), con)
  }, finally = {
    close(con)
  })
  if (connect) connect_adf(destination, ...) else invisible(NULL)
}

#' @inheritParams device_type
#' @param name A `character` string specifying the disk name for the volume on the
#' virtual device. It will be truncated automatically when too long.
#' @param ffs A `logical` value indicating which file system to be used. If `TRUE`
#' the 'Fast File System' (FFS) is used, when `FALSE`, the 'Old File System' is used.
#' See also `vignette("file_system_modes")`.
#' @param international A `logical` value indicating whether the international mode
#' should be used for file naming. See also `vignette("file_system_modes")`.
#' @param dircache A `logical` value indicating whether directory caching should be
#' used. See also `vignette("file_system_modes")`.
#' @param bootable A `logical` value indicating whether you want to include executable
#' code on the boot block. If set to `TRUE` minimal code will be added to the boot block.
#' In an Amiga emulator, this code will load the Amiga Disk Operating System library
#' and start the Amiga Command line interface (CLI). It will then run the startup
#' sequence file from the disk (if available).
#' 
#' If set to `FALSE` no such code is added. In that case the file system will still be
#' accessible by the Amiga operating system (if the file system mode is compatible).
#' You just can't use the disk to start up a (virtual) Amiga machine.
#' @param ... Ignored for `prepare_adf_device()`.
#' @include device_info.R
#' @rdname create_adf_device
#' @export
prepare_adf_device <- function(dev, name = "EMPTY",
                              ffs = TRUE, international = TRUE, dircache = FALSE,
                              bootable = TRUE,
                              ...) {
  UseMethod("prepare_adf_device", dev)
}

#' @rdname create_adf_device
#' @export
prepare_adf_device.adf_device <- function(dev, name = "EMPTY",
                                         ffs = TRUE, international = TRUE, dircache = FALSE,
                                         bootable = TRUE,
                                         ...) {
  adf_dev_format(dev, name, ffs, international, dircache, bootable) |> invisible()
  dev
}