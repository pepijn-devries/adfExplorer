#' Create a connection to a virtual disk
#' 
#' Establish a connection to a virtual disk stored as Amiga Disk Files (ADF).
#' You cannot write or read directly from this connection. Instead, use the
#' methods provided in this package to retrieve information about the virtual
#' disk or create connections to the files on the disk, to which you _can_
#' write and read from (see [`adf_file_con()`]). Like any other connection,
#' please use [`close()`] to close the connection after use.
#' @include cpp11.R
#' @param filename Filename of the `ADF` or `ADZ` file containing the virtual disk
#' @param write_protected A `logical` value indicating whether the
#' virtual disk needs to be write protected. If `TRUE`, you can only open
#' 'read only' connections and cannot write to the disk.
#' @returns Returns an R connection of class `adf_device`.
#' @examples
#' adz_file <- system.file("example.adz", package = "adfExplorer")
#' my_device <- connect_adf(adz_file)
#' 
#' device_capacity(my_device)
#' close(my_device)
#' @author Pepijn de Vries
#' @export
connect_adf <- function(filename, write_protected = TRUE) {
  if (toupper(filename) |> endsWith(".ADZ")) {
    if (!write_protected) warning("ADZ files can only be opened in write-protected mode.")
    write_protected <- TRUE
    
    ## Get file size of gzip'd adf:
    con <- file(filename, "rb", raw = TRUE)
    tryCatch({
      seek(con, -4L, "end")
      gz_size <- readBin(con, "integer", 1)
    }, finally = {
      close(con)
    })
    
    ## decompress gzip file to temp file
    f <- tempfile(fileext = ".adf")
    con_in  <- gzfile(filename, "rb")
    tryCatch({
      con_out <- file(f, "wb")
    }, error = function(e) {
      close(con_in)
      stop(e$message)
    })
    tryCatch({
      readBin(con_in, "raw", n = gz_size) |>
        writeBin(con_out)
    }, finally = {
      close(con_in)
      close(con_out)
    })
    filename <- f
    
    ## open the temp file
  }
  open_adf_(filename, write_protected)
}

#' Open a connection to a file on a virtual ADF device
#' 
#' Open a connection to a file on a virtual ADF device. The created connection (if valid)
#' should be accepted by any R function that reads from or writes to a connection,
#' such as [`readLines()`], [`writeLines()`], [`readBin()`], [`writeBin()`], etc.
#' @include cpp11.R
#' @param x Either a connection to a virtual ADF device created with [`connect_adf()`],
#' or a `virtual_path` created with [`virtual_path()`].
#' @param path Only required when `x` is a virtual device of class `adf_device`.
#' In that case `path` should be a character string representing the
#' path to the file on the virtual device. See also `vignette("virtual_paths")`.
#' @param writable A `logical` value. When `TRUE` the connection can be used to
#' write to the file on the virtual device. When `FALSE` it can only be used to read.
#' Note that a writeable connection can only be setup on a virtual device that is
#' not write protected.
#' @param ... Ignored.
#' @returns Returns an R connection that can be handled by any function that
#' accepts a connection for reading or writing. Remember to call [`close()`] after
#' use.
#' @examples
#' ## First setup a connection to a virtual device
#' adz_file <- system.file("example.adz", package = "adfExplorer")
#' my_device <- connect_adf(adz_file)
#' 
#' ## Open a connection to a file on the virtual device
#' fcon <- adf_file_con(my_device, "DF0:s/startup-sequence")
#' 
#' ## Read from the file
#' my_startup <- readLines(fcon, warn = FALSE)
#' 
#' ## Close the file
#' close(fcon)
#' 
#' ## Close the virtual device
#' close(my_device)
#' @author Pepijn de Vries
#' @rdname adf_file_con
#' @name adf_file_con
#' @export
adf_file_con <- function(x, ..., writable = FALSE) {
  UseMethod("adf_file_con", x)
}


#' @rdname adf_file_con
#' @name adf_file_con
#' @method adf_file_con adf_device
#' @export adf_file_con.adf_device
#' @export
adf_file_con.adf_device <- function(x, path, ..., writable = FALSE) {
  UseMethod("adf_file_con.adf_device", path)
}

#' @rdname adf_file_con
#' @name adf_file_con
#' @method adf_file_con.adf_device character
#' @export
adf_file_con.adf_device.character <- function(x, path, ..., writable = FALSE) {
  virtual_path(x, path) |>
    adf_file_con.virtual_path(..., writable = writable)
}

#' @rdname adf_file_con
#' @name adf_file_con
#' @export
adf_file_con.virtual_path <- function(x, ..., writable = FALSE) {
  if (length(x) != 1) stop("'virtual_path' should be of length 1.")
  x <- unclass(x)
  adf_file_con_(x$device[[1]], x$path[[1]], writable)
}

#' Close all virtual devices
#' 
#' Close all virtual devices that are currently open. This function is useful
#' when you have multiple devices opened at the same time.
#' @returns Returns `NULL` invisibly.
#' @export
close_all_devices <- function() {
  close_all_devices_()
}