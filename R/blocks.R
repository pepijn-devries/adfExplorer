#' Read or write raw data blocks to a virtual device
#' 
#' The Amiga file system is structured around 512 byte blocks. A double density
#' floppy disk consists of 1760 blocks of 512 bytes. `read_adf_block` and `write_adf_block`
#' can be used to transform raw data from and to virtual devices (created with
#' [`create_adf_device()`] or [`connect_adf()`]). Note that writing raw data to
#' a disk could corrupt the file system on the device. So it is generally not
#' advised unless you know what you are doing.
#' @inheritParams device_type
#' @param sector Sector ID of the block you wish to read/write. It is an integer value.
#' For double density disks, the ID ranges from 0 to 1759.
#' @param data Block data (`raw` vector of length 512) you wish to write to a virtual device
#' @param ... Ignored
#' @returns In case of `write_adf_block` `NULL` is returned invisibly. In case of `read_adf_block`
#' the `raw` data is returned as a `adf_block` class object.
#' @author Pepijn de Vries
#' @rdname adf_block
#' @export
read_adf_block <- function(dev, sector, ...) {
  UseMethod("read_adf_block", dev)
}

#' @rdname adf_block
#' @export
read_adf_block.adf_device <- function(dev, sector, ...) {
  read_adf_block_(dev, as.integer(sector))
}

#' @rdname adf_block
#' @export
write_adf_block <- function(dev, sector, data, ...) {
  UseMethod("write_adf_block", dev)
}

#' @rdname adf_block
#' @method write_adf_block adf_device
#' @export
write_adf_block.adf_device <- function(dev, sector, data, ...) {
  UseMethod("write_adf_block.adf_device", data)
}

#' @rdname adf_block
#' @method write_adf_block.adf_device raw
#' @export
write_adf_block.adf_device.raw <- function(dev, sector, data, ...) {
  NextMethod()
}

#' @rdname adf_block
#' @method write_adf_block.adf_device adf_block
#' @export
write_adf_block.adf_device.adf_block <- function(dev, sector, data, ...) {
  NextMethod()
}

#' @rdname adf_block
#' @method write_adf_block.adf_device default
#' @export
write_adf_block.adf_device.default <- function(dev, sector, data, ...) {
  write_adf_block_(dev, as.integer(sector), data) |> invisible()
}

#' @rdname adf_block
#' @export
as_adf_block <- function(data, ...) {
  if (typeof(data) != "raw" && length(data) != 512)
    stop("`data` should be a raw vector with length 512")
  class(data) <- union("adf_block", class(data))
  data
}

#' @rdname adf_block
#' @export
new_adf_block <- function() {
  as_adf_block(raw(512L))
}
