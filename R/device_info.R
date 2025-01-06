#' Obtain information about an `adf_device` connection
#' 
#' A collection of functions to retrieve information about the virtual device, or
#' any volume (file system) available on the device. See examples for usage and
#' results.
#' 
#' @param dev The virtual adf device for which information needs to be obtained.
#' It should be of class `adf_device` which can be created with [`create_adf_device()`]
#' or [`connect_adf()`].
#' @param vol Volume index number on the device starting at `0`. Default is `0`.
#' Note that floppy disks can only have 1 volume installed.
#' @param ... Ignored
#' @param value Replacement value. In case of `volume_name()` it can be used to
#' assign a new name to the volume.
#' @returns Returns the requested information, or an updated copy of `dev` in case
#' of an assign operation (`<-`).
#' @rdname device_info
#' @examples
#' ## Open virtual device to demonstrate methods
#' my_device <- demo_adf(write_protected = FALSE)
#' 
#' device_type(my_device)
#' 
#' device_capacity(my_device) # in bytes
#' 
#' volume_capacity(my_device) # in bytes
#' 
#' n_volumes(my_device) # number of volumes available on device
#' 
#' volume_name(my_device) # name of the volume
#' 
#' volume_name(my_device) <- "new_name" # rename the volume
#' 
#' bytes_free(my_device) # bytes available for writing
#' 
#' is_bootable(my_device) # tests if device is potentially bootable
#' 
#' is_fast_file_system(my_device) # tests if volume uses FFS
#' 
#' is_international(my_device) # tests if file system uses intl mode
#' 
#' is_dircache(my_device) # tests if file system uses dir caching
#' 
#' is_write_protected(my_device) # tests if device is protected against writing
#' 
#' close(my_device)
#' @author Pepijn de Vries
#' @include move.R
#' @export
device_type <- function(dev, ...) {
  UseMethod("device_type", dev)
}

#' @rdname device_info
#' @name device_type
#' @export
device_type.adf_device <- function(dev, ...) {
  adf_dev_type(dev)
}

#' @rdname device_info
#' @export
device_capacity <- function(dev, ...) {
  UseMethod("device_capacity", dev)
}

#' @rdname device_info
#' @name device_capacity
#' @export
device_capacity.adf_device <- function(dev, ...) {
  adf_dev_size(dev)
}

#' @rdname device_info
#' @export
volume_capacity <- function(dev, ...) {
  UseMethod("volume_capacity", dev)
}

#' @rdname device_info
#' @name volume_capacity
#' @export
volume_capacity.adf_device <- function(dev, vol = 0L, ...) {
  adf_vol_size(dev, vol)
}

#' @rdname device_info
#' @export
volume_name <- function(dev, ...) {
  UseMethod("volume_name", dev)
}

#' @rdname device_info
#' @export
`volume_name<-` <- function(dev, ..., value) {
  UseMethod("volume_name<-", dev)
}

#' @rdname device_info
#' @name volume_name
#' @export
volume_name.adf_device <- function(dev, vol = 0L, ...) {
  adf_dev_name(dev, vol)
}

#' @rdname device_info
#' @name volume_name<-
#' @export
`volume_name<-.adf_device` <- function(dev, vol = 0L, ..., value) {
  if (length(value) != 1 || any(is.na(value)))
    stop("New name should have one element and not be `NA`")
  adf_set_dev_name(dev, vol, .sanitise_name_nonamiga2(value))
  return(dev)
}

#' @rdname device_info
#' @export
n_volumes <- function(dev, ...) {
  UseMethod("n_volumes", dev)
}

#' @rdname device_info
#' @name n_volumes
#' @export
n_volumes.adf_device <- function(dev, ...) {
  adf_dev_nvol(dev)
}

#' @rdname device_info
#' @export
bytes_free <- function(dev, ...) {
  UseMethod("bytes_free", dev)
}

#' @rdname device_info
#' @name bytes_free
#' @export
bytes_free.adf_device <- function(dev, vol = 0L, ...) {
  adf_free_blocks(dev, vol) * adf_block_size(dev, vol)
}

#' @rdname device_info
#' @export
is_bootable <- function(dev, ...) {
  UseMethod("is_bootable", dev)
}

#' @rdname device_info
#' @name is_bootable
#' @export
is_bootable.adf_device <- function(dev, vol = 0L, ...) {
  adf_is_bootable(dev, vol)
}

#' @rdname device_info
#' @export
is_fast_file_system <- function(dev, ...) {
  UseMethod("is_fast_file_system", dev)
}

#' @rdname device_info
#' @name is_fast_file_system
#' @export
is_fast_file_system.adf_device <- function(dev, vol = 0L, ...) {
  adf_is_ffs(dev, vol)
}

#' @rdname device_info
#' @export
is_international <- function(dev, ...) {
  UseMethod("is_international", dev)
}

#' @rdname device_info
#' @name is_international
#' @export
is_international.adf_device <- function(dev, vol = 0L, ...) {
  adf_is_intl(dev, vol)
}

#' @rdname device_info
#' @export
is_dircache <- function(dev, ...) {
  UseMethod("is_dircache", dev)
}

#' @rdname device_info
#' @name is_dircache
#' @export
is_dircache.adf_device <- function(dev, vol = 0L, ...) {
  adf_is_dircache(dev, vol)
}

#' @rdname device_info
#' @export
is_write_protected <- function(dev, ...) {
  UseMethod("is_write_protected", dev)
}

#' @rdname device_info
#' @name is_write_protected
#' @export
is_write_protected.adf_device <- function(dev, ...) {
  adf_is_write_protected(dev)
}
