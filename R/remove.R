#' Remove entry (file / directory) from a virtual ADF device
#' 
#' This function removes an entry (file or directory) from a virtual ADF device. At the moment
#' this function only removes a single entry per call, and in case the entry is a directory,
#' the directory needs to be empty before it can be removed.
#' @param x The virtual ADF device from which an entry needs to be deleted or a virtual path pointing
#' at the entry to be deleted. In case of a virtual device, it should be of class `adf_device` which
#' can be created with [`create_adf_device()`] or [`connect_adf()`]. In case of a virtual path use
#' [`virtual_path()`].
#' @param path A `character` string or a virtual_path (see [`virtual_path()`])
#' representing a file or directory you wish to delete. Should be omitted when `x` is already a
#' virtual path.
#' @param flush A `logical` value. When set to `FALSE` (default), only the entry's registry in its
#' parent directory is removed and its flags in the bitmap block are set to 'available'. The entry's
#' header data and if the entry is a file, the file data will still linger on the virtual disk.
#' If you don't want that, set this argument to `TRUE`, in that case all file or directory data will
#' be purged. Note that in the latter case, it won't be possible to recover your deleted file or directory.
#' @param ... Ignored
#' @returns Returns the device connection
#' @examples
#' # Open a connection to a virtual device:
#' my_device <- demo_adf(write_protected = FALSE)
#' 
#' ## List files in directory 'Devs':
#' list_adf_entries(my_device, "Devs")
#' 
#' ## remove the file 'system-configuration' from the virtual device
#' remove_adf_entry(my_device, "devs/system-configuration")
#' 
#' ## List files in directory 'Devs' again:
#' list_adf_entries(my_device, "Devs")
#' 
#' ## close the connection to the virtual device
#' close(my_device)
#' @rdname remove_adf_entry
#' @name remove_adf_entry
#' @author Pepijn de Vries
#' @export
remove_adf_entry <- function(x, path, flush = FALSE, ...) {
  UseMethod("remove_adf_entry", x)
}

#' @rdname remove_adf_entry
#' @method remove_adf_entry adf_device
#' @export remove_adf_entry.adf_device
#' @export
remove_adf_entry.adf_device <- function(x, path, flush = FALSE, ...) {
  if (missing(path)) {
    remove_adf_entry(x, adf_directory(x), flush = flush, ...)
  } else UseMethod("remove_adf_entry.adf_device", path)
}

#' @rdname remove_adf_entry
#' @method remove_adf_entry virtual_path
#' @export remove_adf_entry.virtual_path
#' @export
remove_adf_entry.virtual_path <- function(x, path, flush = FALSE, ...) {
  if (!missing(path))
    stop("Argument `path` should be omitted when `x` is already a virtual path")
  x <- unclass(x)
  remove_adf_entry(x$device[[1]], x$path)
}

#' @rdname remove_adf_entry
#' @name remove_adf_entry
#' @method remove_adf_entry.adf_device character
#' @export
remove_adf_entry.adf_device.character <- function(x, path, flush = FALSE, ...) {
  adf_remove_entry(x, path, flush)
  return(x)
}

#' @rdname remove_adf_entry
#' @name remove_adf_entry
#' @method remove_adf_entry.adf_device virtual_path
#' @export
remove_adf_entry.adf_device.virtual_path <- function(x, path, flush = FALSE, ...) {
  if (length(path) > 1)
    stop("Cannot remove entries from multiple virtual paths")
  .check_dev(x, path)
  path <- unclass(path[[1]])
  remove_adf_entry(x, path$path, flush, ...)
}
