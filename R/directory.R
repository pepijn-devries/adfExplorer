#' Changing and creating directories on a virtual device
#' 
#' `adf_directory()` shows the current directory of a virtual device, when a file
#' system is present. When connecting to or creating a new device, the current
#' directory is the disk's root by default. To change the current directory,
#' use `adf_directory()` in combination with the assign operator (`<-`).
#' 
#' To create a new directory on a device use `make_adf_dir()` and use a full or
#' relative path name to specify the new directory name.
#' 
#' See `vignette("virtual_paths")` for a note on file and directory names on the Amiga.
#' @inheritParams device_capacity
#' @param x An `adf_device` or `virtual_path` class object. The first specifies
#' the device on which a directory needs to be created. The latter specifies
#' both the directory and the device on which it needs to be created.
#' @param value A `character` string or a `virtual_path` (see [`virtual_path()`])
#' representing directory you wish to set as current.
#' @param path A `character` string or a `virtual_path` (see [`virtual_path()`])
#' specifying the name of the new directory to be created. Should be missing when `x` is
#' of class `virtual_path`
#' @returns `make_adf_dir()` returns the device connection. `adf_directory()` returns
#' the current directory as a `virtual_path` class object.
#' @examples
#' ## Open virtual device to demonstrate methods
#' my_device <- demo_adf(write_protected = FALSE)
#' 
#' ## Show the current directory
#' adf_directory(my_device)
#' 
#' ## Create a new directory
#' make_adf_dir(my_device, "DF0:s/newdir")
#' 
#' ## Change the current dir to the new directory:
#' adf_directory(my_device) <- "DF0:s/newdir"
#' 
#' ## Close the virtual device
#' close(my_device)
#' @rdname adf_directory
#' @include helpers.R
#' @author Pepijn de Vries
#' @export
adf_directory <- function(dev, ...) {
  UseMethod("adf_directory", dev)
}

#' @rdname adf_directory
#' @name adf_directory
#' @export
adf_directory.adf_device <- function(dev, ...) {
  vctrs::new_rcrd(
    adf_get_current_dir(dev),
    class = "virtual_path")
}

#' @rdname adf_directory
#' @export
`adf_directory<-` <- function(dev, ..., value) {
  UseMethod("adf_directory<-", dev)
}

#' @rdname adf_directory
#' @name adf_directory<-
#' @export adf_directory<-.adf_device
#' @method adf_directory<- adf_device
#' @export
`adf_directory<-.adf_device` <- function(dev, ..., value) {
  UseMethod("adf_directory<-.adf_device", value)
}

#' @name adf_directory<-.adf_device
#' @rdname adf_directory
#' @method adf_directory<- adf_device.character
#' @export adf_directory<-.adf_device.character
#' @export
`adf_directory<-.adf_device.character` <- function(dev, ..., value) {
  adf_change_dir(dev, value)
  dev
}

#' @name adf_directory<-.adf_device
#' @rdname adf_directory
#' @method adf_directory<- adf_device.virtual_path
#' @export adf_directory<-.adf_device.virtual_path
#' @export
`adf_directory<-.adf_device.virtual_path` <- function(dev, ..., value) {
  .check_dev(dev, value)
  value <- unclass(value)
  adf_directory(dev, ...) <- value$path
  dev
}

#' @rdname adf_directory
#' @name make_adf_dir
#' @export
make_adf_dir <- function(x, path, ...) {
  UseMethod("make_adf_dir")
}

#' @rdname adf_directory
#' @name make_adf_dir
#' @method make_adf_dir adf_device
#' @export make_adf_dir.adf_device
#' @export
make_adf_dir.adf_device <- function(x, path, ...) {
  UseMethod("make_adf_dir.adf_device", path)
}

#' @rdname adf_directory
#' @name make_adf_dir
#' @method make_adf_dir virtual_path
#' @export make_adf_dir.virtual_path
#' @export
make_adf_dir.virtual_path <- function(x, path, ...) {
  if (!missing(path))
    stop("`path` argument should be missing when `x` is of class `virtual_path`.")
  if (length(x) != 1)
    stop("Cannot create multiple virtual paths at once")
  x <- unclass(x)
  make_adf_dir(x$device[[1]], x$path, ...)
}

#' @rdname adf_directory
#' @name make_adf_dir
#' @method make_adf_dir.adf_device character
#' @export
make_adf_dir.adf_device.character <- function(x, path, ...) {
  adf_mkdir(x, path)
}

#' @rdname adf_directory
#' @name make_adf_dir
#' @method make_adf_dir.adf_device virtual_path
#' @export
make_adf_dir.adf_device.virtual_path <- function(x, path, ...) {
  .check_dev(x, path)
  path <- unclass(path)
  make_adf_dir(x, path$path, ...)
}

#' List entries in a directory of a virtual ADF device
#' 
#' Get an overview of all entries (files and directories) in a specific
#' directory.
#' @param x Either an `adf_device` class object, in which case the `virtual_path`
#' argument needs to be specified; or, a `virtual_path` class object.
#' @param path The virtual path for which you wish to obtain a list
#' of entries (see also `vignette("virtual_paths")`). When missing,
#' entries for the current directory ([`adf_directory()`]) are returned, wen
#' `x` is an `adf_device` class object. If `x` is a `virtual_path` class
#' object, content of the path defined in that object is listed
#' @param recursive A `logical` value. When set to `TRUE`, the function is
#' called recursively for all subdirectories in `path`.
#' @param nested A `logical` value. When set to The directory tree is returned
#' as a nested list.
#' @param ... Ignored
#' @returns A vector of `virtual_path` class objects, or a nested `list` in
#' case `nested` is `TRUE`.
#' @examples
#' ## First setup a connection to a virtual device
#' my_device <- demo_adf()
#' 
#' ## List all entries in the disk's root:
#' list_adf_entries(my_device)
#' 
#' ## List all entries on the disk as a vector of `virtual paths`:
#' list_adf_entries(my_device, recursive = TRUE)
#' 
#' ##  List all entries on the disk as a nested list:
#' list_adf_entries(my_device, recursive = TRUE, nested = TRUE)
#'
#' close(my_device)
#' @rdname list_adf_entries
#' @name list_adf_entries
#' @author Pepijn de Vries
#' @export
list_adf_entries <- function(x, path, recursive = FALSE, nested = FALSE, ...) {
  UseMethod("list_adf_entries", x)
}

#' @rdname list_adf_entries
#' @method list_adf_entries adf_device
#' @export list_adf_entries.adf_device
#' @export
list_adf_entries.adf_device <- function(x, path, recursive = FALSE, nested = FALSE, ...) {
  if (missing(path)) {
    list_adf_entries(x, adf_directory(x), recursive = recursive, nested = nested, ...)
  } else UseMethod("list_adf_entries.adf_device", path)
}

#' @rdname list_adf_entries
#' @method list_adf_entries virtual_path
#' @export list_adf_entries.virtual_path
#' @export
list_adf_entries.virtual_path <- function(x, path, recursive = FALSE, nested = FALSE, ...) {
  if (!missing(path))
    stop("`path` argument should be missing when `x` is of class `virtual_path`.")
  if (length(x) != 1)
    stop("Cannot list directory entries for multiple virtual paths")
  x <- unclass(x)
  list_adf_entries(x$device[[1]], x$path, recursive = recursive, nested = nested, ...)
}

#' @rdname list_adf_entries
#' @name list_adf_entries
#' @method list_adf_entries.adf_device character
#' @export
list_adf_entries.adf_device.character <-
  function(x, path, recursive = FALSE, nested = FALSE, ...) {
    result <- list_adf_entries_(x, path, recursive, nested)
    if (nested) return(result)
    result <- unlist(result)
    vctrs::new_rcrd(list(device = rep(list(x), length(result)),
                         path   = result),
                    class = "virtual_path")
  }

#' @rdname list_adf_entries
#' @name list_adf_entries
#' @method list_adf_entries.adf_device virtual_path
#' @export
list_adf_entries.adf_device.virtual_path <- function(x, path,
                                                 recursive = FALSE, ...) {
  if (length(path) != 1)
    stop("Cannot list directory entries for multiple virtual paths")
  .check_dev(x, path)
  path <- unclass(path[[1]])
  list_adf_entries(x, path$path, recursive = recursive, ...)
}
