#' Test if an entry exists on a virtual device
#' 
#' Test if an entry (file or directory) exists on a virtual ADF device.
#' `adf_file_exists()` is the equivalent of [`file.exists()`] on a virtual ADF device.
#' `adf_dir_exists()` is the equivalent of [`dir.exists()`] on a virtual ADF device.
#' @param x Either a virtual device or virtual path.
#' @param path A [`virtual_path()`] pointing to the targeted entry (file or directory). Should
#' be omitted when `x` is already a virtual path.
#' @param ... Ignored
#' @returns `adf_file_exists()` returns `TRUE` if the path exists on the virtual device, `FALSE` otherwise.
#' `adf_dir_exists()` returns `TRUE` when the path exists and is a directory, `FALSE` otherwise.
#' @examples
#' ## First setup a connection to a virtual device
#' my_device <- demo_adf()
#' 
#' adf_file_exists(my_device, "s/startup-sequence")
#' adf_dir_exists(my_device, "s/startup-sequence")
#' 
#' close(my_device)
#' @author Pepijn de Vries
#' @rdname exists
#' @export
adf_file_exists <- function(x, path, ...) {
  UseMethod("adf_file_exists", x)
}

#' @export
#' @rdname exists
adf_file_exists.adf_device <- function(x, path,...) {
  if (typeof(path) != "character") {
    .check_dev(x, path)
    path <- unclass(path)$path
  }
  lapply(path, \(y) adf_file_exists_(extptr = x, path = y)) |> unlist()
}

#' @export
#' @rdname exists
adf_file_exists.virtual_path <- function(x, path,...) {
  if (!missing(path))
    stop("'path' argument should be omitted when 'x' is already a 'virtual_path'.")
  if (length(x) > 1) lapply(x, adf_file_exists) |> unlist() else {
    x <- unclass(x)
    adf_file_exists_(x$device[[1]], x$path)
  }
}

#' @rdname exists
#' @export
adf_dir_exists <- function(x, path, ...) {
  UseMethod("adf_dir_exists", x)
}

#' @rdname exists
#' @export
adf_dir_exists.adf_device <- function(x, path,...) {
  if (length(path) > 1) {
    lapply(path, adf_dir_exists, x = x) |> unlist()
  } else {
    if (typeof(path) != "character") {
      .check_dev(x, path)
      path <- unclass(path)$path
    }
    adf_dir_exists_(x, path)
  }
}

#' @rdname exists
#' @export
adf_dir_exists.virtual_path <- function(x, path,...) {
  if (!missing(path))
    stop("'path' argument should be omitted when 'x' is already a 'virtual_path'.")
  if (length(x) > 1) {
    lapply(x, adf_dir_exists) |> unlist()
  } else {
    x <- unclass(x)
    adf_dir_exists_(x$device[[1]], x$path)
  }
}

#' Retrieve information from entry headers on virtual ADF devices
#' 
#' Retrieve information from entry (file and directory) headers on virtual ADF devices.
#' Get information like entry name, modification date, file size etc.
#' @inheritParams adf_file_exists
#' @returns Returns a `list` of named `list`s of entry properties.
#' Elements included in the named `list`
#' depend on the type of entry (root, directory or file).
#' @examples
#' ## First setup a connection to a virtual device
#' my_device <- demo_adf()
#' 
#' adf_entry_info(my_device, "DF0:")
#' adf_entry_info(my_device, "s")
#' adf_entry_info(my_device, "s/startup-sequence")
#' 
#' close(my_device)
#' @author Pepijn de Vries
#' @rdname entry_info
#' @export
adf_entry_info <- function(x, path, ...) {
  UseMethod("adf_entry_info", x)
}

#' @rdname entry_info
#' @method adf_entry_info adf_device
#' @export adf_entry_info.adf_device
#' @export
adf_entry_info.adf_device <- function(x, path, ...) {
  UseMethod("adf_entry_info.adf_device", path)
}

#' @rdname entry_info
#' @name adf_entry_info
#' @method adf_entry_info.adf_device virtual_path
#' @export
adf_entry_info.adf_device.virtual_path <- function(x, path, ...) {
  if (length(path) > 1) {
    lapply(path, \(y) adf_entry_info(x, y, ...)[[1]])
  } else {
    .check_dev(x, path)
    path <- unclass(path)$path
    adf_entry_info(x, path)
  }
}

#' @rdname entry_info
#' @name adf_entry_info
#' @method adf_entry_info.adf_device character
#' @export
adf_entry_info.adf_device.character <- function(x, path, ...) {
  if (length(path) > 1) {
    lapply(path, \(y) adf_entry_info_(x, y))
  } else {
    list(adf_entry_info_(x, path))
  }
}

#' @rdname entry_info
#' @export adf_entry_info.virtual_path
#' @export
adf_entry_info.virtual_path <- function(x, path, ...) {
  if (!missing(path)) stop("`path` only needs to be specified when `x` is an `adf_device`.")
  if (length(x) > 1) lapply(x, \(y) adf_entry_info(y, ...)[[1]]) else {
    x <- unclass(x)
    adf_entry_info(x$device[[1]], x$path)
  }
}

#' @rdname entry_info
#' @export adf_entry_info.adf_file_con
#' @export
adf_entry_info.adf_file_con <- function(x, path, ...) {
  if (!missing(path)) stop("`path` only needs to be specified when `x` is an `adf_device`.")
  list(adf_entry_info_(x, ""))
}

#' Obtain or modify an entry name on a virtual device
#' 
#' Get the name of an entry (root, file or directory) or update it with the
#' assign operator (`<-`).
#' @inheritParams adf_file_exists
#' @param value New name for the entry. The name will be sanitised and truncated before
#' it is assigned to the entry.
#' @returns Returns the entry name of the requested path or in case of an assign
#' operation (`<-`) an updated version of `x`.
#' @examples
#' ## Open virtual device to demonstrate methods
#' my_device <- demo_adf(write_protected = FALSE)
#' 
#' ## rename a specific entry
#' adf_entry_name(my_device, "DF0:mods/mod.intro") <- "mod.music"
#' 
#' ## rename disk (also possible with `volume_name<-()`)
#' adf_entry_name(my_device, "DF0:") <- "my_disk"
#' 
#' close(my_device)
#' @author Pepijn de Vries
#' @rdname name
#' @export
`adf_entry_name<-` <- function(x, path, ..., value) {
  UseMethod("adf_entry_name<-", x)
}

#' @rdname name
#' @export
adf_entry_name <- function(x, path, ...) {
  x <- adf_entry_info(x, path, ...)
  x |> lapply(\(y) c(y[["diskName"]], y[["dirname"]], y[["filename"]])) |> unlist()
}

#' @rdname name
#' @method adf_entry_name<- adf_file_con
#' @export adf_entry_name<-.adf_file_con
#' @export
`adf_entry_name<-.adf_file_con` <- function(x, path, ..., value) {
  if (!missing(path))
    stop("`path` argument should be missing when `x` is of class `adf_file_con`.")
  adf_set_entry_name_(x, "", .sanitise_name_nonamiga2(value))
}

#' @rdname name
#' @method adf_entry_name<- adf_device
#' @export adf_entry_name<-.adf_device
#' @export
`adf_entry_name<-.adf_device` <- function(x, path, ..., value) {
  UseMethod("adf_entry_name<-.adf_device", path)
}

#' @rdname name
#' @export
`adf_entry_name<-.virtual_path` <- function(x, path, ..., value) {
  if (!missing(path))
    stop("`path` argument should be missing when `x` is of class `virtual_path`.")
  if (length(x) != length(value)) stop("replacement should have the same length as `x`")
  if (any(is.na(value)) || !is.character(value))
    stop("Replacement should be a `character` with no missing values")
  lapply(seq_along(x), \(i) {
    y <- unclass(x[i])
    adf_entry_name(y$device[[1]], y$path, ...) <- value[i]
  }) |> invisible()
  x
}

#' @rdname name
#' @export
`adf_entry_name<-.adf_device.character` <- function(x, path, ..., value) {
  if (length(path) > 1) {
    path <- virtual_path(x, path)
    adf_entry_name(path) <- value
    x
  } else
    adf_set_entry_name_(x, path, .sanitise_name_nonamiga2(value))
}

#' @rdname name
#' @export
`adf_entry_name<-.adf_device.virtual_path` <- function(x, path, ..., value) {
  .check_dev(x, path)
  path <- unclass(path)$path
  adf_entry_name(x, path) <- value
  x
}
