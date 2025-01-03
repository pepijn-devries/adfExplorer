#' Copy or move files between physical and virtual devices
#' 
#' With these functions you can copy or move entries (files and directories) between
#' a physical and virtual ADF device. With `copy_adf_entry()` the files are duplicated,
#' with `move_adf_entry()` the files are moved (and deleted from its source).
#' @param source,destination The `source` is a path to a file or directory that needs
#' to be moved or copied. `destination` is a path to a directory to which the `source`
#' needs to be copied or moved. When `source` or `destination` is a `character` string,
#' it is assumed to be a path to a file or directory on a physical device. You
#' can use a [`virtual_path()`] for either the `source` or `destination` or both.
#' `source` and `destination` cannot both be a `character` string. For copying and moving
#' files on a physical device you should you `base` function [`file.copy()`].
#' @param ... Ignored
#' @rdname move
#' @examples
#' ## Create an Amiga Disk File
#' ## and prepare a file system on the virtual device
#' my_device <-
#'   create_adf_device(
#'     tempfile(fileext = ".adf"),
#'     write_protected = FALSE) |>
#'   prepare_adf_device()
#' 
#' ## Copy the packaged R scripts of this package to the virtual device
#' copy_adf_entry(
#'   system.file("R", package = "adfExplorer"),
#'   virtual_path(my_device, "DF0:")
#' )
#' 
#' ## List all entries on the virtual device
#' list_adf_entries(my_device, recursive = TRUE)
#' 
#' ## Move the entire virtual device content to
#' ## the tempdir on your physical device
#' dest <- file.path(tempdir(), "DF0")
#' dir.create(dest)
#' move_adf_entry(
#'   virtual_path(my_device, "DF0:"),
#'   dest
#' )
#' 
#' ## cleanup the temp directory
#' unlink(dest, recursive = TRUE)
#' 
#' close(my_device)
#' @author Pepijn de Vries
#' @export
copy_adf_entry <- function(source, destination, ...) {
  UseMethod("copy_adf_entry", source)
}

#' @rdname move
#' @method copy_adf_entry character
#' @export copy_adf_entry.character
#' @export
copy_adf_entry.character <- function(source, destination, ...) {
  UseMethod("copy_adf_entry.character", destination)
}

#' @rdname move
#' @method copy_adf_entry virtual_path
#' @export copy_adf_entry.virtual_path
#' @export
copy_adf_entry.virtual_path <- function(source, destination, ...) {
  UseMethod("copy_adf_entry.virtual_path", destination)
}

#' @rdname move
#' @name copy_adf_entry
#' @method copy_adf_entry.character virtual_path
#' @export
copy_adf_entry.character.virtual_path <- function(source, destination, ...) {
  .move_char_vp(FALSE, source, destination, ...)
}

#' @rdname move
#' @name copy_adf_entry
#' @method copy_adf_entry.virtual_path virtual_path
#' @export
copy_adf_entry.virtual_path.virtual_path <- function(source, destination, ...) {
  .move_vp_vp(FALSE, source, destination, ...)
}

#' @rdname move
#' @name copy_adf_entry
#' @method copy_adf_entry.virtual_path character
#' @export
copy_adf_entry.virtual_path.character <- function(source, destination, ...) {
  .move_vp_char(FALSE, source, destination, ...)
}

#' @rdname move
#' @export
move_adf_entry <- function(source, destination, ...) {
  UseMethod("move_adf_entry", source)
}

#' @rdname move
#' @method move_adf_entry character
#' @export move_adf_entry.character
#' @export
move_adf_entry.character <- function(source, destination, ...) {
  UseMethod("move_adf_entry.character", destination)
}

#' @rdname move
#' @method move_adf_entry virtual_path
#' @export move_adf_entry.virtual_path
#' @export
move_adf_entry.virtual_path <- function(source, destination, ...) {
  UseMethod("move_adf_entry.virtual_path", destination)
}

#' @rdname move
#' @name move_adf_entry
#' @method move_adf_entry.character virtual_path
#' @export
move_adf_entry.character.virtual_path <- function(source, destination, ...) {
  .move_char_vp(TRUE, source, destination, ...)
}

#' @rdname move
#' @name move_adf_entry
#' @method move_adf_entry.virtual_path virtual_path
#' @export
move_adf_entry.virtual_path.virtual_path <- function(source, destination, ...) {
  .move_vp_vp(TRUE, source, destination, ...)
}

#' @rdname move
#' @name move_adf_entry
#' @method move_adf_entry.virtual_path character
#' @export
move_adf_entry.virtual_path.character <- function(source, destination, ...) {
  .move_vp_char(TRUE, source, destination, ...)
}

.is_file <- function(path) {
  !dir.exists(path) && file.exists(path)
}

.read_data <- function(con) {
  data <- NULL
  repeat {
    chunk <- readBin(con, "raw", 1024)
    if (length(chunk) == 0) break else data <- c(data, chunk)
  }
  data
}

.write_data <- function(con, data) {
  if (length(data) == 0) return()
  writeBin(data, con)
}

.write_to_virtual <- function(source, destination, data) {
  destination <- unclass(destination)
  is_path <- grepl("*.?[/:]$", destination$path)
  dest_file <- .sanitise_name_amiga(basename(source))
  dest_path <- ifelse(
    is_path,
    paste0(destination$path, dest_file),
    file.path(destination$path, dest_file, fsep = "/"))
  destination <- virtual_path(destination$device[[1]], dest_path)
  if (adf_file_exists(destination))
    stop("'destination' file already exists, cannot overwrite.")
  con_out <- adf_file_con(destination, writable = TRUE) |> suppressWarnings()
  tryCatch({
    .write_data(con_out, data)
  }, finally = {
    close(con_out)
  })
}

.sanitise_name_nonamiga <- function(x) {
  x <- gsub("[:]", "/", as.character(x)) |> basename() |>
    .sanitise_name_nonamiga2()
}

.sanitise_name_nonamiga2 <- function(x) {
  gsub("[/\\:*?\"<>|/]", "", x)
}

.sanitise_name_amiga <- function(x) {
  ifelse(
    nchar(x) > 30L,
    abbreviate(x, 30L),
    x)
}

.concat_path <- function(x, y) {
  x <- paste0(
    gsub("$|/$", "/", as.character(x)),
    as.character(y) |> basename())
  gsub(":/", ":", x)
}

.move_char_vp <- function(delete, source, destination, ...) {
  if (length(source) > 1) {
    lapply(source, \(x) .move_char_vp(delete, source, destination, ...))
  } else {
    if (dir.exists(source)) {
      dest_dev <- unclass(destination)$device[[1]]
      new_dest <- virtual_path(
        dest_dev,
        .concat_path(destination, .sanitise_name_amiga(basename(source)))
      )
      if (adf_file_exists(new_dest)) stop("Cannot create directory, path already exists")
      make_adf_dir(new_dest)
      content <- list.files(source, include.dirs = TRUE, full.names = TRUE)
      for (i in seq_along(content)) {
        .move_char_vp(delete, content[i], new_dest, ...)
      }
      if (delete) unlink(source, recursive = TRUE)
    } else {
      if (!.is_file(source))
        stop("'source' path does not point to a file!")
      con_in  <- file(source, "rb")
      tryCatch({
        data    <- .read_data(con_in)
        .write_to_virtual(source, destination, data)
      }, finally = {
        close(con_in)
      })
      if (delete) unlink(source)
    }
  }
}

.move_vp_vp <- function(delete, source, destination, ...) {
  if (length(source) > 1) {
    lapply(source, \(x) {.move_vp_vp(delete, x, destination, ...)})
    return()
  }
  dev_src  <- unclass(source)$device[[1]]
  dev_dest <- unclass(destination)$device[[1]]
  if (delete && identical(dev_src, dev_dest)) {
    ## We could use a more elegant approach when source and destination are on the same device.
    ## In this case we don't have to move around data, just update the headers
    ## method below is not full implemented yet currently it only performs a check
    move_adf_internal(dev_dest, unclass(source)$path, unclass(destination)$path)
  }
  
  if (adf_dir_exists(source)) {
    new_dest <- .concat_path(destination, gsub("[:]", "/", as.character(source)) |> basename())
    new_dest <- virtual_path(dev_dest, new_dest)
    if (adf_dir_exists(new_dest)) stop("Cannot create directory, the path already exists.")
    make_adf_dir(new_dest)
    content <- list_adf_entries(source)
    for (i in seq_along(content)) {
      .move_vp_vp(delete, content[i], new_dest, ...)
    }
  } else {
    source_path <- as.character(source)
    
    con_in  <- adf_file_con(source)
    tryCatch({
      data    <- .read_data(con_in)
      .write_to_virtual(source_path, destination, data)
    }, finally = {
      close(con_in)
    })
  }
  if (delete && !endsWith(as.character(source), ":")) remove_adf_entry(source)
}

.move_vp_char <- function(delete, source, destination, ...) {
  if (.is_file(destination))
    stop("'destination' file already exists, cannot overwrite")
  if (length(source) > 1) {
    lapply(source, \(x) {.move_vp_char(delete, x, destination, ...)})
  } else {
    if (adf_dir_exists(source)) {
      new_dest <- file.path(
        destination,
        gsub("[:]", "/", as.character(source)) |>
          basename() |>
          .sanitise_name_nonamiga())
      if (file.exists(new_dest)) stop("Cannot create directory, the path already exists.")
      dir.create(new_dest)
      content <- list_adf_entries(source, recursive = FALSE)
      for (i in seq_along(content)) {
        .move_vp_char(delete, content[i], new_dest, ...)
      }
    } else {
      source_path <- as.character(source)
      
      fname <- .sanitise_name_nonamiga(source_path)
      con_in  <- adf_file_con(source)
      data    <- .read_data(con_in)
      
      destination <- file.path(destination, fname)
      if (file.exists(destination)) stop("Path already exists, cannot overwrite")
      
      con_out <- file(destination, "wb")
      tryCatch({
        .write_data(con_out, data)
      }, finally = {
        close(con_out)
      })
      close(con_in)
    }
    if (delete && !endsWith(as.character(source), ":")) remove_adf_entry(source)
  }
}
