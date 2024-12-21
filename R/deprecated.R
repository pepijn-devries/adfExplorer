#' Deprecated functions
#' 
#' Functions documented here are deprecated and will be removed in future
#' versions of the package. Please use the new functions as indicated instead
#' or revert to old releases when no alternative is available. See also
#' `vignette("version2")` for more information.
#' @param ... Ignored.
#' @param value Ignored.
#' @returns `NULL`
#' @rdname deprecated
#' @export
amigaDateToRaw <- function(...) {
  .Deprecated()
  NULL
}

#' @rdname deprecated
#' @export
amigaIntToRaw <- function(...) {
  .Deprecated()
  NULL
}

#' @rdname deprecated
#' @export
bitmapToRaw <- function(...) {
  .Deprecated()
  NULL
}

#' @rdname deprecated
#' @export
displayRawData <- function(...) {
  .Deprecated()
  NULL
}

#' @rdname deprecated
#' @export
rawToAmigaDate <- function(...) {
  .Deprecated()
  NULL
}

#' @rdname deprecated
#' @export
rawToAmigaInt <- function(...) {
  .Deprecated()
  NULL
}

#' @rdname deprecated
#' @export
rawToBitmap <- function(...) {
  .Deprecated()
  NULL
}

#' @rdname deprecated
#' @export
`adf.disk.name<-` <- function(..., value) {
  .Deprecated("volume_name<-")
  NULL
}

#' @rdname deprecated
#' @export
`adf.file.mode<-` <- function(..., value) {
  .Deprecated()
  NULL
}

#' @rdname deprecated
#' @export
`adf.file.time<-` <- function(..., value) {
  .Deprecated()
  NULL
}

#' @rdname deprecated
#' @export
`amigaBlock<-` <- function(..., value) {
  .Deprecated("write_adf_block")
  NULL
}

#' @rdname deprecated
#' @export
`current.adf.dir<-` <- function(..., value) {
  .Deprecated("adf_directory<-")
  NULL
}

#' @rdname deprecated
#' @export
adf.disk.name <- function(...) {
  .Deprecated("volume_name")
  NULL
}

#' @rdname deprecated
#' @export
adf.file.exists <- function(...) {
  .Deprecated("adf_file_exists")
  NULL
}

#' @rdname deprecated
#' @export
adf.file.info <- function(...) {
  .Deprecated("adf_entry_info")
  NULL
}

#' @rdname deprecated
#' @export
adf.file.mode <- function(...) {
  .Deprecated()
  NULL
}

#' @rdname deprecated
#' @export
adf.file.remove <- function(...) {
  .Deprecated("remove_adf_entry")
  NULL
}

#' @rdname deprecated
#' @export
adf.file.size <- function(...) {
  .Deprecated()
  NULL
}

#' @rdname deprecated
#' @export
adf.file.time <- function(...) {
  .Deprecated()
  NULL
}

#' @rdname deprecated
#' @export
amigaBlock <- function(...) {
  .Deprecated("read_adf_block")
  NULL
}

#' @rdname deprecated
#' @export
blank.amigaDOSDisk <- function(...) {
  .Deprecated("create_adf_device")
  NULL
}

#' @rdname deprecated
#' @export
current.adf.dir <- function(...) {
  .Deprecated("adf_directory")
  NULL
}

#' @rdname deprecated
#' @export
dir.create.adf <- function(...) {
  .Deprecated("adf_mkdir")
  NULL
}

#' @rdname deprecated
#' @export
dir.exists.adf <- function(...) {
  .Deprecated("adf_dir_exists")
  NULL
}

#' @rdname deprecated
#' @export
get.blockID <- function(...) {
  .Deprecated()
  NULL
}

#' @rdname deprecated
#' @export
get.diskLocation <- function(...) {
  .Deprecated()
  NULL
}

#' @rdname deprecated
#' @export
is.amigaDOS <- function(...) {
  .Deprecated()
  NULL
}

#' @rdname deprecated
#' @export
is.bootable <- function(...) {
  .Deprecated("is_bootable")
  NULL
}

#' @rdname deprecated
#' @export
list.adf.files <- function(...) {
  .Deprecated("list_adf_entries")
  NULL
}

#' @rdname deprecated
#' @export
put.adf.file <- function(...) {
  .Deprecated("move_adf_entry")
  NULL
}

#' @rdname deprecated
#' @export
read.adf <- function(...) {
  .Deprecated("connect_adf")
  NULL
}

#' @rdname deprecated
#' @export
read.adz <- function(...) {
  .Deprecated("connect_adf")
  NULL
}


#' @rdname deprecated
#' @export
write.adf <- function(...) {
  .Deprecated("connect_adf")
  NULL
}

#' @rdname deprecated
#' @export
write.adz <- function(...) {
  .Deprecated("connect_adf")
  NULL
}
