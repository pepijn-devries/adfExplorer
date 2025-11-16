#' @keywords internal
"_PACKAGE"
NULL

.onUnload <- function(libpath) {
  close_all_devices()
  library.dynam.unload("adfExplorer", libpath)
}

#' @useDynLib adfExplorer, .registration = TRUE
#' @importFrom methods setOldClass
NULL

setOldClass("virtual_path")
setOldClass("adf_device")
setOldClass("adf_file_con")
setOldClass("adf_block")
