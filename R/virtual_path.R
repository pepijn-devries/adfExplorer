#' A path pointing to a file or directory on a virtual ADF device
#' 
#' This function creates a path pointing to a file or directory
#' on a virtual ADF device (created with [`connect_adf()`] or [`create_adf_device()`]).
#' The virtual path created with this function can be used to establish a readable
#' or writable connection to a file, or obtain information about a file or directory.
#' See also `vignette("virtual_paths")`
#' @param dev A virtual ADF device (created with [`connect_adf()`] or
#' [`create_adf_device()`]).
#' Make sure a file system is present on the virtual device or install first when missing
#' using [`prepare_adf_device()`].
#' @param path A `character` string representing the path to a file or directory
#' on the virtual device.
#' @returns Returns a `virtual_path` class object.
#' @examples
#' # Open a connection to a virtual device:
#' my_device <- demo_adf()
#' 
#' # specify a virtual path:
#' my_path <- virtual_path(my_device, "DF0:s/startup-sequence")
#' 
#' # close the virtual device:
#' close(my_device)
#' @author Pepijn de Vries
#' @export
virtual_path <- function(dev, path) {
  if (!inherits(dev, "adf_device")) stop("`dev` should be of class `adf_device`.")
  if (typeof(path) != "character") stop("`virtual_path` should be of type `character`.")
  if (any(is.na(path))) stop("`virtual_path` cannot be `NA`")
  if (length(path) > 1) {
    do.call(c, lapply(path, \(y) virtual_path(dev, y)))
  } else {
    vctrs::new_rcrd(list(device = list(dev), path = path), class = "virtual_path")
  }
}
