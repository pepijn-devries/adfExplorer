#' Salvage entries on ADF disks
#' 
#' Functions to attempt to salvage files and directories from
#' virtual disks (ADF).
#' 
#' When a file or directory is removed from a virtual device (or
#' a physical device for that matter), the record is removed from
#' its parent directory hash table and its bitmap flags are flipped to
#' mark the file blocks as available. This means that the actual
#' record of the file and its contents is still on the disk, until
#' they are overwritten. You can use these functions to salvage such
#' entries.
#'
#' You can use `adf_dumster_dive()` to scan for entries that could
#' potentially be salvaged. Than you can use `salvage_adf_entry()`
#' to attempt to salvage an entry.
#' 
#' Note that these functions won't work when you have flushed the file
#' (see [remove_adf_entry()]). In that case the actual record of the file
#' and its data have been permanently removed.
#' @inheritParams device_type
#' @param sector Sector number of the file/directory header that you wish to
#' salvage
#' @returns In case of `adf_dumpster_dive()` a `data.frame` is returned
#' with records that could potentially be salvaged from the disk.
#' In case of `salvage_adf_entry()`, the `disk` (`adf_device` class object)
#' is returned, where (if successful) the record is restored.
#' @examples
#' my_device <- demo_adf(write_protected = FALSE)
#' # The demo disk contains a deleted file that could be salvaged:
#' salvageable <- adf_dumpster_dive(my_device)
#' 
#' # Let's recover it:
#' salvage_adf_entry(my_device, sector = salvageable$sect)
#' 
#' # It is now listed as an entry on the disk:
#' list_adf_entries(my_device, recursive = TRUE)
#' 
#' close(my_device)
#' @rdname salvage_adf_entry
#' @export
adf_dumpster_dive <- function(dev, vol = 0L, ...) {
  dumpster_dive(dev, vol)
}

#' @rdname salvage_adf_entry
#' @export
salvage_adf_entry <- function(dev, vol = 0L, sector, ...) {
  undelete_adf_entry(dev, vol, sector)
}