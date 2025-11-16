#' Get virtual disks bitmap flags
#' 
#' Amiga devices had  one (or more) blocks dedicated to
#' registering which blocks are used by the file system and
#' which are available to allocate. This block is called the
#' bitmap block. This function returns the bitmap table for
#' the specified volume.
#' 
#' @returns Returns the bitmap table
#' as a `logical` `vector`. Each, element is named after its
#' referencing block number, the value indicates if the block
#' is reserved by the file system.
#' @inheritParams device_type
#' @examples
#' disk <- demo_adf()
#' 
#' bitmap <- get_adf_bitmap(disk)
#' 
#' ## Show blocks used by the file system
#' bitmap[bitmap]
#' 
#' close(disk)
#' @include device_info.R
#' @export
get_adf_bitmap <- function(dev, vol = 0L, ...) {
  get_bitmap(dev, vol)
}