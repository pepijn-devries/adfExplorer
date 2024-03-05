validity.amigaBlock <- function(object) {
  if (typeof(object@data) != "raw") return (F)
  if (length(object@data) != BLOCK_SIZE) return (F)
  return(T)
}

#' The amigaBlock class
#'
#' The Commodore Amiga stores data on floppy disks as 512 byte blocks.
#' This class reflects such a block.
#'
#' There are several types of blocks. Most important are the boot block
#' (used for booting the Amiga system), the root block (containing information
#' on the disk and the root directory), header blocks (indicating where to
#' find file data) and data blocks (containing the actual file data). See
#' this package's vignette (`vignette("amigaDiskFiles")`) for more
#' details. use the [`amigaBlock-method`] to extract a specific
#' block from an [`amigaDisk`] object.
#' 
#' @slot data The `raw` data of a 'block' of data on
#' an Amiga disk file. Each block holds 512 bytes of information.
#' This slot is therefore a `vector` of the same length.
#'
#' @name amigaBlock-class
#' @rdname amigaBlock-class
#' @aliases amigaBlock
#' @examples
#' ## create a block with no data:
#' new("amigaBlock")
#' @family blocks
#' @author Pepijn de Vries
#' @exportClass amigaBlock
setClass("amigaBlock",
         representation(data = "raw"),
         prototype(data = raw(BLOCK_SIZE)),
         validity = validity.amigaBlock)

setGeneric("amigaBlock", function(x, block) standardGeneric("amigaBlock"))
setGeneric("amigaBlock<-", function(x, block, value) standardGeneric("amigaBlock<-"))

#' Extract block from or replace a block on an amigaDisk object
#'
#' Extract an [`amigaBlock`] from an [`amigaDisk`] object,
#' or replace it on the disk.
#'
#' Information is stored in 512 byte blocks on floppy disks. This method
#' extracts a specific block at a `numeric` identifier (whole numbers
#' ranging from 0 up to 1759 (DD disk) or 3519 (HD disk)) from an
#' [`amigaDisk`] object.
#'
#' @docType methods
#' @name amigaBlock-method
#' @rdname amigaBlock-method
#' @aliases amigaBlock,amigaDisk,numeric-method
#' @param x An [`amigaDisk`] object from which the block needs to
#' be extracted or on which the block needs to be replaced.
#' @param block A `numeric` identifier (whole numbers
#' ranging from 0 up to 1759 (DD disk) or 3519 (HD disk)).
#' @param value An [`amigaBlock`] object with which the block
#' at the specified location on the disk needs to be replaced.
#' @return The [`amigaBlock`] object at the specified location
#' is returned. In case of the replace method, an [`amigaDisk`]
#' object with a replaced [`amigaBlock`] is returned.
#' @examples
#' ## get the root block from the example adf:
#' amigaBlock(adf.example, 880)
#' 
#' ## Create a completely blank disk without file system:
#' blank.disk <- new("amigaDisk")
#' 
#' ## Replace the boot block on the blank disk with
#' ## that from the example object:
#' amigaBlock(blank.disk, 0) <- amigaBlock(adf.example, 0)
#' 
#' ## The blank disk now has a boot block,
#' ## but still no file system...
#' @family block.operations
#' @author Pepijn de Vries
#' @export
setMethod("amigaBlock", c("amigaDisk", "numeric"), function(x, block){
  block <- round(block[[1]])
  if(x@type == "DD")
    NUMBER_OF_SECTORS <- NUMBER_OF_SECTORS_DD
  else
    NUMBER_OF_SECTORS <- NUMBER_OF_SECTORS_HD
  if (block < 0 || block >= NUMBER_OF_SECTORS*NUMBER_OF_SIDES*NUMBER_OF_CYLINDERS) stop("Block id is out of range (0-1759/3519)")
  start    <- 1 + block*BLOCK_SIZE
  end      <- (block + 1)*BLOCK_SIZE
  methods::new("amigaBlock", data = x@data[start:end])
})

#' @rdname amigaBlock-method
#' @name amigaBlock<-
#' @aliases amigaBlock<-,amigaDisk,numeric,amigaBlock-method
#' @export
setReplaceMethod("amigaBlock", c("amigaDisk", "numeric", "amigaBlock"), function(x, block, value){
  block <- round(block[[1]])
  if(x@type == "DD")
    NUMBER_OF_SECTORS <- NUMBER_OF_SECTORS_DD
  else
    NUMBER_OF_SECTORS <- NUMBER_OF_SECTORS_HD
  if (block < 0 || block >= NUMBER_OF_SECTORS*NUMBER_OF_SIDES*NUMBER_OF_CYLINDERS) stop("Block id is out of range (0-1759/3519)")
  start    <- 1 + block*BLOCK_SIZE
  end      <- (block + 1)*BLOCK_SIZE
  x@data[start:end] <- value@data
  methods::validObject(x)
  return(x)
})

setMethod("show", "amigaBlock", function(object){
  print(object)
})

#' @rdname print
#' @aliases print,amigaBlock-method
#' @export
setMethod("print", "amigaBlock", function(x, ...){
  displayRawData(x@data)
  return(invisible(NULL))
})

