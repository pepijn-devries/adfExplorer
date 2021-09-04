setGeneric("adf.file.remove", function(x, file, full) standardGeneric("adf.file.remove"))

#' Remove a file from an amigaDisk object
#'
#' Remove a file from a virtual Amiga floppy disk represented by
#' an \code{\link{amigaDisk}} object.
#'
#' Remove a file from a virtual Amiga floppy disk (represented by an \code{\link{amigaDisk}} object.
#' Make sure that the virtual disk is DOS formatted (see \link{is.amigaDOS}). This method can only remove one file at
#' a time from a virtual virtual disk, it is not allowed to use wild cards in the source or destination names.
#' It is possible to remove an entire directory at once. Use loops to remove multiple files/directories from a virtual
#' disk.
#'
#' @docType methods
#' @name adf.file.remove
#' @rdname adf.file.remove
#' @aliases adf.file.remove,amigaDisk,character,logical-method
#' @param x An \code{\link{amigaDisk}} onto which the file should be put.
#' @param file A \code{character} string of the path on the virtual floppy of the file that should be removed.
#' The path should be conform Amiga specs (see \link{current.adf.dir}). Wild cards are not allowed (see details).
#' Both files and directories can be removed from the virtual disk using this function.
#' @param full A \code{logical} value (default is \code{FALSE}). When set to \code{TRUE} not only pointers to
#' the \code{file} are removed, but also the data in the header and data blocks. When set to \code{FALSE},
#' the data is left as orphans on the disk. Technically, these files can be undeleted, unless they are overwritten.
#' @return Returns an \code{\link{amigaDisk}} object where the
#' specified \code{file} is removed.
#' @examples
#' \dontrun{
#' ## This removes a single file from a disk
#' resulting.disk <- adf.file.remove(adf.example, "df0:mods/mod.intro")
#' 
#' ## This removes the entire 's' directory and cannot be undone:
#' resulting.disk <- adf.file.remove(adf.example, "df0:s", TRUE)
#' }
#' @author Pepijn de Vries
#' @export
setMethod("adf.file.remove", c("amigaDisk", "character", "logical"), function(x, file, full) {
  # hash table length:
  ht_length  <- BLOCK_SIZE/4 - 56
  
  ##############################################################################
  ##                                                                          ##
  ##     Check if 'file' is a file or a directory, and whether it exists      ##
  ##                                                                          ##
  ##############################################################################

  fh <- find.file.header(x, file)      ## This will throw an error if the file doesn't exist
  fi <- header.info(x, fh)             ## file header info
  if (fi[[1]]$sec_type == "ST_ROOT") stop("You can't remove the root from a disk. Silly you!")

  ##############################################################################
  ##                                                                          ##
  ##                  Get info about disk from boot block                     ##
  ##                                                                          ##
  ##############################################################################
  
  bt <- boot.info(x)
  ## Correct the location of the root block if set to zero:
  NUMBER_OF_SECTORS <- ifelse(x@type == "DD", NUMBER_OF_SECTORS_DD, NUMBER_OF_SECTORS_HD)
  bt$rootblock[bt$rootblock == 0] <- NUMBER_OF_SECTORS*NUMBER_OF_CYLINDERS*NUMBER_OF_SIDES/2

  ##############################################################################
  ##                                                                          ##
  ##        find all nested files, which also have to be removed              ##
  ##                                                                          ##
  ##############################################################################
  
  tree_get <- function(h) {
    if (h == 0) h <- bt$rootblock
    result <- header.info(x, h)
    if (result[[1]]$sec_type %in% c("ST_ROOT", "ST_USERDIR")) {
      db <- unlist(lapply(result, function(z) z$datablocks[z$datablocks != 0]))
      return (c(result, do.call(c, lapply(db, tree_get))))
    } else {
      return (result)
    }
  }
  tree <- tree_get(fi[[1]]$header_key)

  ##############################################################################
  ##                                                                          ##
  ##               Remove pointer to the deleted file from the                ##
  ##                   hash chain in the parent directory                     ##
  ##                                                                          ##
  ##############################################################################

  pi <- header.info(x, fi[[1]]$parent) ## parent header info
  pi[[1]]$header_key[pi[[1]]$header_key == 0] <- bt$rootblock
  
  hash_id               <- hash.name(fi[[1]]$file_name, bt$flag$intl.mode)
  chain                 <- pi[[1]]
  pointer               <- chain$header_key
  next_pointer_in_chain <- rawToAmigaInt(x@data[pointer*BLOCK_SIZE + hash_id*4 + 25:28], 32)
  ## fix hash chain, essentially by skipping deleted file in chain
  if (next_pointer_in_chain == fi[[1]]$header_key) {
    x@data[pointer*BLOCK_SIZE + hash_id*4 + 25:28] <- raw(4)
    x@data[pointer*BLOCK_SIZE + 21:24] <- calculate.checksum(x@data[pointer*BLOCK_SIZE + 1:BLOCK_SIZE])
  } else {
    protect.count <- 0
    repeat {
      protect.count <- protect.count + 1
      if (protect.count > NUMBER_OF_SECTORS*NUMBER_OF_CYLINDERS*NUMBER_OF_SIDES)
        stop("Hash chain is unrealistically long.")
      pointer               <- next_pointer_in_chain
      chain                 <- header.info(x, next_pointer_in_chain)[[1]]
      next_pointer_in_chain <- chain$hash_chain
      if (next_pointer_in_chain == fi[[1]]$header_key) {
        fix_value <- header.info(x, next_pointer_in_chain)[[1]]$hash_chain
        x@data[pointer*BLOCK_SIZE + ht_length*4 + 209:212] <- amigaIntToRaw(fix_value, 32, F)
        x@data[pointer*BLOCK_SIZE + 21:24] <- calculate.checksum(x@data[pointer*BLOCK_SIZE + 1:BLOCK_SIZE])
        break
      }
    }
  }

  ##############################################################################
  ##                                                                          ##
  ## find out which disk blocks can be set as 'available' in the disk bitmap  ##
  ##                                                                          ##
  ##############################################################################
  
  data_blocks   <- sort(unlist(lapply(tree, function(f) f$datablocks[f$datablocks != 0])))
  header_blocks <- sort(unlist(lapply(tree, function(f) f$header_key)))
  header_blocks <- header_blocks[header_blocks > 0] ## 0 indicates root block, we should keep that...
  
  ##############################################################################
  ##                                                                          ##
  ##           nested directory cache blocks should be made available         ##
  ##           dir cache block of parent directory needs to be updated        ##
  ##                                                                          ##
  ##############################################################################
  
  dir.cache.blocks <- NULL
  if (bt$flag$dir.cache.mode) {
    if (fi[[1]]$sec_type == "ST_USERDIR") {
      dir.cache.blocks <- unlist(lapply(tree, function(z) {
        tryCatch({unlist(lapply(dir.cache.info(x, z$header_key), function(z2) z2$dir.cache))},
                 error = function(e){NULL})
        }))
      dir.cache.blocks <- unique(dir.cache.blocks)
    }
    
    ## Remove deleted file from parent directory cache, reduce number of cache blocks if possible
    dc    <- dir.cache.info(x, pi[[1]]$header_key)
    current.dc.blocks <- attributes(dc)$dc.blocks
    dc    <- do.call(rbind, lapply(dc[unlist(lapply(dc, function(z) z$header)) != fi[[1]]$header_key], as.data.frame))
    
    record.required.length <- 25 + nchar(as.character(dc$name)) + nchar(as.character(dc$comment))
    record.required.length <- 2*ceiling(record.required.length/2)
    record.required.length <- cumsum(record.required.length)
    while (any(record.required.length > (BLOCK_SIZE - 24))) {
      record.required.length[record.required.length > (BLOCK_SIZE - 24)] <-
        record.required.length[record.required.length > (BLOCK_SIZE - 24)] -
        record.required.length[which(record.required.length > (BLOCK_SIZE - 24))[[1]] - 1]
    }
    required.blocks <- 1 + length(which(diff(record.required.length) < 0))
    
    if (required.blocks < length(current.dc.blocks)) {
      dir.cache.blocks <- c(dir.cache.blocks, utils::head(current.dc.blocks, length(current.dc.blocks) - required.blocks))
      current.dc.blocks <- utils::tail(current.dc.blocks, required.blocks)
    }
    
    dc.bl <- .make.dir.cache.block(dc, pi[[1]]$header_key, current.dc.blocks)
    x@data[as.vector(outer(1:BLOCK_SIZE, current.dc.blocks*BLOCK_SIZE, "+"))] <-
      unlist(dc.bl)
  }
  
  ##############################################################################
  ##                                                                          ##
  ##                        Update the disks bitmap                           ##
  ##                                                                          ##
  ##############################################################################
  
  clear.blocks <- sort(unique(c(data_blocks, header_blocks, dir.cache.blocks)))
  x <- clear.amigaBlock(x, clear.blocks)
  
  ##############################################################################
  ##                                                                          ##
  ##               Erase the block contents if requested                      ##
  ##                                                                          ##
  ##############################################################################
  
  if (full) {
    ## Erase actual data on the blocks (not just the pointers)
    x@data[c(outer(1:BLOCK_SIZE, clear.blocks*BLOCK_SIZE, "+"))] <- raw(length(clear.blocks) * BLOCK_SIZE)
  }

  return(x)
})

#' @rdname adf.file.remove
#' @export
setMethod("adf.file.remove", c("amigaDisk", "character", "missing"), function(x, file) {
  adf.file.remove(x, file, F)
})
