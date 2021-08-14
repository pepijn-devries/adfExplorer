setGeneric("put.adf.file", function(x, source, destination, date, comment) standardGeneric("put.adf.file"))

setMethod("put.adf.file", c("amigaDisk", "raw", "character", "POSIXt", "character"), function(x, source, destination, date, comment) {
  .put.file(x, source, destination, date, comment)
})

setMethod("put.adf.file", c("amigaDisk", "raw", "character", "POSIXt", "missing"), function(x, source, destination, date, comment) {
  .put.file(x, source, destination, date)
})

setMethod("put.adf.file", c("amigaDisk", "raw", "character", "missing", "missing"), function(x, source, destination, date, comment) {
  .put.file(x, source, destination)
})

setMethod("put.adf.file", c("amigaDisk", "character", "character", "POSIXt", "character"), function(x, source, destination, date, comment) {
  .put.file(x, source, destination, date, comment)
})

setMethod("put.adf.file", c("amigaDisk", "character", "character", "POSIXt", "missing"), function(x, source, destination, date, comment) {
  .put.file(x, source, destination, date)
})

setMethod("put.adf.file", c("amigaDisk", "character", "character", "missing", "missing"), function(x, source, destination, date, comment) {
  .put.file(x, source, destination)
})

setMethod("put.adf.file", c("amigaDisk", "character", "missing", "missing", "missing"), function(x, source, destination, date, comment) {
  .put.file(x, source)
})

.put.file <- function(x, source, destination, date = Sys.time(), comment = "") {
  ## XXX remove line below when the method is exported
  warning("You called a method that was intentionally not exported. This method is not fully developped and should not be relied upon.")
  NUMBER_OF_SECTORS <- ifelse(x@type == "DD", NUMBER_OF_SECTORS_DD, NUMBER_OF_SECTORS_HD)
  if (missing(destination)) {
    ## use file name from source when destination is missing
    destination <- basename(source)
  }
  if (class(source) == "character") {
    con <- file(source, "rb")
    temp <- readBin(con, "raw", (NUMBER_OF_SIDES*NUMBER_OF_CYLINDERS*NUMBER_OF_SECTORS - 3)*BLOCK_SIZE)
    test.byte <- readBin(con, "raw", 1)
    close(con)
    if (length(test.byte) > 0) stop ("Not enough space on virtual disk for source file.")
    source <- temp
    rm(temp, test.byte)
  }
  date <- as.POSIXct(date)
  ## hash table size
  ht_length  <- BLOCK_SIZE/4 - 56
  ## file size:
  fsize <- length(source)
  
  if (adf.file.exists(x, destination)) stop("File already exists!")
  dest.split <- strsplit(destination[[1]], "[/]|[:]")[[1]]
  dest.base <- substr(destination, 0, nchar(destination) - nchar(dest.split[[length(dest.split)]]))
  destination <- dest.split[[length(dest.split)]]
  if (nchar(destination) > 30 || nchar(destination) == 0) stop("Destination filename should be between 1 and 30 characters long.")
  ## Comment in cache block can only be 22 characters long...
  if (nchar(comment) > 79) stop("Comment cannot be longer than 79 characters.")
  
  ## get the pointer to the base directory
  ## for the destination file
  if (nchar(dest.base) == 0) {
    cur.dir <- x@current.dir
  } else {
    cur.dir <- find.file.header(x, dest.base)
  }
  
  bi <- boot.info(x)
  hi <- header.info(x, cur.dir)[[1]]
  ri <- root.info(x)
  ######################################################
  ##  1: ALLOCATE FREE BLOCKS
  ######################################################
  
  ## required number of data blocks
  ndata <- ceiling(fsize/BLOCK_SIZE)
  ## If the disk uses the Old File System, data blocks hold 24 bytes less data:
  if (!bi$flag$fast.file.system) ndata <- ceiling(fsize/(BLOCK_SIZE - 24))
  
  ## a header / extension block is needed for every ht_length data blocks
  nheader <- ceiling(ndata / ht_length)
  ## But also for an empty file, a header is required:
  nheader[nheader == 0] <- 1
  
  make.new.dir.cache <- F
  ## check if the disk uses directory cache mode and if
  ## the directory cache block can hold the file info
  ## or whether a new block needs to be reserved.
  if (bi$flag$dir.cache.mode) {
    ## XXX remove line below in future versions:
    stop("Sorry directory cache mode is not yet supported by 'put.adf.file'. Please look for future releases of this package.")
    record.required.length <- 25 + nchar(destination) + nchar(comment)
    record.required.length <- 2*ceiling(record.required.length/2)
    dc <- dir.cache.info(x, cur.dir)
    current.dc.blocks <- attributes(dc)$dc.blocks
    dc <- lapply(dc, as.data.frame)
    dc <- do.call(rbind, dc)
    dc.size <- by(dc, factor(dc$dir.cache, unique(dc$dir.cache)), function(y) {
      rec.length <- 25 + as.numeric(as.character(y$name_len)) +
        as.numeric(as.character(y$comment_len))
      block.size <- 24 + sum(rec.length)
      return(data.frame(dir.cache = y$dir.cache[[1]], size = block.size))
    })
    if (length(dc.size) == 0) {
      dc.size <- data.frame(dir.cache = numeric(0), size = numeric(0))
    } else {
      dc.size <- do.call(rbind, dc.size)
    }
    if (length(dc.size) != length(current.dc.blocks)) {
      dc.size <- data.frame(dir.cache = current.dc.blocks[!(current.dc.blocks %in% dc.size$dir.cache)],
                            size = 24)
    }

    ## XXX Check what will happen if less than the current directory cache blocks are required.
    ## OS 3.0 does not manage the cache blocks very efficiently. Once
    ## a cache block is create it is not removed by the OS, even if it is
    ## not necassary any more (e.g. when files have been deleted or
    ## have been renamed with shorter names...) We could free those blocks
    ## first...

    ## if the new record wont fit in the last directory cache
    ## block, we have to make a new one.
    if ((dc.size$size[[nrow(dc.size)]] + record.required.length) > BLOCK_SIZE) {
      make.new.dir.cache <- T
    }
  }
  
  nblocks <- ndata + nheader
  ndircache <- 0
  if (make.new.dir.cache){
    nblocks <- nblocks + 1
    ndircache <- 1
  }
  
  allocated.blocks <- NULL
  try(allocated.blocks <- allocate.amigaBlock(x, nblocks), T)
  if (is.null(allocated.blocks)) stop("Not enough space on virtual disk for source file.")
  
  new.dir.cache <- numeric(0)
  if (make.new.dir.cache) new.dir.cache <- allocated.blocks[[1]]
  
  if (bi$flag$fast.file.system) {
    header.blocks <- allocated.blocks[ndircache + 1]
    if (nheader > 1) {
      ## max. 3 extension blocks after each other (so it seems)
      sep <- (((2:nheader) - 2) %% 3) +
        (ht_length + 1)*3*floor(((2:nheader) - 2) / 3)
      header.blocks <- c(header.blocks, allocated.blocks[ndircache + ht_length + sep + 2])
    }
  } else {
    header.blocks <- allocated.blocks[ndircache + (ht_length + 1)*((1:nheader) - 1) + 1]
  }
  if (ndata == 0) {
    data.blocks <- numeric(0)
  } else {
    data.blocks   <- allocated.blocks[!(allocated.blocks %in% c(header.blocks, new.dir.cache))]
  }
  
  ######################################################
  ##  2: ADD FILE TO DESTINATION HASHTABLE
  ######################################################
  
  hash.value <- hash.name(destination, bi$flag$intl.mode || bi$flag$dir.cache.mode)
  
  hashchain <- hi$datablocks[[hash.value + 1]]
  
  protect.count <- 0
  while (hashchain[[length(hashchain)]] > 0) {
    hashchain <- c(hashchain, header.info(x, hashchain[[length(hashchain)]])[[1]]$hash_chain)
    protect.count <- protect.count + 1
    if (protect.count > NUMBER_OF_SECTORS*NUMBER_OF_CYLINDERS*NUMBER_OF_SIDES) {
      stop("Hash chain is unrealistically long.")
      break
    }
  }
  ## Hash chain should be sorted by block id...
  hashchain <- c(sort(c(hashchain[hashchain != 0], allocated.blocks[[1]])), 0)
  x@data[cur.dir*BLOCK_SIZE + 25:28 + hash.value*4] <- amigaIntToRaw(hashchain[[1]], 32)
  x@data[cur.dir*BLOCK_SIZE + 21:24] <- calculate.checksum(x@data[cur.dir*BLOCK_SIZE + 1:BLOCK_SIZE])
  if (length(hashchain) > 2){
    ## set new hash values
    x@data[as.vector(outer(-15:-12, (hashchain[-length(hashchain)] + 1)*BLOCK_SIZE, "+"))] <-
      amigaIntToRaw(hashchain[-1], 32)
    ## calculate and update checksums for each file header in the hash chain
    for (i in hashchain[-length(hashchain)]) {
      x@data[i*BLOCK_SIZE + 21:24] <- calculate.checksum(x@data[i*BLOCK_SIZE + 1:BLOCK_SIZE])
    }
  }
  
  ######################################################
  ##  3: CREATE FILE HEADER(S)
  ######################################################
  
  file.headers <- lapply(1:nheader, function(y) {
    db <- data.blocks[(y - 1)*ht_length + 1:ht_length]
    db[is.na(db)] <- 0
    db <- rev(db)
    fname <- charToRaw(destination)
    fname <- rawToAmigaInt(c(as.raw(nchar(destination)), fname[1:30], raw(1)), 32)
    extension <- 0
    if (nheader > y) extension <- header.blocks[y + 1]
    if (y == 1) {
      fh <- c(TYPES$value[TYPES$type == "T_HEADER"], # primary block type
              header.blocks[[y]],                    # self pointer
              sum(db != 0),                          # high seq
              0,                                     # data size (not used)
              db[[length(db)]],                      # first_data
              0,                                     # checksum, calculate later
              db,                                    # data block pointers
              rep(0, 3),                             # unused, UID, GID, protection flags
              fsize,                                 # byte size
              rep(0, 23),                            # comm_len, com, unused
              rawToAmigaInt(amigaDateToRaw(date, tz = "UTC"), 32), # change date
              fname,                                 # file name length, file name and unused
              0,                                     # unused
              0,                                     # real_entry (FFS unused)
              0,                                     # next_link (unused)
              rep(0, 5),                             # unused
              0,                                     # next hash (zero as it is put at the end of the hash chain)
              cur.dir,                               # same as parent directory of current dir
              extension,                             # next extension block
              SEC_TYPES$value[SEC_TYPES$type == "ST_FILE"]
      )
    } else {
      fh <- c(TYPES$value[TYPES$type == "T_LIST"],   # primary block type (Extension)
              header.blocks[[y]],                    # self pointer
              sum(db != 0),                          # high seq
              0,                                     # data size (not used)
              0,                                     # Not used in extension
              0,                                     # checksum, calculate later
              db,                                    # data block pointers
              rep(0, 47),                            # unused
              header.blocks[[1]],                    # file header
              extension,                             # next extension block
              SEC_TYPES$value[SEC_TYPES$type == "ST_FILE"]
      )
    }
    fh <- amigaIntToRaw(fh, 32)
    fh[21:24] <- calculate.checksum(fh)
    ## put the header block in the disk object:
    x@data[header.blocks[[y]]*BLOCK_SIZE + 1:BLOCK_SIZE] <<- fh
  })
  
  ######################################################
  ##  4: ADD DATA BLOCKS
  ######################################################
  
  if (ndata > 0) {
    data.idx <- lapply(1:ndata, function(y) {
      if (bi$flag$fast.file.system) {
        idx <- (y - 1)*BLOCK_SIZE + 1:BLOCK_SIZE
      } else {
        idx <- (y - 1)*(BLOCK_SIZE - 24) + 1:(BLOCK_SIZE - 24)
      }
      if (bi$flag$fast.file.system) {
        rep.range <- 1:BLOCK_SIZE
        if (length(idx) < length(25:BLOCK_SIZE)){
          rep.range <- 1:length(idx)
          x@data[data.blocks[[y]]*BLOCK_SIZE + 1:BLOCK_SIZE] <- raw(1)
        }
        x@data[data.blocks[[y]]*BLOCK_SIZE + rep.range] <<- source[idx]
      } else {
        ## Write OFS stuff to first 24 bytes...
        temp <- (1:fsize)[idx]
        nextblock <- 0
        if (y < ndata) nextblock <- data.blocks[[y + 1]]
        data_head <- c(
          TYPES$value[TYPES$type == "T_DATA"],  ## block primary type
          header.blocks[[1]],                   ## self pointer
          y,                                    ## seq_num
          sum(!is.na(temp)),                    ## data_size
          nextblock,
          0                                     ## checksum, caculate below
        )
        x@data[data.blocks[[y]]*BLOCK_SIZE + 1:24] <<- amigaIntToRaw(data_head, 32)
        rep.range <- 25:BLOCK_SIZE
        if (length(idx) < length(25:BLOCK_SIZE)){
          rep.range <- 25:(24 + length(idx))
          x@data[data.blocks[[y]]*BLOCK_SIZE + 25:BLOCK_SIZE] <- raw(1)
        }
        x@data[data.blocks[[y]]*BLOCK_SIZE + rep.range] <<- source[idx]
        x@data[data.blocks[[y]]*BLOCK_SIZE + 21:24] <<- calculate.checksum(x@data[data.blocks[[y]]*BLOCK_SIZE + 1:BLOCK_SIZE])
      }
    })
  }
  
  ######################################################
  ##  5: UPDATE DIRECTORY CACHE BLOCK (IF AVAILABLE)
  ######################################################

  ## PM XXX

  ######################################################
  ##  6: UPDATE BITMAP BLOCK
  ######################################################
  
  ## first create a bitmap with all falgs set.
  bitmap <- rep(T, NUMBER_OF_CYLINDERS*NUMBER_OF_SIDES*NUMBER_OF_SECTORS)
  ## turn of flags for blocks that are already occupied and those
  ## that have been allocated for the new file.
  bitmap[c(bitmap.info(x), allocated.blocks) - 1] <- F # +1 to compensate for different index base; -2 to skip boot blocks
  ## To raw data
  bitmap <- bitmapToRaw(bitmap)
  ## Fill to complete block (1st 4 bytes are for the checksum)
  bitmap <- c(raw(4), bitmap, raw(BLOCK_SIZE - length(bitmap) - 4))
  
  ## calculate checksum for bitmap block
  bitmap[1:4] <- calculate.checksum(bitmap, chcksm.pos = 1)
  bm <- ri$bm_pages[ri$bm_pages != 0]
  if (length(bm) > 1) stop("unexpected multiple bitmap blocks found on disk, only the first will be updated.")
  bm <- bm[[1]]
  x@data[bm*BLOCK_SIZE + 1:BLOCK_SIZE] <- bitmap
  
  return(x)
}

setGeneric("adf.file.exists", function(x, file) standardGeneric("adf.file.exists"))

#' Test file or directory existsence in an amigaDisk object
#'
#' Tests whether a specific file (or directory) exists in an
#' \code{\link{amigaDisk}} object.
#'
#' This method will look for a file/directory header, based on its name.
#' If such a header exists, it is assumed that the file exists. The
#' file/directory itself is not checked for validity.
#'
#' @docType methods
#' @name adf.file.exists
#' @rdname adf.file.exists
#' @aliases adf.file.exists,amigaDisk,character-method
#' @param x An \code{\link{amigaDisk}} object in which this method
#' will check for the file's existence.
#' @param file A \code{character} string representing a file or directory name.
#' Use Amiga specifications for file name (see \code{\link{current.adf.dir}}).
#' @return Returns a \code{logical} value indicating whether the file exists
#' or not.
#' @examples
#' data(adf.example)
#' 
#' ## This file exists:
#' adf.file.exists(adf.example, "df0:mods/mod.intro")
#' 
#' ## This file doesn't:
#' adf.file.exists(adf.example, "df0:idontexist")
#' @author Pepijn de Vries
#' @export
setMethod("adf.file.exists", c("amigaDisk", "character"), function(x, file){
  fh <- NULL
  try(fh <- find.file.header(x, file), T)
  return(!is.null(fh))
})