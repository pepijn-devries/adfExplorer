#' Convert raw values into Amiga integers
#'
#' Convert raw data into 8, 16, or 32-bit signed or unsigned
#' integer values, conform Amiga specifications.
#'
#' The Commodore Amiga has specified the following data formats
#' to represent integer data: BYTE (signed 8-bit integer), UBYTE
#' (unsigned 8-bit integer), WORD (signed 16-bit integer), UWORD
#' (unsigned 16-bit integer), LONG (signed 32-bit integer), ULONG,
#' (unsigned 32-bit integer). This function converts raw data into
#' such integers. Note that WORD and UWORD are also referred to as
#' SHORT and USHORT respectively.
#'
#' @param x A vector of class `raw` to be converted into a `character`.
#' @param bits Number of bits that represents the integer value. Should be 8 or a
#' positive multitude of 8.
#' @param signed A `logical` value indicating whether the integer
#' should be signed (`TRUE`, default) or not (`FALSE`).
#' @return A `numeric` value (or a `vector` of values),
#' representing the integer data represented by the provided
#' `raw` data. Note that R defines `integer` as 32-bit
#' signed integers and cannot store the 32-bit signed values.
#' Therefore a `numeric` value is returned rather than an
#' explicit `integer`.
#' @examples
#' ## Let's start by obtaining unsigned 8-bit integers:
#' rawToAmigaInt(as.raw(0:255))
#' 
#' ## note that this is the same as:
#' as.numeric(as.raw(0:255))
#' 
#' ## but with this function we can also get signed values:
#' rawToAmigaInt(as.raw(0:255), signed = TRUE)
#'
#' ## Furthermore 32 or 16-bit integers can also be obtained.
#' ## Let's look at 16-bit integers:
#' rawToAmigaInt(as.raw(0:255), 16)
#' 
#' ## Note that 16-bit integers require twice as many bytes
#' ## as 8 bit integers:
#' length(rawToAmigaInt(as.raw(0:255), 16))
#' length(rawToAmigaInt(as.raw(0:255), 8))
#' @family raw.operations
#' @author Pepijn de Vries
#' @export
rawToAmigaInt <- function(x, bits = 8, signed = F) {
  # Convert raw values into Amiga integers (BYTE (8 bit signed), UBYTE (8 bit unsigned),
  # WORD (16 bit signed), UWORD (16 bit unsigned), LONG (32 bit signed), ULONG (32 bit unsigned))
  if ((bits %% 8) != 0 || bits < 8) stop("Bits should be positive, it should also be a multitude of 8 (or 8 itself).")
  # pad x with zeros when it does not consist of a multitude of specified bits
  x <- c(x, raw(length(x) %% (bits/8)))
  i.start <- 1:floor(length(x)/(bits/8))
  i.stop  <- i.start*(bits/8)
  i.start <- (i.start - 1)*(bits/8) + 1
  result <- mapply(function(start, stop) {
    y <- x[start:stop]
    result <- as.numeric(unlist(lapply(y, function(z) rev(rawToBits(z)))))
    result <- sum(2^(which(rev(result) == as.raw(0x01)) - 1))
    return(result)
  }, start = i.start, stop = i.stop)
  if (signed) {
    result[result >= (2^bits)/2] <- result[result >= (2^bits)/2] - (2^bits)
    return(result)
  } else {
    return(result)
  }
}

#' Convert Amiga integers into raw values
#'
#' Convert 8, 16, or 32-bit signed or unsigned
#' integer values into raw data, conform Amiga specifications.
#'
#' The Commodore Amiga has specified the following data formats
#' to represent integer data: BYTE (signed 8-bit integer), UBYTE
#' (unsigned 8-bit integer), WORD (signed 16-bit integer), UWORD
#' (unsigned 16-bit integer), LONG (signed 32-bit integer), ULONG,
#' (unsigned 32-bit integer). This function converts 
#' such integers into raw data.
#'
#' @param x A vector of class `numeric` which needs to be converted into raw values.
#' @param bits Number of bits that represents the integer value. Should be 8 or a
#' positive multitude of 8.
#' @param signed A `logical` value indicating whether the numeric values
#' is signed (`TRUE`, default) or not (`FALSE`).
#' @return Returns (a `vector` of) `raw` data, representing
#' the integer value(s) conform Amiga specifications.
#' @examples
#' ## some unsigned 8-bit integers:
#' ubyte <- sample.int(255, 100, TRUE)
#' 
#' ## The same values as raw data:
#' amigaIntToRaw(ubyte)
#' 
#' ## some signed 8-bit integers:
#' byte <- sample.int(255, 100, TRUE) - 128
#' 
#' ## The same values as raw data:
#' amigaIntToRaw(byte, signed = TRUE)
#' 
#' ## some signed 16-bit integers:
#' word <- sample.int(2^16, 100, TRUE) - 2^15
#' 
#' ## The same values as raw data:
#' amigaIntToRaw(word, 16, TRUE)
#' 
#' ## note that 16-bit integers require
#' ## twice as many raw values:
#' length(amigaIntToRaw(word, 16, TRUE))
#' length(amigaIntToRaw(byte, 8, TRUE))
#' @family raw.operations
#' @author Pepijn de Vries
#' @export
amigaIntToRaw <- function(x, bits = 8, signed = F) {
  x <- round(x)
  if (!signed && any(x < 0)) stop("negative values not allowed for unsigned values.")
  val.range <- c(0, 2^bits - 1)
  if (signed) val.range <- c(-(2^bits)/2,(2^bits)/2 - 1)
  if (any(x < val.range[1]) || any(x > val.range[2])) {
    warning("One or more values are out of the specified bit-range. They will be clipped...")
    x[x < val.range[1]] <- val.range[1]
    x[x > val.range[2]] <- val.range[2]
  }
  if (signed) x[x < 0] <- (2^bits) + x[x < 0]
  ## used later on to reorder bits for the little-endian bytes
  idx <- sort(rep(((1:(bits/8)) - 1)*8, 8), T) + rep(1:8, bits/8)
  result <- unlist(lapply(x, function(y) {
    bitlist <- NULL
    while (y > 0) {
      bitlist <- c(bitlist, y %% 2)
      y <- floor(y/2)
    }
    bitlist <- c(bitlist, numeric(bits - length(bitlist)))
    res <- packBits(as.logical(bitlist)[idx], "raw")
    return(res)
  }))
  return(result)
}

# Function that replaces special characters in a raw format
# by dots than converts it into a character string...
rawToCharDot <- function(raw_dat) {
  raw_dat[raw_dat <= as.raw(0x1F)] <- as.raw(46)
  raw_dat[raw_dat >= as.raw(0x21) & raw_dat <= as.raw(0x25)] <- as.raw(46)
  raw_dat[raw_dat == as.raw(0x81)] <- as.raw(46)
  raw_dat[raw_dat == as.raw(0x8d)] <- as.raw(46)
  raw_dat[raw_dat == as.raw(0x8f)] <- as.raw(46)
  raw_dat[raw_dat == as.raw(0x90)] <- as.raw(46)
  raw_dat[raw_dat == as.raw(0x9d)] <- as.raw(46)
  raw_dat[raw_dat == as.raw(0xad)] <- as.raw(46)
  raw_dat[raw_dat == as.raw(0x7f)] <- as.raw(46)

  return(iconv(rawToChar(raw_dat), from = "ISO-8859-1", to = "UTF-8"))
}

#' Display raw data in a comprehensive way
#'
#' Cat `raw` data to the sink in columns with ASCII code
#'
#' As binary data is hard to decipher this function will
#' cat raw data as hexadecimal code in columns, together
#' with the relative (hexadecimal) address of the data and
#' an ASCII translation of the data. Hexadecimals are shown
#' in space separated columns for improved readability. Special
#' characters are replaced by dots in the ASCII representation.
#'
#' Raw data is padded with zeros at the end to fill remaining
#' columns...
#' @param x A vector of class `raw` to be displayed.
#' @param ncol Number of columns of hexadecimal code to display.
#' @param col.wid Width of each column (in bytes) to display.
#' @param address.len Length of the hexadecimal address
#' (in number of hexadecimal digits) to display.
#' @param hex.upper `logical` value, to specify whether hexadecimals
#' should be displayed in uppercase (`TRUE`, default) or
#' lowercase (`FALSE`).
#' @return The `character` string send to the sink is also
#' returned by the function.
#' @examples
#' ## Display some raw random data:
#' displayRawData(as.raw(sample.int(100)))
#' 
#' ## Display the full ASCII table:
#' displayRawData(as.raw(0:255))
#' @family raw.operations
#' @author Pepijn de Vries
#' @export
displayRawData <- function(x, ncol = 4, col.wid = 4, address.len = 3, hex.upper = T) {
  nrow <- ceiling(length(x) / (ncol*col.wid))
  len  <- nrow*ncol*col.wid
  x   <- c(x, raw(len - length(x)))
  m   <- matrix(x, nrow, ncol*col.wid, byrow = T)
  hex <- apply(m, 1, function (x) paste0(sprintf("%02x", as.numeric(x)), collapse = ""))
  hex <- unlist(lapply(hex, function (x) paste0(substring(x,
                                                          seq(1, (2*ncol*col.wid-1), 2*col.wid),
                                                          seq(1, (2*ncol*col.wid-1), 2*col.wid) + 2*col.wid - 1),
                                                collapse = " ")))
  if (hex.upper) hex <- toupper(hex)
  ch  <- apply(m, 1, rawToCharDot)
  add <- sprintf(paste0("%0", address.len, "x"), (0:(length(ch) - 1))*ncol*col.wid)
  if (hex.upper) add <- toupper(add)
  add <- paste0("0x", add)
  m   <- apply(cbind(add, hex, ch), 1, paste0, collapse = "  ")
  m   <- paste0(m, collapse = "\n")
  cat(m)
  return(invisible(m))
}

## function to get the address of the root block
## x should be of class "amigaDisk" or character "DD" or "HD"
get.root.id <- function(x) {
  if (inherits(x, "amigaDisk")) x <- x@type
  x <- match.arg(x, c("DD", "HD"))
  NUMBER_OF_SECTORS <- NUMBER_OF_SECTORS_DD
  if (x == "HD") NUMBER_OF_SECTORS <- NUMBER_OF_SECTORS_HD
  return(ceiling((NUMBER_OF_CYLINDERS*NUMBER_OF_SIDES*NUMBER_OF_SECTORS - 1)/2))
}

## calculate boot blocks check sum
## maybe export at later stage
calculate.boot.checksum <- function(x, as.raw = T) {
  if (inherits(x, "amigaDisk")) {
    return(calculate.boot.checksum.dat(x@data, as.raw))
  } else if (typeof(x) == "raw") {
    return(calculate.boot.checksum.dat(x, as.raw))
  } else stop("x should be raw data or an amigaBlock object")
}

## calculate boot blocks check sum
## maybe export at later stage
calculate.boot.checksum.dat <- function(x, as.raw = T) {
  checksum <- 0
  for(i in seq(1, 2*BLOCK_SIZE, by = 4)) {
    if (i != 5) { # skip the current checksum
      checksum <- checksum + rawToAmigaInt(x[i:(i + 3)], 32, F)
      if (checksum >= 0xffffffff)
        checksum <- checksum %% 0xffffffff
    }
  }
  checksum <- 0xffffffff - checksum
  if (as.raw) return(amigaIntToRaw(checksum, 32, F)) else return (checksum)
}

## boot info. Maybe export at some stage
## x should be of class "amigaDisk"
boot.info <-  function(x) {
  flag <- as.logical(rawToBits(x@data[4]))
  list(
    disk.type = rawToCharDot(x@data[1:3]),
    flag = list(fast.file.system  = flag[1],
                intl.mode         = flag[2],
                dir.cache.mode    = flag[3]),
    checksum  = rawToAmigaInt(x@data[5:8], 32, F),
    rootblock = rawToAmigaInt(x@data[9:12], 32, F)
  )
}

#' Convert raw data into a bitmap or vice versa
#'
#' Convert raw data into a bitmap or vice versa (i.e., binary data)
#' conform Amiga specifications.
#'
#' A bitmap is simply put a map of bits (binary data, which can
#' be interpeted as 0 or 1; or FALSE and TRUE). Bitmaps can have
#' several purposes, also on the Commodore Amiga. The Amiga file
#' system uses a bitmap to indicates which blocks are occupied with
#' data and which are free. Bitmaps can also be used in bitmap images
#' where each bit indicates which color should be used for a specific
#' pixel in an image. These function can be used to convert raw data
#' into usable bitmaps or vice versa.
#' 
#' As the Commodore Amiga is a big-endian system (most significant
#' bit first) using a 32 bit CPU, it may sometimes necessary to invert
#' the bits of a byte or longs (4 bytes, 32 bits), which can be done
#' with the arguments '`invert.bytes`' and '`invert.longs`'
#' respectively.
#'
#' @param x A `vector` of `raw` data, in case
#' `rawToBitmap` is used. A `vector` of `raw`,
#' `interger` or `logical` values should be used in
#' case of `bitmapToRaw`. In the latter case each value in the
#' `vector` is interpreted as a bit and should be a multiple of
#' 8 long.
#' @param invert.bytes A `logical` value. When set to `TRUE`,
#' the bit order of bytes are reversed.
#' @param invert.longs A `logical` value. When set to `TRUE`,
#' the bit order of long values (32 bits) are reversed. When `x`
#' does not have a multiple length of 32 bits or 4 bytes, `x` will
#' be padded with zeros to the right, but the result will be trimmed to
#' correspond with the length of `x`. Note that data might get lost
#' this way.
#' @return Returns a `vector` of `raw` data in case of
#' `bitmapToRaw`, and a `vector` of binary `raw` values
#' in case of `rawToBitmap`.
#' @examples
#' ## The bitmap block of the example disk is located at block
#' ## number 882 (note that this is not true for all disks,
#' ## the actual location is stored in the root block)
#' data(adf.example)
#' bitmap.block <- amigaBlock(adf.example, 881)
#'
#' ## bitmap data are stored in bytes 5 up to 224 in this block:
#' bitmap.raw <- bitmap.block@data[5:224]
#' 
#' ## let's get the bitmap from the raw data:
#' bitmap <- rawToBitmap(bitmap.raw)
#' 
#' ## Whe can now get the occupied blocks (minus one is used for
#' ## the discrepancy in indexing):
#' which(bitmap != as.raw(0x01)) - 1
#' 
#' ## we can also do the reverse:
#' bitmap.raw.new <-  bitmapToRaw(bitmap)
#' ## it should be the same as the original raw data:
#' all(bitmap.raw.new == bitmap.raw)
#' 
#' ## WARNING: don't use these methods to directly
#' ## modify an amigaDisk objects bitmap block. The
#' ## file system on that object may get corrupted.
#' ## All methods in this package should update the
#' ## bitmap block automatically and cleanly...
#' @family raw.operations
#' @author Pepijn de Vries
#' @export
rawToBitmap <- function(x, invert.bytes = F, invert.longs = T) {
  if (typeof(x) != "raw") stop("Argument 'x' should be a vector of raw data.")
  if (!all("logical" %in% c(typeof(invert.bytes), typeof(invert.longs)))) stop ("Both 'invert.bytes' and 'invert.longs' should be a logical value.")
  if (length(invert.bytes) != 1 || length(invert.longs) != 1) stop("Both 'invert.bytes' and 'invert.longs' should have a length of 1.")
  ## pad data with zeros and trim at the end
  true.len <- length(x)
  x <- c(x, raw(4 - (true.len %% 4)))
  len <- length(x)
  if (invert.longs) {
    l2 <- ceiling(len/4)
    ord2 <- 1 + sort(rep((0:(l2 - 1))*4, 4)) + (3:0)
    ord2 <- ord2[1:len]
    x <- x[ord2]
  }
  if (invert.bytes) {
    ord <- 1 + sort(rep((0:(len - 1))*8, 8)) + (7:0)
  } else {
    ord <- 1:(8*len)
  }
  ## trim the result to correspond with the input length (data might get lost!)
  rawToBits(x)[ord][1:(true.len*8)]
}

#' @name bitmapToRaw
#' @rdname rawToBitmap
#' @export
bitmapToRaw <- function(x, invert.bytes = T, invert.longs = T) {
  # 'x' should be anything that is accepted by packBits
  if (!all("logical" %in% c(typeof(invert.bytes), typeof(invert.longs)))) stop ("Both 'invert.bytes' and 'invert.longs' should be a logical value.")
  if (length(invert.bytes) != 1 || length(invert.longs) != 1) stop("Both 'invert.bytes' and 'invert.longs' should have a length of 1.")
  true.len <- length(x)
  ## pad with zeros
  x <- c(x, raw(32 - (true.len %% 32)))
  len <- length(x)/8
  if (invert.bytes) {
    ord <- 1 + sort(rep((0:(len - 1))*8, 8)) + (7:0)
  } else {
    ord <- 1:(8*len)
  }
  if (invert.longs) {
    l2 <- ceiling(8*len/32)
    ord2 <- 1 + sort(rep((0:(l2 - 1))*32, 32)) + (31:0)
    ord2 <- ord2[1:(8*len)]
    x <- x[ord2]
  }
  ## order results and trim length to correspond with input
  x <- packBits(x[ord])[1:ceiling(true.len/8)]
  return(x)
}

## bitmap info. Maybe export at some stage
## x should be of class "amigaDisk"
bitmap.info <- function(x) {
  root <- root.info(x)
  if (!root$bm_flag) stop("Disk does not have a valid bitmap!")
  bm_pages <- root$bm_pages[root$bm_pages != 0]
  # TODO check bm_extension. Should not be required for floppy disks
  bitmap <- lapply(bm_pages, function(y) {
    # first four bytes are checksum of block
    amigaBlock(x, y)@data[-1:-4]
  })
  bitmap <- do.call(c, bitmap)
  if (x@type == "DD") {
    len <- NUMBER_OF_SIDES*NUMBER_OF_CYLINDERS*NUMBER_OF_SECTORS_DD - 2
  } else {
    len <- NUMBER_OF_SIDES*NUMBER_OF_CYLINDERS*NUMBER_OF_SECTORS_HD - 2
  }
  bitmap <- rawToBitmap(bitmap[1:ceiling(len/8)])
  bitmap <- which(!as.logical(bitmap)) + 1
  return(bitmap)
}

## function used in international mode to convert
## text into upper case
intl_toupper <- function(x, international = T) {
  values <- charToRaw(x)
  shift <- as.numeric(charToRaw("a")) - as.numeric(charToRaw("A"))
  if (international) {
    sel <- (values >= charToRaw("a") & values <= charToRaw("z")) |
      (values >= as.raw(224) & values <=  as.raw(254) & values != as.raw(247))
  } else {
    ## This seems to be the OFS way for to_upper. Might need some more
    ## checks TODO
    sel <- values >= charToRaw("a") & values <= charToRaw("z")
  }
  values[sel] <- 
    as.raw(as.numeric(values[sel]) - shift)
  return (rawToChar(values))
}

# This function returns the hash table value based on the filename
hash.name <- function(x, intl = F) {
  # Amiga uses ISO 8859 Latin-1 character set
  Encoding(x) <- "latin1"
  hash <- nchar(x)
  fun <- function(x, b = intl) intl_toupper(x, b)
  for (i in 1:hash) {
    hash <- bitwAnd(hash <- hash*13 + as.numeric(charToRaw(fun(substr(x, i, i)))), 0x7ff)
  }
  hash <- hash %% ((BLOCK_SIZE/4) - 56)
  return(hash)
}

# x = amigaDisk
file.info <- function(x, block = 880) {
  root.id    <- get.root.id(x)
  if (block == root.id) {
    info <- root.info(x)
    hash.table <- info$ht
  } else {
    info <- header.info(x, block)[[1]]
    hash.table <- info$datablocks
  }
  hash.table <- hash.table[hash.table != 0]
  result <- NULL
  ## loop through all hash chains
  reality.check <- 0
  while (T) {
    new.result <- header.info(x, hash.table)
    result <- c(result, new.result)
    hash.table <- unlist(lapply(new.result, function(x) x$hash_chain))
    hash.table <- hash.table[hash.table != 0]
    # When there are no more new hash.tables, break.
    reality.check <- reality.check + 1
    if (reality.check > NUMBER_OF_SIDES*NUMBER_OF_CYLINDERS*NUMBER_OF_SECTORS_HD) stop("Hash chain appears to be unrealistically long.")
    if (length(hash.table) == 0) break
  }
  return(result)
}

dir.cache.info <- function(x, block) {
  if (missing(block)) block <- get.root.id(x@type)
  hi <- header.info(x, block)[[1]]
  bi <- boot.info(x)
  if (!(hi$sec_type %in% c("ST_ROOT", "ST_USERDIR"))) stop("Directory cache information can only be obtained from a (root) directory.")
  if (!bi$flag$dir.cache.mode || hi$extension == 0) stop("No pointer to directory cache block found.")
  dc.id <- hi$extension
  records <- list()
  while (dc.id[length(dc.id)] != 0) {
    dc <- amigaBlock(x, dc.id[length(dc.id)])
    dc.info <- as.list(rawToAmigaInt(dc@data[1:(6*4)], 32))
    names(dc.info) <- c("type", "header_key", "parent", "records_nb", "next_dirc", "chksum")
    dc.id <- c(dc.id, dc.info$next_dirc)
    offset <- 24
    if (dc.info$records_nb > 0) {
      for (i in 1:dc.info$records_nb) {
        rec <- as.list(rawToAmigaInt(dc@data[offset + 1:(3*4)], 32))
        rec <- c(rec, as.list(rawToAmigaInt(dc@data[offset + 13:16], 16)))
        rec[[length(rec) + 1]] <- rawToAmigaDate(dc@data[offset + 17:22], "short")
        rec <- c(rec, as.list(rawToAmigaInt(dc@data[offset + 23], 8, T)))
        rec <- c(rec, as.list(rawToAmigaInt(dc@data[offset + 24], 8)))
        names(rec) <- c("header", "size", "protect", "UID", "GID",
                        "date", "type", "name_len")
        rec$type[rec$type < 0] <- 0x100000000 + rec$type[rec$type < 0]
        rec$type <- SEC_TYPES$type[SEC_TYPES$value == rec$type]
        rec$name <- rawToChar(dc@data[offset + 25:(24 + rec$name_len)])
        rec$comment_len <- rawToAmigaInt(dc@data[offset + 25 + rec$name_len], 8)
        if (rec$comment_len > 0) {
          rec$comment <- rawToChar(dc@data[offset + rec$name_len + 26:(25 + rec$comment_len)])
        } else {
          rec$comment <- ""
        }
        tot.len <- 3*4 + 5*2 + 3 + rec$name_len + rec$comment_len
        tot.len <- 2*ceiling(tot.len/2)
        offset <- offset + tot.len
        rec <- c(list(dir.cache = dc.info$header_key), rec)
        records[[length(records) + 1]] <- rec
      }
    }
  }
  attributes(records)$dc.blocks <- dc.id[-length(dc.id)]
  return(records)
}

setGeneric("get.diskLocation", function(disktype, block) standardGeneric("get.diskLocation"))

#' Get the physical location on the disk for a specific block
#'
#' Get the side, cylinder and sector on a disk, based on disk type and
#' block id.
#'
#' Data on Amiga floppy disks are stored as 512 byte blocks. These blocks
#' are physically stored on a specific cylinder and side at a specific sector.
#' This method returns the identifiers for the physical location based on the
#' block identifier. The inverse of this function is achieved with the
#' [`get.blockID`] method.
#'
#' @docType methods
#' @name get.diskLocation
#' @rdname get.diskLocation
#' @aliases get.diskLocation,character,numeric-method
#' @param disktype A `character` string indicating the type of disk:
#' `DD` for double density disks. `HD` for high density disks.
#' @param block `numeric` identifier of a block. Whole numbers ranging from
#' 0 up to 1759 (for DD disks) or 3519 (for HD disks). Note that the base
#' index is zero (for consitency with Amiga specifications and documentation)
#' opposed to the base of one used in R.
#' @return Returns a `list` with corresponding sector, side and cylinder
#' identifiers (`numeric`).
#' @examples
#' ## get the physical location of the first 20 blocks on a DD disk
#' ## and arrange as a data.frame:
#' as.data.frame(get.diskLocation("DD", 0:19))
#' @family block.operations
#' @author Pepijn de Vries
#' @export
setMethod("get.diskLocation", c("character", "numeric"), function(disktype, block) {
  disktype <- match.arg(disktype, c("DD", "HD"), several.ok = T)
  block    <- round(block)
  NUMBER_OF_SECTORS <- rep(NUMBER_OF_SECTORS_DD, length(disktype))
  NUMBER_OF_SECTORS[disktype == "HD"] <- NUMBER_OF_SECTORS_HD
  maxblock <- NUMBER_OF_SECTORS*NUMBER_OF_CYLINDERS*NUMBER_OF_SIDES
  if (any(block < 0) || any(block >= maxblock)) stop ("Block id out of range (0-1759/3519).")
  result <- list()
  result$sector   <- block %% NUMBER_OF_SECTORS
  block           <- (block - result$sector)/NUMBER_OF_SECTORS
  result$side     <- block %% NUMBER_OF_SIDES
  block           <- (block - result$side)/NUMBER_OF_SIDES
  result$cylinder <- block %% NUMBER_OF_CYLINDERS
  return(result)
})

setGeneric("get.blockID", function(disktype, sector, side, cylinder) standardGeneric("get.blockID"))

#' Get the block ID from the physical location on the disk
#'
#' Get the block identifier based on the physical location on a disk (side,
#' cylinder and sector) and the disk type.
#'
#' Data on Amiga floppy disks are stored as 512 byte blocks. These blocks
#' are physically stored on a specific cylinder and side at a specific sector.
#' This method returns the block identifier based on the physical location
#' on the disk. The inverse of this function is achieved with the
#' [`get.diskLocation`] method.
#' 
#' Note that all identifiers (or indices) have a base at zero, for consistency
#' with Amiga specifications and documentation, opposed to the base of one
#' used in R.
#'
#' @docType methods
#' @name get.blockID
#' @rdname get.blockID
#' @aliases get.blockID,character,numeric,numeric,numeric-method
#' @param disktype A `character` string indicating the type of disk:
#' `DD` for double density disks. `HD` for high density disks.
#' @param sector `numeric` identifier for the sector on the disk, ranging
#' from 0 up to 10 (`DD` disks) or 21 (`HD` disks).
#' @param side `numeric` identifier for the side of the disk (0 or 1).
#' @param cylinder `numeric` identifier for the cylinder on the disk,
#' ranging from 0 up to 79.
#' @return Returns the `numeric` identifier for the corresponding block.
#' @examples
#' ## Get the block identifier for sectors 0 up to 3 combined with
#' ## cylinders 0 up to 3 on side 0 of the disk:
#' get.blockID(disktype = "DD",
#'             sector   = 0:3,
#'             side     = 0,
#'             cylinder = 0:3)
#' @family block.operations
#' @author Pepijn de Vries
#' @export
setMethod("get.blockID", c("character", "numeric", "numeric", "numeric"), function(disktype, sector, side, cylinder) {
  disktype <- match.arg(disktype, c("DD", "HD"), several.ok = T)
  sector   <- round(sector)
  side     <- round(side)
  cylinder <- round(cylinder)
  NUMBER_OF_SECTORS <- rep(NUMBER_OF_SECTORS_DD, length(disktype))
  NUMBER_OF_SECTORS[disktype == "HD"] <- NUMBER_OF_SECTORS_HD
  if (any(cylinder < 0) || any(cylinder >= NUMBER_OF_CYLINDERS)) stop ("Cylinder id out of range (0-79).")
  if (any(side < 0) || any(side >= NUMBER_OF_SIDES)) stop ("Side id out of range (0-1).")
  if (any(sector < 0) || any(sector >= NUMBER_OF_SECTORS)) stop ("Sector id out of range (0-10/21).")
  cylinder*NUMBER_OF_SIDES*NUMBER_OF_SECTORS + side*NUMBER_OF_SECTORS + sector
})

#' Convert raw values into a date time object
#'
#' This function converts raw data into a date time object conform the
#' Amiga file system specifications.
#'
#' The Amiga file system stores date time objects as three unsigned
#' short (16 bit) or long (32 bit) integers. Where the values are
#' number of days, minutes and ticks (fiftieth of a second) since
#' 1978-01-01 respectively.
#' 
#' As these values are always positive, only date time values on or after
#' 1978-01-01 are allowed. The inverse of this function can be achieved
#' with [`amigaDateToRaw`].
#' @param x a `vector` of `raw` values with a length of a multitude
#' of 6 (for the short format) or 12 (for the long format).
#' @param format a `character` string indicating whether the date
#' is stored as `short` or `long` integers.
#' @param tz A `character` string specifying the time zone to be used
#' to retrieve the date time object. Note that the time zone is not stored
#' on the Amiga. By default the Universal time zone (UTC) is assumed.
#' @return Returns a [`POSIXct`][base::DateTimeClasses] object based on the provided
#' raw data.
#' @examples
#' ## all raw data is zero, so the origin date is returned:
#' rawToAmigaDate(raw(12))
#' 
#' ## let's get the date, one day, one minute and 50 ticks from the origin:
#' rawToAmigaDate(amigaIntToRaw(c(1, 1, 50), 32))
#' @family raw.operations
#' @author Pepijn de Vries
#' @export
rawToAmigaDate <- function(x, format = c("long", "short"), tz = "UTC") {
  x        <- as.raw(x)
  format   <- match.arg(format, c("long", "short"))
  ## Root and file headers uses longs. Directory cache block uses shorts
  ## The date format uses unsigned integers and is therefore always
  ## after the origin data (1978-01-01 00:00:00)
  byte_len <- 4
  if (format == "short") byte_len <- 2
  if ((length(x) %% (byte_len*3)) != 0) stop (sprintf("x should hold 3 %ss."))
  x <- matrix(x, ncol = ifelse(format == "long", 12, 6), byrow = T)
  result <- apply(x, 1, function(y) {
    val <- rawToAmigaInt(y, ifelse(format == "long", 32, 16))
    if (val[[2]] >= 1440) stop ("Number of minutes out of range.")
    if (val[[3]] >= 3000) stop ("Number of ticks out of range.")
    return(as.POSIXct(val[[1]]*60*60*24 +   # days
                        val[[2]]*60 +       # minutes
                        val[[3]]/50,        # ticks (1/50 second)
                      tz = tz, origin = "1978-01-01"))
  })
  return(as.POSIXct(result, tz = tz, origin = "1970-01-01"))
}

#' Convert date time objects into raw values
#'
#' This function converts date-time objects into raw data conform
#' Amiga file system specifications.
#'
#' The Amiga file system stores date time objects as three unsigned
#' short (16 bit) or long (32 bit) integers. Where the values are
#' number of days, minutes and ticks (fiftieth of a second) since
#' 1978-01-01 respectively.
#' 
#' As these values are always positive, only date time values on or after
#' 1978-01-01 are allowed. The inverse of this function can be achieved
#' with [`rawToAmigaDate`].
#' @param x A (`vector` of) [`POSIXt`][base::DateTimeClasses] object(s).
#' @param format a `character` string indicating whether the date
#' should be stored as `short` or `long` integers.
#' @param tz A `character` string specifying the time zone to be used
#' to convert the date time object. Note that the time zone is not stored
#' on the Amiga. By default the Universal time zone (UTC) is assumed.
#' You will get a warning when you use a timezone other then UTC.
#' @return returns `raw` data reflecting the date-time objects conform
#' the Amiga file system specifications.
#' @examples
#' ## Note that using the same date-time with different timezones will
#' ## result in different raw data. The time zone is not stored.
#' amigaDateToRaw(as.POSIXct("1978-01-01 13:30", tz = "UTC"))
#' amigaDateToRaw(as.POSIXct("1978-01-01 13:30", tz = "CET"))
#' @family raw.operations
#' @author Pepijn de Vries
#' @export
amigaDateToRaw <- function(x, format = c("long", "short"), tz = "UTC") {
  format   <- match.arg(format, c("long", "short"))
  if (!inherits(x, "POSIXt")) stop("x should be a date-time object (POSIXt).")
  if (any(x < as.POSIXct("1978-01-01", tz = tz))) stop("The date should be 1978-01-01 or later.")
  x     <- as.numeric(x) - as.numeric(as.POSIXct("1978-01-01 00:00:00", tz = tz))
  days  <- floor(x/86400)
  x     <- x - days*86400
  mins  <- floor(x/60)
  x     <- x - mins*60
  ticks <- round(x*50)
  amigaIntToRaw(as.vector(rbind(days, mins, ticks)), ifelse(format == "long", 32, 16))
}
