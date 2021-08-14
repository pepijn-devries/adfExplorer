validity.amigaDisk <- function(object) {
  # This will purely check whether the object holds
  # valid (amount of) data. A valid ADF object
  # does not necessarily mean a valid dos disk
  NUMBER_OF_SECTORS <- NUMBER_OF_SECTORS_DD
  if (object@type == "HD") NUMBER_OF_SECTORS <- NUMBER_OF_SECTORS_HD
  if (typeof(object@data) != "raw") return (F)
  if (typeof(object@type) != "character") return (F)
  if (length(object@type) != 1) return (F)
  if (typeof(object@current.dir) != "integer") return (F)
  # Current dir should be a single id value:
  if (length(object@current.dir) != 1) return (F)
  # Current dir should point to a block that is on the disk:
  if (object@current.dir < 0 || object@current.dir > NUMBER_OF_SECTORS*NUMBER_OF_SIDES*NUMBER_OF_CYLINDERS) return (F)
  if (!(object@type %in% c("DD", "HD"))) return (F)
  if (length(object@data) != BLOCK_SIZE*NUMBER_OF_SECTORS*NUMBER_OF_SIDES*NUMBER_OF_CYLINDERS) return (F)
  return(T)
}

#' The amigaDisk class
#'
#' An S4 class representing the information from an Amiga Disk File.
#'
#' An Amiga Disk File (ADF) holds the raw data of an amiga disk
#' in the same order as blocks (\code{\link{amigaBlock-class}})
#' on the physical disks. As an Amiga
#' Disk can hold any kind of information, so can this class.
#'
#' An ADF file does not hold any other information. The size of
#' the file will dictate whether it represents a double density
#' floppy disk (880 kB) or a high density floppy disk (1760 kB).
#' The disk type is also stored in this class.
#' 
#' Finally, the current directory is stored with this class. Which
#' is only useful for DOS-formatted disks (with a file structure).
#' By default this is set to the disk's root.
#'
#' For more (technical) backgrounds please check this package's
#' \href{../doc/amigaDiskFiles.html}{vignette}.
#' 
#' Use the objects constructor (\code{new("amigaDisk")}) to create
#' a completely blank disk (without a filesystem). If you want to be
#' able to transfer files from and to the virtual disk, use
#' \code{\link{blank.amigaDOSDisk}} instead.
#' 
#' @slot data The \code{raw} data of the virtual disk. It should be
#' a \code{vector} of length 901,120 in case of a double density disk and
#' 1,802,240 in case of a high density disk.
#' @slot type A \code{character} indicating whether the virtual disk
#' represents a \code{"DD"} (double density, most common) or \code{"HD"} (high density)
#' disk.
#' @slot current.dir An \code{integer}, pointing at the block address
#' of the current directory of this virtual disk. Use
#' \code{\link{current.adf.dir}} to get or set the current directory.
#'
#' @name amigaDisk-class
#' @rdname amigaDisk-class
#' @aliases amigaDisk
#' @examples
#' ## This creates a blank non-bootable, non-DOS disk:
#' new("amigaDisk")
#' @family amiga.disk.access
#' @author Pepijn de Vries
#' @exportClass amigaDisk
setClass("amigaDisk",
         representation(data = "raw", type = "character", current.dir = "integer"),
         prototype(data = raw(NUMBER_OF_CYLINDERS*NUMBER_OF_SECTORS_DD*NUMBER_OF_SIDES*BLOCK_SIZE),
                   type = "DD",
                   current.dir = 880L),
         validity = validity.amigaDisk)

setGeneric("calculate.checksum", function(x, block, chcksm.pos = 21, as.raw = T) standardGeneric("calculate.checksum"))

## calculate any block check sum
## maybe export at later stage
setMethod("calculate.checksum", c("amigaDisk", "numeric"), function(x, block, chcksm.pos = 21, as.raw = T) {
  # This checksum routine appears to be a bit off, but returns the
  # correct checksum. Maybe test some more XXX
  x <- amigaBlock(x, block)
  calculate.checksum(x@data, NULL, chcksm.pos, as.raw)
})

setMethod("calculate.checksum", c("raw", "ANY"), function(x, block, chcksm.pos = 21, as.raw = T) {
  checksum <- 0
  for(i in seq(1, BLOCK_SIZE, by = 4)) {
    if (i != chcksm.pos) { # skip the current checksum
      checksum <- checksum + rawToAmigaInt(x[i:(i + 3)], 32, F)
      if (checksum >= 0x100000000)
        checksum <- checksum %% 0x100000000
    }
  }
  checksum <- 0x100000000 - checksum
  if (as.raw) return(amigaIntToRaw(checksum, 32, F)) else return (checksum)
})

setGeneric("read.adf", function(file) standardGeneric("read.adf"))

#' Read an Amiga Disk File
#'
#' Read data from an Amiga Disk File (ADF) to an \code{\link{amigaDisk}}
#' object. Alternatively data can be read from an ADZ file.
#'
#' Amiga Disk Files usually have a .adf-extension to the file name.
#' It should be 880 kB (double density) or 1760 kB (high density)
#' in size. This function can read such files.
#' 
#' Alternatively, ADZ files can also be read. These are essentially
#' gzipped ADF files.
#' 
#' Note that this package cannot read extended ADF files containing
#' information on the disk's Modified frequency modulation (MFM).
#' This information is typically only required for copy protected disk's
#' and is therefore out of the scope of this package.
#'
#' @docType methods
#' @name  read.adf
#' @rdname read.adf
#' @aliases read.adf,character-method
#' @param file Either a file name or a file connection, that
#' allows reading binary data (see e.g., \code{\link[base]{file}} or
#' \code{\link[base]{url}}). \code{read.adz} only accepts file names.
#' @return Returns an \code{\link{amigaDisk}} object read from the provided amiga disk file
#'
#' @examples
#' \dontrun{
#' ## In order to read an adf-file, we first need one.
#' ## so let's first write the example obect to a file:
#' data(adf.example)
#' 
#' ## write it to the current working directory:
#' write.adf(adf.example, "test.adf")
#' 
#' ## now we can read it again:
#' my.disk <- read.adf("test.adf")
#' print(my.disk)
#' 
#' ## and this is how you read it,
#' ## using a connection:
#' con <- file("test.adf", "rb")
#' my.disk2 <- read.adf(con)
#' close(con)
#' 
#' print(my.disk2)
#' 
#' ## Alternatively, you can work with ADZ files:
#' write.adz(adf.example, "test.adz")
#' my.disk3 <- read.adz("test.adz")
#' 
#' print(my.disk3)
#' }
#' @family io.operations
#' @author Pepijn de Vries
#' @export
setMethod("read.adf", "character", function(file){
  file <- file[[1]]
  con <- file(file, "rb")
  adf <- read.adf(con)
  close(con)
  return(adf)
})

#' @rdname read.adf
#' @aliases read.adf,ANY-method
#' @export
setMethod("read.adf", "ANY", function(file){
  con <- file
  if (!("connection" %in% class(con))) stop ("argument con is not a file connection!")
  con_info <- summary(con)
  if (!(con_info$text == "binary" && con_info$`can read` == "yes")) stop("Unsuitable connection provided. read.adf() requires a binary connection from which can be read.")

  dat <- readBin(con, "raw", NUMBER_OF_SECTORS_HD*NUMBER_OF_CYLINDERS*NUMBER_OF_SIDES*BLOCK_SIZE, endian = "big")
  tp <- NULL
  if (length(dat) == NUMBER_OF_SECTORS_DD*NUMBER_OF_CYLINDERS*NUMBER_OF_SIDES*BLOCK_SIZE) tp <- "DD"
  if (length(dat) == NUMBER_OF_SECTORS_HD*NUMBER_OF_CYLINDERS*NUMBER_OF_SIDES*BLOCK_SIZE) tp <- "HD"
  if (is.null(tp)) stop ("Unexpected file size.")
  adf <- methods::new("amigaDisk", data = dat, type = tp)
  
  test_byte <- readBin(con, "raw", 1, endian = "big")
  if (length(test_byte) > 0) stop ("Unexpected file size.")
  
  return(adf)
})

setGeneric("read.adz", function(file) standardGeneric("read.adz"))

#' @rdname read.adf
#' @name read.adz
#' @aliases read.adz,character-method
#' @export
setMethod("read.adz", "character", function(file){
  ## get some test files to test this... XXX
  file <- file[[1]]
  con <- gzfile(file, "rb")
  adf <- read.adf(con)
  close(con)
  return(adf)
})

setGeneric("write.adf", def = function(x, file){
  standardGeneric("write.adf")
})

#' Write an amigaDisk object to an ADF file
#'
#' Write an \code{\link{amigaDisk}} object to an Amiga Disk File (ADF) or
#' alternatively to an ADZ file.
#'
#' Use this function to write \code{\link{amigaDisk}} objects as binary
#' data to so-called Amiga Disk Files (ADF). These files can be used as
#' input for Amiga emulator software.
#' 
#' Alternatively, the object can be saved with 'write.adz', which is
#' essentially a gzipped version of an ADF file.
#'
#' @docType methods
#' @name write.adf
#' @rdname write.adf
#' @aliases write.adf,amigaDisk,ANY-method
#' @param x An \code{\link{amigaDisk}} object that needs to be saved to
#' an ADF file.
#' @param file either a file name to write to, or a file connection, that
#' allows to write binary data (see \code{\link[base]{file}}).
#' \code{write.adz} only accepts a file name.
#' @return Writes to an ADF file but returns nothing.
#'
#' @examples
#' \dontrun{
#' ## Let's write the example data to an ADF file:
#' data(adf.example)
#' 
#' ## Let's put it in the current working directory:
#' write.adf(adf.example, "test.adf")
#' 
#' ## You can also use file connections to do the same:
#' con <- file("test2.adf", "wb")
#' write.adf(adf.example, con)
#' close(con)
#' 
#' ## Last but not least the same object can be saved
#' ## as an adz file:
#' write.adz(adf.example, "test.3.adz")
#' }
#' @family io.operations
#' @author Pepijn de Vries
#' @export
setMethod("write.adf", c("amigaDisk", "ANY"), function(x, file) {
  con <- file
  if (!("connection" %in% class(con))) stop ("argument con is not a file connection!")
  con_info <- summary(con)
  if (!(con_info$text == "binary" && con_info$`can write` == "yes")) stop("Unsuitable connection provided. write.module() requires a connection to which binary data can be written.")
  
  writeBin(x@data, con, endian = "big")

  invisible()
})

#' @export
#' @rdname write.adf
#' @aliases write.adf,amigaDisk,character-method
setMethod("write.adf", c("amigaDisk", "character"), function(x, file) {
  con <- file(file, "wb")
  write.adf(x, con)
  close(con)
  invisible()
})

setGeneric("write.adz", def = function(x, file) standardGeneric("write.adz"))

##XXX
#' @rdname write.adf
#' @name write.adz
#' @aliases write.adz,amigaDisk,character-method
#' @export
setMethod("write.adz", c("amigaDisk", "character"), function(x, file) {
  con <- gzfile(file, "wb")
  write.adf(x, con)
  close(con)
  invisible()
})

setMethod("show", "amigaDisk", function(object) {
  print(object)
})

#' Print Amiga Disk File objects
#'
#' A method to print Amiga Disk File S4 class objects to the sink.
#'
#' @docType methods
#' @rdname print
#' @name print
#' @aliases print,amigaDisk-method
#'
#' @param x Either a \code{\link{amigaDisk}} or \code{\link{amigaBlock}} object.
#' @param ... further arguments passed to or from other methods
#' @return Returns nothing (\code{NULL}).
#'
#' @examples
#' data(adf.example)
#' 
#' print(adf.example)
#' @author Pepijn de Vries
#' @export
setMethod("print", "amigaDisk", function(x, ...){
  cat(paste("\nAmiga (", x@type ,") Disk File:\n", sep = ""))
  is.dos <- is.amigaDOS(x)
  cat(paste("\tType:\t\t\t" , c("Non-", "")[1 + is.bootable(x)], "bootable ", c("Non-DOS", "DOS")[1 + is.dos], "\n", sep = ""))
  if (is.dos) {
    ri <- root.info(x)
    bi <- boot.info(x)
    NUMBER_OF_SECTORS <- NUMBER_OF_SECTORS_DD
    if (x@type == "HD") NUMBER_OF_SECTORS <- NUMBER_OF_SECTORS_HD
    disk.full <- 100*(length(bitmap.info(x)) + 2)/(NUMBER_OF_SIDES*NUMBER_OF_CYLINDERS*NUMBER_OF_SECTORS)
    dir.cache <- bi$flag$dir.cache.mode
    intl <- bi$flag$intl.mode
    ## Directory cache mode is always used in combination with
    ## international mode:
    if (dir.cache) intl <- TRUE
    cat(paste("\tVolume name:\t\t" , ri$diskname, "\n", sep = ""))
    cat(sprintf("\tpercentage full:\t%0.1f%%\n", disk.full))
    cat(paste("\tFast File System:\t" , bi$flag$fast.file.system, "\n", sep = ""))
    cat(paste("\tInternational mode:\t" , intl, "\n", sep = ""))
    cat(paste("\tDirect cache mode:\t" , dir.cache, "\n", sep = ""))
  }
  return(invisible(NULL))
})

setGeneric("is.amigaDOS", function(x) standardGeneric("is.amigaDOS"))

#' Check if amigaDisk object is DOS formatted
#'
#' This method checks if there is a DOS file structure is present
#' on the \code{\link{amigaDisk}} object.
#'
#' Not all Amiga Disk Files have a DOS file structure on them.
#' This function checks if there is.
#'
#' @docType methods
#' @name  is.amigaDOS
#' @rdname is.amigaDOS
#' @aliases is.amigaDOS,amigaDisk-method
#' @param x An \code{\link{amigaDisk}} object for which
#' the check should be performed.
#' @return Returns a \code{logical} value, indicating whether the
#' disk is DOS formatted. When it is not, the attributes to the
#' returned value will contain information as to why the disk is
#' not DOS compatible.
#'
#' @examples
#' data(adf.example)
#' 
#' ## let's check if the example amigaDisk object
#' ## is DOS formatted:
#' 
#' is.amigaDOS(adf.example)
#' 
#' ## it apparently is
#' @author Pepijn de Vries
#' @export
setMethod("is.amigaDOS", "amigaDisk", function(x) {
  root.id  <- get.root.id(x)
  bb       <- boot.info(x)
  ri       <- NULL
  try(ri   <- root.info(x), T)
  result   <- T
  why      <- NULL
  notes    <- NULL
  if (bb$disk.type != "DOS") {
    result <- F
    why <- c(why, "Disk type is not labeled as 'DOS'")
  }
  if (x@data[[4]] > as.raw(0x05)) {
    result <- F
    why <- c(why, "Unknown or unsupported file system flags")
  }
  if (bb$rootblock != root.id) notes <- c(notes, "Uncommon pointer to rootblock")
  if (is.null(ri)) {
    result <- F
    why <- c(why, "Invalid root block")
  } else {
    if (length(ri$type) == 0 || ri$type != "T_HEADER") {
      result <- F
      why <- c(why, "Root block is not of type 'T_HEADER'")
    }
    if (ri$checksum != calculate.checksum(x, root.id, as.raw = F)) {
      result <- F
      why <- c(why, "Root block checksum failed")
    }
    if (length(ri$sec_type) == 0 || ri$sec_type != "ST_ROOT") {
      result <- F
      why <- c(why, "Root block not found")
    }
  }
  attributes(result)$why <- why
  attributes(result)$notes <- notes
  return (result)
})

setGeneric("is.bootable", function(x) standardGeneric("is.bootable"))

#' Check if amigaDisk object is bootable
#'
#' This function checks if the \code{\link{amigaDisk}}
#' object represents a bootable disk.
#'
#' The first two \code{\link{amigaBlock-class}} objects on a disk
#' are special and are called the boot block. The boot block will
#' determine whether an Amiga can boot from the disk.
#' 
#' This function will determine whether the Amiga would attempt
#' to execute the machine code present on the boot block. It will not
#' check whether it would be successful at that, as that would
#' require emulation of the Commodore Amiga system.
#'
#' @docType methods
#' @name  is.bootable
#' @rdname is.bootable
#' @aliases is.bootable,amigaDisk-method
#' @param x An \code{\link{amigaDisk}} object for which
#' the check should be performed.
#' @return Returns a \code{logical} value, indicating whether
#' the disk is bootable.
#'
#' @examples
#' data(adf.example)
#' 
#' ## let's check if the example amigaDisk object
#' ## is bootable:
#' 
#' is.bootable(adf.example)
#' 
#' ## it apparently is
#' @author Pepijn de Vries
#' @export
setMethod("is.bootable", "amigaDisk", function(x) {
  bb <- boot.info(x)
  result <- T
  why <- NULL
  notes <- NULL
  if (bb$disk.type != "DOS") {
    result <- F
    why <- c(why, "disk type is not labeled as 'DOS'")
  }
  if (bb$checksum != calculate.boot.checksum(x, as.raw = F)) {
    result <- F
    why <- c(why, "Boot block checksum failed")
  }
  attributes(result)$why <- why
  attributes(result)$notes <- notes
  return (result)
})

root.info <- function(x) {
  root.id    <- get.root.id(x)
  root       <- amigaBlock(x, root.id)
  ht_length  <- BLOCK_SIZE/4 - 56
#  r_days     <- rawToAmigaInt(root@data[ht_length*4 + 133:136], 32, F)
#  r_mins     <- rawToAmigaInt(root@data[ht_length*4 + 137:140], 32, F)
#  r_ticks    <- rawToAmigaInt(root@data[ht_length*4 + 141:144], 32, F)
#  r_datetime <- r_days*24*60*60 + r_mins*60 + r_ticks/50
#  r_datetime <- as.POSIXct(r_datetime, tz = "UTC", origin = "1978-01-01 00:00:00")
  r_datetime <- rawToAmigaDate(root@data[ht_length*4 + 133:144])
#  v_days     <- rawToAmigaInt(root@data[ht_length*4 + 185:188], 32, F)
#  v_mins     <- rawToAmigaInt(root@data[ht_length*4 + 189:192], 32, F)
#  v_ticks    <- rawToAmigaInt(root@data[ht_length*4 + 193:196], 32, F)
#  v_datetime <- v_days*24*60*60 + v_mins*60 + v_ticks/50
#  v_datetime <- as.POSIXct(v_datetime, tz = "UTC", origin = "1978-01-01 00:00:00")
  v_datetime <- rawToAmigaDate(root@data[ht_length*4 + 185:196])
#  c_days     <- rawToAmigaInt(root@data[ht_length*4 + 197:200], 32, F)
#  c_mins     <- rawToAmigaInt(root@data[ht_length*4 + 203:204], 32, F)
#  c_ticks    <- rawToAmigaInt(root@data[ht_length*4 + 205:208], 32, F)
#  c_datetime <- c_days*24*60*60 + c_mins*60 + c_ticks/50
#  c_datetime <- as.POSIXct(c_datetime, tz = "UTC", origin = "1978-01-01 00:00:00")
  c_datetime <- rawToAmigaDate(root@data[ht_length*4 + 197:208])
  name_len   <- rawToAmigaInt(root@data[ht_length*4 + 145], 8, F)
  name_len[name_len > 30] <- 30
  name_len[name_len < 1] <- 1
  result <- list(
    type       = TYPES$type[TYPES$value == rawToAmigaInt(root@data[1:4], 32, F)],
    headerkey  = rawToAmigaInt(root@data[5:8], 32, F),
    highseq    = rawToAmigaInt(root@data[9:12], 32, F),
    htsize     = rawToAmigaInt(root@data[13:16], 32, F),
    first_data = rawToAmigaInt(root@data[17:20], 32, F),
    checksum   = rawToAmigaInt(root@data[21:24], 32, F),
    ht         = unlist(lapply(1:ht_length, function(y) rawToAmigaInt(root@data[(y*4 + 21):(y*4 + 24)], 32, F))),
    bm_flag    = all(root@data[ht_length*4 + 25:28] == as.raw(c(0xff, 0xff, 0xff, 0xff))),
    bm_pages   = unlist(lapply(1:25, function(y) rawToAmigaInt(root@data[(ht_length*4 + y*4 + 25):(ht_length*4 + y*4 + 28)], 32, F))),
    bm_ext     = rawToAmigaInt(root@data[ht_length*4 + 129:132], 32, F),
    r_datetime = r_datetime,
    name_len   = name_len,
    diskname   = rawToChar(root@data[ht_length*4 + 146:(name_len + 145)]),
    unused1    = root@data[ht_length*4 + 176],
    unused2    = root@data[ht_length*4 + 177:184],
    v_datetime = v_datetime,
    c_datetime = c_datetime,
    next_hash  = rawToAmigaInt(root@data[ht_length*4 + 209:212], 32, F),
    parent_dir = rawToAmigaInt(root@data[ht_length*4 + 213:216], 32, F),
    extension  = rawToAmigaInt(root@data[ht_length*4 + 217:220], 32, F),
    sec_type   = SEC_TYPES$type[SEC_TYPES$value == rawToAmigaInt(root@data[ht_length*4 + 221:224], 32, F)]
  )
  result$diskname <- substr(result$diskname, 1, result$name_len)
  return(result)
}

## x = amigaDisk
header.info <- function(x, hash.table) {
  result <- lapply(hash.table, function(ht) {
    hblock     <- amigaBlock(x, ht)
    ht_length  <- BLOCK_SIZE/4 - 56
    days       <- rawToAmigaInt(hblock@data[ht_length*4 + 133:136], 32, F)
    mins       <- rawToAmigaInt(hblock@data[ht_length*4 + 137:140], 32, F)
    ticks      <- rawToAmigaInt(hblock@data[ht_length*4 + 141:144], 32, F)
    datetime   <- days*24*60*60 + mins*60 + ticks/50
    datetime   <- as.POSIXct(datetime, tz = "UTC", origin = "1978-01-01 00:00:00")
    name_len   <- rawToAmigaInt(hblock@data[ht_length*4 + 145], 8, F)
    name_len[name_len > 30] <- 30
    name_len[name_len < 1] <- 1
    header <- list(
      type       = TYPES$type[TYPES$value == rawToAmigaInt(hblock@data[1:4], 32, F)],
      header_key = rawToAmigaInt(hblock@data[5:8], 32, F),
      high_seq   = rawToAmigaInt(hblock@data[9:12], 32, F),
      data_size  = rawToAmigaInt(hblock@data[13:16], 32, F),
      first_data = rawToAmigaInt(hblock@data[17:20], 32, F),
      checksum   = rawToAmigaInt(hblock@data[21:24], 32, F),
      #bij alles ht_length optellen...
      datablocks = unlist(lapply(1:ht_length, function(y) rawToAmigaInt(hblock@data[(y*4 + 21):(y*4 + 24)], 32, F))),
      unused1    = rawToAmigaInt(hblock@data[ht_length*4 + 25:28], 32, F),
      UID        = rawToAmigaInt(hblock@data[ht_length*4 + 29:32], 32, F),
      GID        = rawToAmigaInt(hblock@data[ht_length*4 + 33:36], 32, F),
      # also add names to each individual flag
      protect    = as.logical(rawToBits(hblock@data[ht_length*4 + 33:36])),
      bytesize   = rawToAmigaInt(hblock@data[ht_length*4 + 37:40], 32, F),
      comm_len   = rawToAmigaInt(hblock@data[ht_length*4 + 41], 8, F),
      ## replace chardot with something better! XXX
      comment    = rawToCharDot(hblock@data[ht_length*4 + 42:120]),
      unused2    = hblock@data[ht_length*4 + 121:132],
      datetime   = datetime,
      name_len   = name_len,
      file_name  = rawToChar(hblock@data[ht_length*4 + 146:(145 + name_len)]),
      unused3    = hblock@data[ht_length*4 + 176],
      unused4    = hblock@data[ht_length*4 + 177:180],
      real_entry = rawToAmigaInt(hblock@data[ht_length*4 + 181:184], 32, F),
      next_link  = rawToAmigaInt(hblock@data[ht_length*4 + 185:188], 32, F),
      unused5    = hblock@data[ht_length*4 + 189:208],
      hash_chain = rawToAmigaInt(hblock@data[ht_length*4 + 209:212], 32, F),
      parent     = rawToAmigaInt(hblock@data[ht_length*4 + 213:216], 32, F),
      extension  = rawToAmigaInt(hblock@data[ht_length*4 + 217:220], 32, F),
      sec_type   = SEC_TYPES$type[SEC_TYPES$value == rawToAmigaInt(hblock@data[ht_length*4 + 221:224], 32, F)]
    )
    header$file_name <- substr(header$file_name, 1, header$name_len)
    # if (header$sec_type == "ST_USERDIR") {
    #   res <- NULL
    #   hash.tab.sub <- header$datablocks[header$datablocks != 0]
    #   ## XX dit nog beveiligen tegen oneindige loops
    #   ## loop through all hash chains
    #   while (T) {
    #     new.res <- header.info(x, hash.tab.sub)
    #     res <- c(res, new.res)
    #     hash.tab.sub <- unlist(lapply(new.res, function(x) x$hash_chain))
    #     hash.tab.sub <- hash.tab.sub[hash.tab.sub != 0]
    #     # When there are no more new hash.tables, break.
    #     if (length(hash.tab.sub) == 0) break
    #   }
    #   header$sub.dir <- res
    # }
    return(header)
  })
  return(result)
}

setGeneric("current.adf.dir<-", function(x, value) standardGeneric("current.adf.dir<-"))
setGeneric("current.adf.dir", function(x) standardGeneric("current.adf.dir"))

#' Get or set the current directory of an amigaDisk object
#'
#' Get or set the current directory of an \code{\link{amigaDisk}} object.
#'
#' By default the disk's root is stored as the current directory
#' for a new \code{\link{amigaDisk}} object. With this method, the
#' current directory can be retrieved or changed.
#' 
#' For this purpose the path should be specified conform Amiga DOS
#' syntax. Use the disk's name or "DF0" followed by a colon in order
#' to refer to the disk's root. Subdirectories are separated by forward
#' slashes ("/"). Colons and forward slashes are not allowed in file and
#' directory names. Both upper and lowercase letters are allowed in file
#' and directory names. The case is ignored when identifying files however.
#' This packages will NOT follow the Amiga's full search path
#' (\url{http://wiki.amigaos.net/wiki/AmigaOS_Manual:_AmigaDOS_Working_With_AmigaDOS#Search_Path}).
#'
#' @name  current.adf.dir
#' @rdname current.adf.dir
#' @aliases current.adf.dir,amigaDisk-method
#' @param x An \code{\link{amigaDisk}} object for which the current
#' directory needs to be obtained or changed.
#' @param value A \code{character} representation of the path, that
#' needs to be set as current directory. Use Amiga DOS syntax as
#' specified in the details
#' @return Returns a \code{character} representation of the current
#' directory.
#'
#' @examples
#' data(adf.example)
#' 
#' ## by default the current dir is the
#' ## disk's root. The disk name is
#' ## therefore shown when running
#' ## current.adf.dir for the provided
#' ## example data:
#' 
#' current.adf.dir(adf.example)
#' 
#' ## change the current dir:
#' current.adf.dir(adf.example) <- "DF0:this/is/a/deep/path"
#' 
#' ## confirm that it has changed:
#' current.adf.dir(adf.example)
#' 
#' ## let's set it back to the disk's root:
#' current.adf.dir(adf.example) <- "DF0:"
#' @author Pepijn de Vries
#' @export
setMethod("current.adf.dir", "amigaDisk", function(x){
  root.id    <- get.root.id(x)
  ri         <- root.info(x)
  dir        <- paste0(ri$diskname, ":")
  dir2       <- ""
  if (x@current.dir != root.id) {
    dir3 <- x@current.dir
    safeguard <- 0
    while (T) {
      hi <- header.info(x, dir3)[[1]]
      dir3 <- hi$parent
      dir2 <- paste0(hi$file_name, "/", dir2)
      if (dir3 %in% c(0, root.id)) break
      safeguard <- safeguard + 1
      if (safeguard > NUMBER_OF_SECTORS_HD*NUMBER_OF_CYLINDERS*NUMBER_OF_SIDES) stop("Seems like I'm stuck in an infinite loop.")
    }
  }
  return(paste0(dir, dir2))
})

#' @rdname current.adf.dir
#' @name current.adf.dir<-
#' @aliases current.adf.dir<-,amigaDisk,character-method
#' @export
setReplaceMethod("current.adf.dir", c("amigaDisk", "character"), function(x, value){
  value <- value[[1]]
  header.id <- find.file.header(x, value)
  hi        <- header.info(x, header.id)[[1]]
  if (!(hi$sec_type %in% c("ST_USERDIR", "ST_ROOT"))) stop ("value doesn't specify a directory")
  x@current.dir <- as.integer(header.id)
  methods::validObject(x)
  return(x)
})

setGeneric("list.adf.files", function(x, path) standardGeneric("list.adf.files"))

#' List files in an amigaDisk directory
#'
#' Get a list of files in a specific directory on a virtual
#' \code{\link{amigaDisk}}.
#'
#' As an analogue of \code{\link[base]{list.files}}, this method
#' list files in a specific directory. But in this case the files
#' are located on a virtual floppy disk represented by the
#' \code{\link{amigaDisk}} object. This works only for DOS-formatted
#' (\code{\link{is.amigaDOS}}) virtual disks.
#'
#' @name  list.adf.files
#' @rdname list.adf.files
#' @aliases list.adf.files,amigaDisk,missing-method
#' @param x An \code{\link{amigaDisk}} object for which the files
#' should be listed.
#' @param path Specify the path on the \code{\link{amigaDisk}}
#' object, conform Amiga specs, for which files should be listed.
#' See \code{\link{current.adf.dir}} for details on these specs.
#' @return Returns a \code{vector} of \code{character}s listing
#' the files in the specified directory on the virtual disk.
#'
#' @examples
#' data(adf.example)
#' 
#' ## show all files in the root of the example
#' ## disk file:
#' list.adf.files(adf.example)
#' 
#' ## you can also list the files in a specified
#' ## directory:
#' list.adf.files(adf.example, "DF0:mods")
#' 
#' ## For the same path, only now specified
#' ## relatively to the current directory:
#' list.adf.files(adf.example, "mods")
#' 
#' @author Pepijn de Vries
#' @export
setMethod("list.adf.files", c("amigaDisk", "missing"), function(x, path){
  fi <- file.info(x, x@current.dir)
  result <- unlist(lapply(fi, function(x) x$file_name))
  if (is.null(result)) result <- character(0)
  return(result)
})

#' @rdname list.adf.files
#' @aliases list.adf.files,amigaDisk,character-method
#' @export
setMethod("list.adf.files", c("amigaDisk", "character"), function(x, path){
  header.id <- find.file.header(x, path)  
  fi <- file.info(x, header.id)
  unlist(lapply(fi, function(x) x$file_name))
})

setGeneric("get.adf.file", function(x, source, destination) standardGeneric("get.adf.file"))

#' Get a file from an amigaDisk object
#'
#' Get files stored on virtual \code{\link{amigaDisk}}s as raw data
#' or copy as file.
#'
#' Amiga DOS formatted disks can store any kind of file (as long
#' as the disk's capacity allows it). Use this method to extract
#' such files embedded in an Amiga Disk File (ADF) as raw data or
#' copy to a file on your system.
#'
#' @name  get.adf.file
#' @rdname get.adf.file
#' @aliases get.adf.file,amigaDisk,character,missing-method
#' @param x An \code{\link{amigaDisk}} object from which a file
#' needs to be extracted.
#' @param source Specify the source file's path on the
#' \code{\link{amigaDisk}} object, conform Amiga specs. See
#' \code{\link{current.adf.dir}} for details on these specs.
#' @param destination either a file name or a file connection, that
#' allows writing binary data (see e.g., \code{\link[base]{file}} or
#' \code{\link[base]{url}}).
#' @return Returns a \code{vector} of \code{raw} data when the
#' argument \code{destination} is missing. Otherwise returns nothing.
#'
#' @examples
#' data(adf.example)
#'
#' \dontrun{
#' ## get the file "Startup-Sequence" from the virtual
#' ## example disk and save as a text file in the
#' ## current working directory:
#' get.adf.file(adf.example, "DF0:S/Startup-Sequence", "startup.txt")
#' }
#' 
#' ## get the same file as raw data
#' ## by omitting the destination:
#' startup <- get.adf.file(adf.example, "DF0:S/Startup-Sequence")
#' 
#' ## Look, it's a text file:
#' cat(rawToChar(startup))
#' 
#' if (requireNamespace("ProTrackR", quietly = TRUE)) {
#'   ## look there is a typical ProTracker module on
#'   ## the example disk. You can load it like this:
#'   
#'   ## get the file from the virtual disk
#'   ## as raw data
#'   mod.raw <- get.adf.file(adf.example, "DF0:mods/mod.intro")
#'
#'   ## open a raw connection with the
#'   ## newly imported raw data
#'   con <- rawConnection(mod.raw, "rb")
#'
#'   ## and read it as a ProTracker module
#'   mod <- ProTrackR::read.module(con)
#'   close(con)
#'   
#'   ## plot the first sample from the module:
#'   plot(ProTrackR::waveform(ProTrackR::PTSample(mod, 1)),
#'        type = "l", ylab = "amplitude")
#' } else {
#'   cat("You need to install and load the\nProTrackR package for this part of the example.")
#' }
#' @author Pepijn de Vries
#' @export
setMethod("get.adf.file", c("amigaDisk", "character", "missing"), function(x, source, destination) {
  .get.adf.file(x, source)
})

#' @rdname get.adf.file
#' @aliases get.adf.file,amigaDisk,character,character-method
#' @export
setMethod("get.adf.file", c("amigaDisk", "character", "character"), function(x, source, destination) {
  .get.adf.file(x, source, destination)
})

#' @rdname get.adf.file
#' @aliases get.adf.file,amigaDisk,character,ANY-method
#' @export
setMethod("get.adf.file", c("amigaDisk", "character", "ANY"), function(x, source, destination) {
  .get.adf.file(x, source, destination)
})

.get.adf.file <- function(x, source, destination) {
  if (!is.amigaDOS(x)) stop("Not a DOS disk!")
  bi <- boot.info(x)
  ffs <- bi$flag$fast.file.system
  hi <- header.info(x, find.file.header(x, source))[[1]]
  if (hi$sec_type != "ST_FILE") stop( "Provided path does not refer to a file.")
  filesize <- hi$bytesize
  db <- rev(hi$datablocks[hi$datablocks != 0])
  #hi$datablocks
  #hi$bytesize/BLOCK_SIZE
  #hi$hash_chain
  while (hi$extension != 0) {
    hi <- header.info(x, hi$extension)[[1]]
    db <- c(db, rev(hi$datablocks[hi$datablocks != 0]))
  }
  dat <- lapply(db, function(db.i) {
    if (ffs) {
      amigaBlock(x, db.i)@data
    } else {
      # first 24 bytes of a data block contains
      # meta-data on the old file system... So
      # skip those
      amigaBlock(x, db.i)@data[25:512]
    }
  })
  dat <- do.call(c, dat)[0:filesize]
  if (missing(destination)) {
    return(dat)
  } else {
    if (class(destination)[[1]] == "character") {
      destination <- destination[[1]]
      con <- file(destination, "wb")
      writeBin(dat, con)
      close(con)
    } else {
      if (!("connection" %in% class(destination))) stop ("argument destination is not a file connection!")
      con_info <- summary(destination)
      if (!(con_info$text == "binary" && con_info$`can write` == "yes")) stop("Unsuitable connection provided. get.adf.file() requires a binary connection to which data can be written.")
      writeBin(dat, destination)
    }
    return (invisible(NULL))
  }
}

setGeneric("adf.disk.name", function(x) standardGeneric("adf.disk.name"))
setGeneric("adf.disk.name<-", function(x, value) standardGeneric("adf.disk.name<-"))

#' Get or set the disk name of an amigaDisk object
#'
#' Get or set the disk name of an \code{\link{amigaDisk}} object.
#'
#' DOS-formatted disks (\code{\link{is.amigaDOS}}) store their disk
#' name on the socalled root block of the disk. This method allows
#' you to obtain the disk's name or change it (when it is DOS-formatted).
#'
#' @name  adf.disk.name
#' @rdname adf.disk.name
#' @aliases adf.disk.name,amigaDisk-method
#' @param x An \code{\link{amigaDisk}} object for which the disk
#' name needs to be obtained or changed.
#' @param value A \code{character} representation with which the
#' disk's name needs to be replaced. Disk name needs to be between
#' 1 and 30 characters long and are not allowed to contain a colon
#' or forward slash.
#' @return Returns A \code{character} representation of the disk's
#' name.
#'
#' @examples
#' \dontrun{
#' data(adf.example)
#' 
#' ## get the disk name:
#' adf.disk.name(adf.example)
#' 
#' ## change it if you don't like it:
#' adf.disk.name(adf.example) <- "MyDisk"
#' 
#' ## confirm that it has changed:
#' adf.disk.name(adf.example)
#' }
#' @author Pepijn de Vries
#' @export
setMethod("adf.disk.name", "amigaDisk", function(x) {
  if (!is.amigaDOS(x)) stop("x is not a DOS formatted disk!")
  ri <- root.info(x)
  return(ri$diskname)
})

#' @rdname adf.disk.name
#' @name adf.disk.name<-
#' @aliases adf.disk.name<-,amigaDisk,character-method
#' @export
setReplaceMethod("adf.disk.name", c("amigaDisk", "character"), function(x, value){
  value <- value[[1]]
  if (nchar(value) == 0) stop("Name should be at least 1 character long.")
  if (nchar(value) > 30) {
    warning("Provided name is too long. It will be truncated.")
    value <- substr(value, 1, 30)
  }
  if (grepl("[:]|[/]", value)) stop("Disk name is not allowed to contain characters ':' or '/'.")
  value <- charToRaw(value)
  ht_length  <- BLOCK_SIZE/4 - 56
  root.id <- get.root.id(x)
  rblock  <- amigaBlock(x, root.id)
  rblock@data[ht_length*4 + 145] <- amigaIntToRaw(length(value), 8, F)
  rblock@data[ht_length*4 + 146:(length(value) + 145)] <- value
  amigaBlock(x, root.id) <- rblock
  # update the checksum:
  rblock@data[21:24] <- calculate.checksum(x, root.id, as.raw = T)
  amigaBlock(x, root.id) <- rblock
  return(x)
})

## function to find file header based on file name
find.file.header <- function(x, filename) {
  if (filename == "") return(x@current.dir)
  root.id  <- get.root.id(x)
  boot     <- boot.info(x)
  intl     <- boot$flag$intl.mode
  if (boot$flag$dir.cache.mode) intl <- T
  cur.dir  <- x@current.dir
  fun <- function(x, b = intl) intl_toupper(x, b)
  diskname <- fun(adf.disk.name(x))
  if (is.null(filename)) return (root.id)
  filename <- fun(as.character(filename))
  result <- lapply(filename, function(f) {
    # split device name from rest of path:
    hasdevname <- grepl(":", f, fixed = T)
    f <- unlist(strsplit(f, ":", fixed = T))
    if (length(f) > 2) stop("Multiple colons in file name.")
    if (length(f) == 1) {
      if (hasdevname && f != "DF0" && f != diskname) stop("Unknown devicename")
      if (hasdevname && (f == "DF0" || f == diskname)) return(root.id)
      path <- unlist(strsplit(f, "/", fixed=T))
      if (any(diff(which(path != "")) > 1)) stop("unexpected double slashes in path")
      # number of branches we should go up in the dir tree
      ## loop the number of times we should go down:
      count.up <- sum(path == "")
      j <- 0
      while (j != count.up) {
        if (cur.dir == root.id) stop("Can't go further down the directory tree than the disk's root.")
        info <- header.info(x, cur.dir)
        cur.dir <- info[[1]]$parent
        j <- j + 1
      }
      path <- path[path != ""]
    } else {
      if (f[[1]] != "DF0" && f[[1]] != diskname) stop("Unknown devicename")
      f <- f[[2]]
      cur.dir <- root.id
      path  <- unlist(strsplit(f, "/", fixed = T))
      if (any(path == "")) stop ("unexpected double slashes in path")
    }
    lapply(path, function(pt) {
      header <- header.info(x, cur.dir)[[1]]
      if (header$sec_type == "ST_ROOT" || header$sec_type == "ST_USERDIR") {
        block <- header$datablocks[hash.name(pt, intl) + 1]
      } else {
        block <- header$header_key
      }
      # go throught hash chain
      safeguard <- 0
      while (T) {
        hi <- header.info(x, block)[[1]]
        if (fun(hi$file_name) == fun(pt)) break
        if (hi$hash_chain == 0) break
        block <- hi$hash_chain
        safeguard <- safeguard + 1
        if (safeguard > NUMBER_OF_SECTORS_HD*NUMBER_OF_CYLINDERS*NUMBER_OF_SIDES) stop("Seems like I'm stuck in an infinite loop.")
      }
      if (fun(hi$file_name) == fun(pt)) {
        cur.dir <<- hi$header_key
      } else {
        stop("File/Path not found...")
      }
    })
    return(cur.dir)
  })
  return (unlist(result))
}

setGeneric("blank.amigaDOSDisk", function(diskname,
                                          disktype      = c("DD", "HD"),
                                          filesystem    = c("OFS", "FFS"),
                                          international = F,
                                          dir.cache     = F,
                                          bootable      = T,
                                          creation.date = Sys.time()) standardGeneric("blank.amigaDOSDisk"))

#' Create blank disk with file system
#'
#' Create a virtual blank DOS formatted floppy disk with a file system on it.
#'
#' Creates a blank \code{link{amigaDisk}} object. This method differs
#' from the object constructor (\code{new("amigaDisk")}) because it also
#' installs a file system on the disk. The blank disk can thus be used to
#' write files onto, and is also usable in Amiga emulators. For use in
#' emulators, the object needs to be saved with the \code{\link{write.adf}}
#' method.
#' 
#' @name  blank.amigaDOSDisk
#' @rdname blank.amigaDOSDisk
#' @aliases blank.amigaDOSDisk,character-method
#' @param diskname A \code{character} string of the desired disk name.
#' Disk name should be between 1 and 30 characters long, and should
#' not contain any colon or forward slash characters.
#' @param disktype Either "\code{DD}" (double density, most common and
#' therefore default) or "\code{HD}" (high denisity). The type of disk
#' the blank disk should represent.
#' @param filesystem Either "\code{OFS}" (old file system) or "\code{FFS}"
#' (fast file system). \code{FFS} is not compatible with Amiga OS <2.0.
#' On the original system, the FFS was slightly faster and can requires
#' less data for the file system. It is however less robust: on corrupt
#' disks, file recovery is more difficult.
#' @param international The international mode was introduced in Amiga
#' OS 2.0. In lower versions, international characters were mistakenly
#' not converted to uppercase when comparing file names. The international
#' mode (set this argument to \code{TRUE}) corrects this mistake.
#' The international mode is not compatible with Amiga OS <2.0.
#' @param dir.cache The directory cache mode (set this argument to
#' \code{TRUE}) was introduced with Amiga OS 3.0 (and is not compatible
#' with lower versions). On real machines this allowed for slightly faster
#' directory listing (but costs disk space). The directory cache mode is
#' always used in combination with the 'international mode'.
#' @param bootable When this argument is set to \code{TRUE}. Minimal
#' executable code is added to the bootblock. This code will open the
#' command line interface when the disk is used to boot the system. In
#' Amiga OS >2.0, the 'Startup-Sequence' file needs to be present
#' for this, otherwise the screen will remain black on booting. See also the
#' \code{\link{boot.block.code}} data.
#' @param creation.date A \code{\link[base]{POSIXt}} object. Will be used
#' and stored as the creation date of the virtual disk. Note that the Amiga
#' does not store the time zone and UTC is assumed as default. The Amiga
#' stores the date and time as positive integers, relative to 1st of
#' January in 1978. As a result, dates before that are not allowed.
#' @return Returns a blank \code{\link{amigaDisk}} object with a file
#' system installed on it.
#' @examples
#' ## Create a blank virtual disk compatible with
#' ## Amiga OS 1.x and up (Note that spaces in file and
#' ## disk names are allowed but not recommended):
#' disk.os1x <- blank.amigaDOSDisk(diskname = "I'm_OS_1.x_compatible",
#'                                 disktype = "DD",
#'                                 filesystem = "OFS",
#'                                 international = FALSE,
#'                                 dir.cache = FALSE,
#'                                 bootable = TRUE)
#' 
#' ## create a disk that is compatable with OS 2.x and up
#' ## (no backward compatability):
#' disk.os2x <- blank.amigaDOSDisk(diskname = "I'm_OS_2.x_compatible",
#'                                 disktype = "DD",
#'                                 filesystem = "FFS",
#'                                 international = TRUE,
#'                                 dir.cache = FALSE,
#'                                 bootable = TRUE)
#' 
#' ## create a disk that is compatable with OS 3.x and up
#' ## (no backward compatability):
#' disk.os3x <- blank.amigaDOSDisk(diskname = "I'm_OS_3.x_compatible",
#'                                 disktype = "DD",
#'                                 filesystem = "FFS",
#'                                 international = TRUE,
#'                                 dir.cache = TRUE,
#'                                 bootable = TRUE)
#' @author Pepijn de Vries
#' @export
setMethod("blank.amigaDOSDisk", "character",
          function(diskname,
                   disktype,
                   filesystem,
                   international,
                   dir.cache,
                   bootable,
                   creation.date) {
            ## It seems that OS >=3 does not allow intl=T when dir.cache=T
            disktype      <- match.arg(disktype, c("DD", "HD"))
            filesystem    <- match.arg(filesystem, c("OFS", "FFS"))
            filesystem    <- filesystem == "FFS"
            international <- as.logical(international[[1]])
            dir.cache     <- as.logical(dir.cache[[1]])
            root.id       <- get.root.id(disktype)

            if (dir.cache && !international) stop ("International mode should be explicitly set to TRUE when directory cache mode is set to TRUE.")
            ## when directory cache mode is set to TRUE,
            ## this is only allowed in 'international mode'.
            ## The flag for the international mode is turned
            ## off however:
            if (dir.cache) international <- FALSE
            
            ######################################################
            ##  1: MAKE A BOOTBLOCK:
            ######################################################
            boot <- charToRaw("DOS")
            boot <- c(boot, packBits(rev(c(rep(F, 5),
                                       dir.cache,
                                       international,
                                       filesystem))),
                      raw(4),
                      amigaIntToRaw(root.id, 32))
            if (bootable) boot <- c(boot, unlist(adfExplorer::boot.block.code$assembled))
            boot <- c(boot, raw(BLOCK_SIZE - length(boot)))
            checksum <- calculate.boot.checksum.dat(boot)
            boot[5:8] <- checksum
            boot <- methods::new("amigaBlock", data = boot)
            disk <- methods::new("amigaDisk")
            amigaBlock(disk, 0) <- boot

            ######################################################
            ##  2: MAKE A ROOTBLOCK:
            ######################################################

            create <- amigaDateToRaw(creation.date)
            ht_length  <- BLOCK_SIZE/4 - 56
            
            rblock <- c(TYPES$value[TYPES$type == "T_HEADER"],
                        rep(0, 2),            # unused
                        ht_length,            # hash table size
                        0,                    # unused
                        0,                    # checksum
                        rep(0, ht_length),    # hash table
                        0x100000000 - 1,      # bitmap flag (valid or not)
                        root.id + 1,          # bitmap pointer
                        rep(0, 25),           # bitmap pointers (for flopy disk one bitmapblock is sufficient)
                        rep(0,3),             # r_date
                        16777216,             # disk name size (temp) 0x01410000
                        rep(0, 9),            # disk name blanks and unused
                        rep(0, 8),            # v_date and c_date, next hash and parent_dir
                        ifelse(dir.cache, root.id + 2, 0), # extension (FFS, dir cache)
                        SEC_TYPES$value[SEC_TYPES$type == "ST_ROOT"])
            rblock <- amigaIntToRaw(rblock, 32)
            rblock[ht_length*4 + 133:144] <- create
            rblock[ht_length*4 + 185:196] <- create
            rblock[ht_length*4 + 197:208] <- create
            rblock[21:24] <- calculate.checksum(rblock)
            rblock <- methods::new("amigaBlock", data = c(rblock, raw(BLOCK_SIZE - length(rblock))))
            amigaBlock(disk, root.id) <- rblock
            adf.disk.name(disk) <- diskname

            ######################################################
            ##  3: MAKE A BITMAP BLOCK:
            ######################################################

            NUMBER_OF_SECTORS <- ifelse(disktype == "DD", NUMBER_OF_SECTORS_DD, NUMBER_OF_SECTORS_HD)
            bitmap <- rep(T, NUMBER_OF_CYLINDERS*NUMBER_OF_SIDES*NUMBER_OF_SECTORS)
            ## Set root and bitmap block to false
            bitmap[root.id + 0:1 - 1] <- F # +1 to compensate for different index base; -2 to skip boot blocks
            if (dir.cache) bitmap[root.id + 1] <- F
            bitmap <- bitmapToRaw(bitmap)
            bitmap <- c(raw(4), bitmap, raw(BLOCK_SIZE - length(bitmap) - 4))
            bitmap[1:4] <- calculate.checksum(bitmap, chcksm.pos = 1)
            bmblock <- methods::new("amigaBlock", data = bitmap)
            amigaBlock(disk, root.id + 1) <- bmblock

            ######################################################
            ##  4: MAKE A DIRECT CACHE BLOCK:
            ######################################################
            if (dir.cache) {
              dc <- c(
                TYPES$value[TYPES$type == "DIRCACHE"], # TYPE
                root.id + 2,           # self pointer
                root.id,               # cached dir (root)
                0,                     # entries number
                0,                     # next direct cache
                0,                     # checksum
                rep(0, 122)            # padded with zeros
              )
              dcblock <- amigaIntToRaw(dc, 32)
              dcblock[21:24] <- calculate.checksum(dcblock)
              dcblock <- methods::new("amigaBlock", data = dcblock)
              amigaBlock(disk, root.id + 2) <- dcblock
            }
            return (disk)
          })
