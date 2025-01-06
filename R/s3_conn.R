#' @export
summary.adf_file_con <- function(object, ...) {
  adf_con_summary(object)
}

#' @export
close.adf_file_con <- function(con, ...) {
  adf_close_file_con(con) |> invisible()
}

#' @export
close.adf_device <- function(con, ...) {
  close_adf(con) |> invisible()
}

#' @export
seek.adf_file_con <- function(con, where = NA, origin = "start", ...) {
  
  origin <- pmatch(origin, c("start", "current", "end"))
  seek_adf(con, as.numeric(where), origin)
}

#' Transfer binary data to and from connections
#' 
#' These methods mask the identical functions in the `base` package (see [base::readBin()],
#' [base::readLines()], [base::readChar()], [base::writeBin()], [base::writeLines()] and
#' [base::writeChar()]. They behave exactly the
#' same as their base counterpart, with the exception that they can read and write to connections
#' opened with `adf_file_con()`.
#' @param con A connection to a file on a virtual ADF device. Such a connection can
#' be established with `adf_file_con()`.
#' @inheritParams base::readBin
#' @inheritParams base::readLines
#' @inheritParams base::readChar
#' @inheritParams base::writeBin
#' @inheritParams base::writeLines
#' @inheritParams base::writeChar
#' @returns Returns `NULL` invisibly
#' @rdname read_write
#' @export
readBin <-
  function(con, what, n = 1L, size = NA_integer_, signed = TRUE,
           endian = .Platform$endian) {
    UseMethod("readBin")
  }

#' @rdname read_write
#' @export
readBin.default <-
  function(con, what, n = 1L, size = NA_integer_, signed = TRUE,
           endian = .Platform$endian) {
    base::readBin(con, what, n, size, signed, endian)
  }

#' @rdname read_write
#' @export readBin.adf_file_con
#' @export
readBin.adf_file_con <-
  function(con, what, n = 1L, size = NA_integer_, signed = TRUE,
           endian = .Platform$endian) {
    w <- c("numeric", "double", "integer", "int", "logical",
           "complex", "character", "raw")
    if(!is.character(what) || is.na(what) ||
       length(what) != 1L ||
       !any(what == w))
      what <- typeof(what)
    sz  <- adf_readbin_size(pmatch(what, w), size)
    dat <- adf_file_read_ext(con, ifelse(is.na(sz), 1L, sz) * n)
    readBin.default(dat, what, n, size, signed, endian)
  }

#' @rdname read_write
#' @export
readLines <-
  function(con, n = -1L, ok = TRUE, warn = TRUE,
           encoding = "unknown", skipNul = FALSE) {
    UseMethod("readLines")
  }

#' @rdname read_write
#' @export
readLines.default <-
  function(con = stdin(), n = -1L, ok = TRUE, warn = TRUE,
           encoding = "unknown", skipNul = FALSE) {
    base::readLines(con, n, ok, warn, encoding, skipNul)
  }

#' @rdname read_write
#' @export readLines.adf_file_con
#' @export
readLines.adf_file_con <-
  function(con, n = -1L, ok = TRUE, warn = TRUE,
           encoding = "unknown", skipNul = FALSE) {
    adf_readlines(con, n, ok, warn, encoding, skipNul)
  }

#' @rdname read_write
#' @export
writeBin <-
  function(object, con, size = NA_integer_, endian = .Platform$endian,
           useBytes = FALSE) {
    UseMethod("writeBin", con)
  }

#' @rdname read_write
#' @export
writeBin.default <-
  function(object, con, size = NA_integer_, endian = .Platform$endian,
           useBytes = FALSE) {
    base::writeBin(object, con, size, endian, useBytes)
  }

#' @rdname read_write
#' @export writeBin.adf_file_con
#' @export
writeBin.adf_file_con <-
  function(object, con, size = NA_integer_, endian = .Platform$endian,
           useBytes = FALSE) {
    if (!endian %in% c("big", "little", "swap"))
      stop("invalid 'endian' argument")
    swap <- endian != .Platform$endian
    if(!is.vector(object) || mode(object) == "list")
      stop("can only write vector objects")
    adf_writebin(object, con, size, swap, useBytes)
  }

#' @rdname read_write
#' @export
writeLines <-
  function(text, con, sep = "\n", useBytes = FALSE) {
    UseMethod("writeLines", con)
  }

#' @rdname read_write
#' @export
writeLines.default <-
  function(text, con = stdout(), sep = "\n", useBytes = FALSE) {
    base::writeLines(text, con, sep, useBytes)
  }

#' @rdname read_write
#' @export writeLines.adf_file_con
#' @export
writeLines.adf_file_con <-
  function(text, con = stdout(), sep = "\n", useBytes = FALSE) {
    if(!is.character(text))
      stop("can only write character objects")
    adf_writelines(text, con, sep, useBytes) |> invisible()
  }
