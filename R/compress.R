#' Compress ADF to ADZ files and vice versa
#' 
#' The ADZ format is essentially a compressed (gzip) version of the
#' Amiga Disk File (ADF) format. The `adfExplorer` allows you to connect
#' to both formats. However, you can only open a 'read-only' connection
#' to ADZ files. Use the compression and decompression functions documented
#' here to move back and forth from and to ADF and ADZ formats.
#' @param source Path to the source file to read.
#' @param destination Path to the destination file to write.
#' @returns Returns `NULL` invisibly.
#' @examples
#' adz_file  <- system.file("example.adz", package = "adfExplorer")
#' adf_file  <- tempfile(fileext = ".adf")
#' adz_file2 <- tempfile(fileext = ".adz")
#' 
#' decompress_adz(adz_file, adf_file)
#' compress_adf(adf_file, adz_file2)
#' @author Pepijn de Vries
#' @rdname compress
#' @export
compress_adf <- function(source, destination) {
  .convert_adf(source, destination, function(x) x, gzcon)
}

#' @rdname compress
#' @export
decompress_adz <- function(source, destination) {
  .convert_adf(source, destination, gzcon, function(x) x)
}

.convert_adf <- function(source, destination, s_fun, d_fun) {
  con_in  <- s_fun(file(source, "rb"))
  con_out <- d_fun(file(destination, "wb"))
  chunk_size <- 1024*1024
  tryCatch({
    repeat {
      dat <- readBin(con_in, "raw", chunk_size)
      if (length(dat) < 1) break else {
        writeBin(dat, con_out)
      }
    }
  }, finally = {
    close(con_in)
    close(con_out)
  })
  invisible(NULL)
}
