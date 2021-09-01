setGeneric("adf.file.remove", function(x, file, full) standardGeneric("adf.file.remove"))

#' Remove a file from an amigaDisk object
#'
#' Remove a file from a virtual Amiga floppy disk represented by
#' an \code{\link{amigaDisk}} object.
#'
#' TODO
#'
#' @docType methods
#' @name adf.file.remove
#' @rdname adf.file.remove
#' @aliases adf.file.remove,amigaDisk,character,logical-method
#' @param x An \code{\link{amigaDisk}} onto which the file should be put.
#' @param file TODO
#' @param full TODO
#' @return Returns an \code{\link{amigaDisk}} object where the
#' specified \code{file} is removed.
#' @examples
#' \dontrun{
#' ## TODO
#' resulting.disk <- adf.file.remove(adf.example, "df0:mods/mod.intro")
#' }
#' @author Pepijn de Vries
#' @export
setMethod("adf.file.remove", c("amigaDisk", "character", "logical"), function(x, file, full) {
  browser() ## TODO
  ## Step 1 check if 'file' is a file or a directory, and whether it exists
  fh <- find.file.header(x, file)
  fi <- header.info(x, fh)
  
  ## Check if the disk uses directory caching:
  #TODO implement
  stop("This method is not yet implemented!")
})

#' @export
setMethod("adf.file.remove", c("amigaDisk", "character", "missing"), function(x, file) {
  adf.file.remove(x, file, F)
})
