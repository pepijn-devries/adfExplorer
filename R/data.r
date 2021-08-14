#' An example of an amigaDisk object
#'
#' An example of an \code{\link{amigaDisk-class}} object.
#'
#' An \code{\link{amigaDisk-class}} object that represents an
#' 'Old File System' formatted and bootable disk. It is used in
#' multiple examples of this package. It contains a directory
#' structure and some files that can be accessed using this package.
#' The content of this example might change in future versions.
#'
#' @docType data
#' @name adf.example
#' @format An S4 \code{\link{amigaDisk-class}} object.
#' @examples
#' data("adf.example")
NULL

#' Minimal executable code for a bootblock
#'
#' A minimal piece of code required to boot to a command line
#' interface on a Commodore Amiga.
#'
#' The first two blocks (\code{\link{amigaDisk}}) are special and are
#' called the boot block. This block should contain information on the
#' type of disk and possibly some executable code that will be run
#' at boot time. This \code{data.frame} contains some minimal executable
#' code that will start the Amiga command line interface. On Amiga OS
#' >=2.0 the screen will stay black unless a startup-sequence file is
#' present on the disk.
#' 
#' The original code is from Thomas Kessler as published by Laurent
#' Cl\ifelse{latex}{\out{{\'{e}}}}{\ifelse{html}{\out{&eacute;}}{e}}vy
#' (\url{http://lclevy.free.fr/adflib/adf_info.html}).
#'
#' @docType data
#' @name boot.block.code
#' @format A data frame with two columns. The first column
#' contains the assembled code (as \code{raw} data). The
#' second column contains the corresponding Motorola 68000
#' (the main CPU of the original Commodore Amiga) assembly
#' syntax.
#' @examples
#' data("boot.block.code")
#' 
#' ## To create a basic boot block for a DD disk:
#' bblock <- new("amigaBlock", data =
#'   c(as.raw(c(0x44, 0x4F, 0x53, 0x00, 0xE3, 0x3D, 0x0E, 0x73,
#'   0x00, 0x00, 0x03, 0x70)), unlist(boot.block.code$assembled),
#'   raw(419))
#' )
#' ## The raw data preceding the executable code are
#' ## a label, flags and a checksum.
NULL
