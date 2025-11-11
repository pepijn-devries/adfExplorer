# Compress ADF to ADZ files and vice versa

The ADZ format is essentially a compressed (gzip) version of the Amiga
Disk File (ADF) format. The `adfExplorer` allows you to connect to both
formats. However, you can only open a 'read-only' connection to ADZ
files. Use the compression and decompression functions documented here
to move back and forth from and to ADF and ADZ formats.

## Usage

``` r
compress_adf(source, destination)

decompress_adz(source, destination)
```

## Arguments

- source:

  Path to the source file to read.

- destination:

  Path to the destination file to write.

## Value

Returns `NULL` invisibly.

## Author

Pepijn de Vries

## Examples

``` r
adz_file  <- system.file("example.adz", package = "adfExplorer")
adf_file  <- tempfile(fileext = ".adf")
adz_file2 <- tempfile(fileext = ".adz")

decompress_adz(adz_file, adf_file)
compress_adf(adf_file, adz_file2)
```
