# Open a connection to a file on a virtual ADF device

Open a connection to a file on a virtual ADF device. The created
connection (if valid) should be accepted by any R function that reads
from or writes to a connection, such as
[`readLines()`](https://pepijn-devries.github.io/adfExplorer/reference/read_write.md),
[`writeLines()`](https://pepijn-devries.github.io/adfExplorer/reference/read_write.md),
[`readBin()`](https://pepijn-devries.github.io/adfExplorer/reference/read_write.md),
[`writeBin()`](https://pepijn-devries.github.io/adfExplorer/reference/read_write.md),
etc.

## Usage

``` r
adf_file_con(x, ..., writable = FALSE)

# S3 method for class 'adf_device'
adf_file_con(x, path, ..., writable = FALSE)

# S3 method for class 'character'
adf_file_con.adf_device(x, path, ..., writable = FALSE)

# S3 method for class 'virtual_path'
adf_file_con(x, ..., writable = FALSE)
```

## Arguments

- x:

  Either a connection to a virtual ADF device created with
  [`connect_adf()`](https://pepijn-devries.github.io/adfExplorer/reference/connect_adf.md),
  or a `virtual_path` created with
  [`virtual_path()`](https://pepijn-devries.github.io/adfExplorer/reference/virtual_path.md).

- ...:

  Ignored.

- writable:

  A `logical` value. When `TRUE` the connection can be used to write to
  the file on the virtual device. When `FALSE` it can only be used to
  read. Note that a writeable connection can only be setup on a virtual
  device that is not write protected.

- path:

  Only required when `x` is a virtual device of class `adf_device`. In
  that case `path` should be a character string representing the path to
  the file on the virtual device. See also
  [`vignette("virtual_paths")`](https://pepijn-devries.github.io/adfExplorer/articles/virtual_paths.md).

## Value

Returns an R connection that can be handled by any function that accepts
a connection for reading or writing. Remember to call
[`close()`](https://rdrr.io/r/base/connections.html) after use.

## Author

Pepijn de Vries

## Examples

``` r
## First setup a connection to a virtual device
adz_file <- system.file("example.adz", package = "adfExplorer")
my_device <- connect_adf(adz_file)

## Open a connection to a file on the virtual device
fcon <- adf_file_con(my_device, "DF0:s/startup-sequence")

## Read from the file
my_startup <- readLines(fcon, warn = FALSE)

## Close the file
close(fcon)

## Close the virtual device
close(my_device)
```
