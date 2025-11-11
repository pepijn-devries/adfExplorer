# Create a connection to a virtual disk

Establish a connection to a virtual disk stored as Amiga Disk Files
(ADF). You cannot write or read directly from this connection. Instead,
use the methods provided in this package to retrieve information about
the virtual disk or create connections to the files on the disk, to
which you *can* write and read from (see
[`adf_file_con()`](https://pepijn-devries.github.io/adfExplorer/reference/adf_file_con.md)).
Like any other connection, please use
[`close()`](https://rdrr.io/r/base/connections.html) to close the
connection after use.

## Usage

``` r
connect_adf(filename, write_protected = TRUE)
```

## Arguments

- filename:

  Filename of the `ADF` or `ADZ` file containing the virtual disk

- write_protected:

  A `logical` value indicating whether the virtual disk needs to be
  write protected. If `TRUE`, you can only open 'read only' connections
  and cannot write to the disk.

## Value

Returns an R connection of class `adf_device`.

## Author

Pepijn de Vries

## Examples

``` r
adz_file <- system.file("example.adz", package = "adfExplorer")
my_device <- connect_adf(adz_file)

device_capacity(my_device)
#> [1] 901120
close(my_device)
```
