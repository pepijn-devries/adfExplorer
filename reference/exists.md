# Test if an entry exists on a virtual device

Test if an entry (file or directory) exists on a virtual ADF device.
`adf_file_exists()` is the equivalent of
[`file.exists()`](https://rdrr.io/r/base/files.html) on a virtual ADF
device. `adf_dir_exists()` is the equivalent of
[`dir.exists()`](https://rdrr.io/r/base/files2.html) on a virtual ADF
device.

## Usage

``` r
adf_file_exists(x, path, ...)

# S3 method for class 'adf_device'
adf_file_exists(x, path, ...)

# S3 method for class 'virtual_path'
adf_file_exists(x, path, ...)

adf_dir_exists(x, path, ...)

# S3 method for class 'adf_device'
adf_dir_exists(x, path, ...)

# S3 method for class 'virtual_path'
adf_dir_exists(x, path, ...)
```

## Arguments

- x:

  Either a virtual device or virtual path.

- path:

  A
  [`virtual_path()`](https://pepijn-devries.github.io/adfExplorer/reference/virtual_path.md)
  pointing to the targeted entry (file or directory). Should be omitted
  when `x` is already a virtual path.

- ...:

  Ignored

## Value

`adf_file_exists()` returns `TRUE` if the path exists on the virtual
device, `FALSE` otherwise. `adf_dir_exists()` returns `TRUE` when the
path exists and is a directory, `FALSE` otherwise.

## Author

Pepijn de Vries

## Examples

``` r
## First setup a connection to a virtual device
my_device <- demo_adf()

adf_file_exists(my_device, "s/startup-sequence")
#> [1] TRUE
adf_dir_exists(my_device, "s/startup-sequence")
#> [1] FALSE

close(my_device)
```
