# A path pointing to a file or directory on a virtual ADF device

This function creates a path pointing to a file or directory on a
virtual ADF device (created with
[`connect_adf()`](https://pepijn-devries.github.io/adfExplorer/reference/connect_adf.md)
or
[`create_adf_device()`](https://pepijn-devries.github.io/adfExplorer/reference/create_adf_device.md)).
The virtual path created with this function can be used to establish a
readable or writable connection to a file, or obtain information about a
file or directory. See also
[`vignette("virtual_paths")`](https://pepijn-devries.github.io/adfExplorer/articles/virtual_paths.md)

## Usage

``` r
virtual_path(dev, path)
```

## Arguments

- dev:

  A virtual ADF device (created with
  [`connect_adf()`](https://pepijn-devries.github.io/adfExplorer/reference/connect_adf.md)
  or
  [`create_adf_device()`](https://pepijn-devries.github.io/adfExplorer/reference/create_adf_device.md)).
  Make sure a file system is present on the virtual device or install
  first when missing using
  [`prepare_adf_device()`](https://pepijn-devries.github.io/adfExplorer/reference/create_adf_device.md).

- path:

  A `character` string representing the path to a file or directory on
  the virtual device.

## Value

Returns a `virtual_path` class object.

## Author

Pepijn de Vries

## Examples

``` r
# Open a connection to a virtual device:
my_device <- demo_adf()

# specify a virtual path:
my_path <- virtual_path(my_device, "DF0:s/startup-sequence")

# close the virtual device:
close(my_device)
```
