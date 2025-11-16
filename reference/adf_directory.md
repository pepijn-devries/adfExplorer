# Changing and creating directories on a virtual device

`adf_directory()` shows the current directory of a virtual device, when
a file system is present. When connecting to or creating a new device,
the current directory is the disk's root by default. To change the
current directory, use `adf_directory()` in combination with the assign
operator (`<-`).

## Usage

``` r
adf_directory(dev, ...)

# S3 method for class 'adf_device'
adf_directory(dev, ...)

adf_directory(dev, ...) <- value

# S3 method for class 'adf_device'
adf_directory(dev, ...) <- value

# S3 method for class 'adf_device.character'
adf_directory(dev, ...) <- value

# S3 method for class 'adf_device.virtual_path'
adf_directory(dev, ...) <- value

make_adf_dir(x, path, ...)

# S3 method for class 'adf_device'
make_adf_dir(x, path, ...)

# S3 method for class 'virtual_path'
make_adf_dir(x, path, ...)

# S3 method for class 'character'
make_adf_dir.adf_device(x, path, ...)

# S3 method for class 'virtual_path'
make_adf_dir.adf_device(x, path, ...)
```

## Arguments

- dev:

  The virtual adf device for which information needs to be obtained. It
  should be of class `adf_device` which can be created with
  [`create_adf_device()`](https://pepijn-devries.github.io/adfExplorer/reference/create_adf_device.md)
  or
  [`connect_adf()`](https://pepijn-devries.github.io/adfExplorer/reference/connect_adf.md).

- ...:

  Ignored

- value:

  A `character` string or a `virtual_path` (see
  [`virtual_path()`](https://pepijn-devries.github.io/adfExplorer/reference/virtual_path.md))
  representing directory you wish to set as current.

- x:

  An `adf_device` or `virtual_path` class object. The first specifies
  the device on which a directory needs to be created. The latter
  specifies both the directory and the device on which it needs to be
  created.

- path:

  A `character` string or a `virtual_path` (see
  [`virtual_path()`](https://pepijn-devries.github.io/adfExplorer/reference/virtual_path.md))
  specifying the name of the new directory to be created. Should be
  missing when `x` is of class `virtual_path`

## Value

`make_adf_dir()` returns the device connection. `adf_directory()`
returns the current directory as a `virtual_path` class object.

## Details

To create a new directory on a device use `make_adf_dir()` and use a
full or relative path name to specify the new directory name.

See
[`vignette("virtual_paths")`](https://pepijn-devries.github.io/adfExplorer/articles/virtual_paths.md)
for a note on file and directory names on the Amiga.

## Author

Pepijn de Vries

## Examples

``` r
## Open virtual device to demonstrate methods
my_device <- demo_adf(write_protected = FALSE)

## Show the current directory
adf_directory(my_device)
#> ROOT                   adfExampleOFS

## Create a new directory
make_adf_dir(my_device, "DF0:s/newdir")
#> Bootable DOS Floppy DD
#>   Volume 0 [-i-]: adfExampleOFS (2.2%)

## Change the current dir to the new directory:
adf_directory(my_device) <- "DF0:s/newdir"

## Close the virtual device
close(my_device)
```
