# Obtain information about an `adf_device` connection

A collection of functions to retrieve information about the virtual
device, or any volume (file system) available on the device. See
examples for usage and results.

## Usage

``` r
device_type(dev, ...)

# S3 method for class 'adf_device'
device_type(dev, ...)

device_capacity(dev, ...)

# S3 method for class 'adf_device'
device_capacity(dev, ...)

volume_capacity(dev, ...)

# S3 method for class 'adf_device'
volume_capacity(dev, vol = 0L, ...)

volume_name(dev, ...)

volume_name(dev, ...) <- value

# S3 method for class 'adf_device'
volume_name(dev, vol = 0L, ...)

# S3 method for class 'adf_device'
volume_name(dev, vol = 0L, ...) <- value

n_volumes(dev, ...)

# S3 method for class 'adf_device'
n_volumes(dev, ...)

bytes_free(dev, ...)

# S3 method for class 'adf_device'
bytes_free(dev, vol = 0L, ...)

is_bootable(dev, ...)

# S3 method for class 'adf_device'
is_bootable(dev, vol = 0L, ...)

is_fast_file_system(dev, ...)

# S3 method for class 'adf_device'
is_fast_file_system(dev, vol = 0L, ...)

is_international(dev, ...)

# S3 method for class 'adf_device'
is_international(dev, vol = 0L, ...)

is_dircache(dev, ...)

# S3 method for class 'adf_device'
is_dircache(dev, vol = 0L, ...)

is_write_protected(dev, ...)

# S3 method for class 'adf_device'
is_write_protected(dev, ...)
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

- vol:

  Volume index number on the device starting at `0`. Default is `0`.
  Note that floppy disks can only have 1 volume installed.

- value:

  Replacement value. In case of `volume_name()` it can be used to assign
  a new name to the volume.

## Value

Returns the requested information, or an updated copy of `dev` in case
of an assign operation (`<-`).

## Author

Pepijn de Vries

## Examples

``` r
## Open virtual device to demonstrate methods
my_device <- demo_adf(write_protected = FALSE)

device_type(my_device)
#> [1] "Floppy DD"

device_capacity(my_device) # in bytes
#> [1] 901120

volume_capacity(my_device) # in bytes
#> [1] 901120

n_volumes(my_device) # number of volumes available on device
#> [1] 1

volume_name(my_device) # name of the volume
#> [1] "adfExampleOFS"

volume_name(my_device) <- "new_name" # rename the volume

bytes_free(my_device) # bytes available for writing
#> [1] 882176

is_bootable(my_device) # tests if device is potentially bootable
#> [1] TRUE

is_fast_file_system(my_device) # tests if volume uses FFS
#> [1] FALSE

is_international(my_device) # tests if file system uses intl mode
#> [1] FALSE

is_dircache(my_device) # tests if file system uses dir caching
#> [1] FALSE

is_write_protected(my_device) # tests if device is protected against writing
#> [1] FALSE

close(my_device)
```
