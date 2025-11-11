# Remove entry (file / directory) from a virtual ADF device

This function removes an entry (file or directory) from a virtual ADF
device. At the moment this function only removes a single entry per
call, and in case the entry is a directory, the directory needs to be
empty before it can be removed.

## Usage

``` r
remove_adf_entry(x, path, flush = FALSE, ...)

# S3 method for class 'adf_device'
remove_adf_entry(x, path, flush = FALSE, ...)

# S3 method for class 'virtual_path'
remove_adf_entry(x, path, flush = FALSE, ...)

# S3 method for class 'character'
remove_adf_entry.adf_device(x, path, flush = FALSE, ...)

# S3 method for class 'virtual_path'
remove_adf_entry.adf_device(x, path, flush = FALSE, ...)
```

## Arguments

- x:

  The virtual ADF device from which an entry needs to be deleted or a
  virtual path pointing at the entry to be deleted. In case of a virtual
  device, it should be of class `adf_device` which can be created with
  [`create_adf_device()`](https://pepijn-devries.github.io/adfExplorer/reference/create_adf_device.md)
  or
  [`connect_adf()`](https://pepijn-devries.github.io/adfExplorer/reference/connect_adf.md).
  In case of a virtual path use
  [`virtual_path()`](https://pepijn-devries.github.io/adfExplorer/reference/virtual_path.md).

- path:

  A `character` string or a virtual_path (see
  [`virtual_path()`](https://pepijn-devries.github.io/adfExplorer/reference/virtual_path.md))
  representing a file or directory you wish to delete. Should be omitted
  when `x` is already a virtual path.

- flush:

  A `logical` value. When set to `FALSE` (default), only the entry's
  registry in its parent directory is removed and its flags in the
  bitmap block are set to 'available'. The entry's header data and if
  the entry is a file, the file data will still linger on the virtual
  disk. If you don't want that, set this argument to `TRUE`, in that
  case all file or directory data will be purged. Note that in the
  latter case, it won't be possible to recover your deleted file or
  directory.

- ...:

  Ignored

## Value

Returns the device connection

## Author

Pepijn de Vries

## Examples

``` r
# Open a connection to a virtual device:
my_device <- demo_adf(write_protected = FALSE)

## List files in directory 'Devs':
list_adf_entries(my_device, "Devs")
#> FILE DEWR----   0.2 kB system-configuration

## remove the file 'system-configuration' from the virtual device
remove_adf_entry(my_device, "devs/system-configuration")
#> Bootable DOS Floppy DD
#>   Volume 0 [---]: adfExampleOFS (2.0%)

## List files in directory 'Devs' again:
list_adf_entries(my_device, "Devs")
#> :EMPTY:

## close the connection to the virtual device
close(my_device)
```
