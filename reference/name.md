# Obtain or modify an entry name on a virtual device

Get the name of an entry (root, file or directory) or update it with the
assign operator (`<-`).

## Usage

``` r
adf_entry_name(x, path, ...) <- value

adf_entry_name(x, path, ...)

# S3 method for class 'adf_file_con'
adf_entry_name(x, path, ...) <- value

# S3 method for class 'adf_device'
adf_entry_name(x, path, ...) <- value

# S3 method for class 'virtual_path'
adf_entry_name(x, path, ...) <- value

# S3 method for class 'adf_device.character'
adf_entry_name(x, path, ...) <- value

# S3 method for class 'adf_device.virtual_path'
adf_entry_name(x, path, ...) <- value
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

- value:

  New name for the entry. The name will be sanitised and truncated
  before it is assigned to the entry.

## Value

Returns the entry name of the requested path or in case of an assign
operation (`<-`) an updated version of `x`.

## Author

Pepijn de Vries

## Examples

``` r
## Open virtual device to demonstrate methods
my_device <- demo_adf(write_protected = FALSE)

## rename a specific entry
adf_entry_name(my_device, "DF0:mods/mod.intro") <- "mod.music"

## rename disk (also possible with `volume_name<-()`)
adf_entry_name(my_device, "DF0:") <- "my_disk"

close(my_device)
```
