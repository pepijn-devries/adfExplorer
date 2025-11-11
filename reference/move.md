# Copy or move files between physical and virtual devices

With these functions you can copy or move entries (files and
directories) between a physical and virtual ADF device. With
`copy_adf_entry()` the files are duplicated, with `move_adf_entry()` the
files are moved (and deleted from its source).

## Usage

``` r
copy_adf_entry(source, destination, ...)

# S3 method for class 'character'
copy_adf_entry(source, destination, ...)

# S3 method for class 'virtual_path'
copy_adf_entry(source, destination, ...)

# S3 method for class 'virtual_path'
copy_adf_entry.character(source, destination, ...)

# S3 method for class 'virtual_path'
copy_adf_entry.virtual_path(source, destination, ...)

# S3 method for class 'character'
copy_adf_entry.virtual_path(source, destination, ...)

move_adf_entry(source, destination, ...)

# S3 method for class 'character'
move_adf_entry(source, destination, ...)

# S3 method for class 'virtual_path'
move_adf_entry(source, destination, ...)

# S3 method for class 'virtual_path'
move_adf_entry.character(source, destination, ...)

# S3 method for class 'virtual_path'
move_adf_entry.virtual_path(source, destination, ...)

# S3 method for class 'character'
move_adf_entry.virtual_path(source, destination, ...)
```

## Arguments

- source, destination:

  The `source` is a path to a file or directory that needs to be moved
  or copied. `destination` is a path to a directory to which the
  `source` needs to be copied or moved. When `source` or `destination`
  is a `character` string, it is assumed to be a path to a file or
  directory on a physical device. You can use a
  [`virtual_path()`](https://pepijn-devries.github.io/adfExplorer/reference/virtual_path.md)
  for either the `source` or `destination` or both. `source` and
  `destination` cannot both be a `character` string. For copying and
  moving files on a physical device you should you `base` function
  [`file.copy()`](https://rdrr.io/r/base/files.html).

- ...:

  Ignored

## Author

Pepijn de Vries

## Examples

``` r
## Create an Amiga Disk File
## and prepare a file system on the virtual device
my_device <-
  create_adf_device(
    tempfile(fileext = ".adf"),
    write_protected = FALSE) |>
  prepare_adf_device()

## Copy the packaged R scripts of this package to the virtual device
copy_adf_entry(
  system.file("R", package = "adfExplorer"),
  virtual_path(my_device, "DF0:")
)

## List all entries on the virtual device
list_adf_entries(my_device, recursive = TRUE)
#> DIR  DEWR...        R
#> FILE DEWR...lorer.rdb
#> FILE DEWR...fExplorer
#> FILE DEWR...lorer.rdx

## Move the entire virtual device content to
## the tempdir on your physical device
dest <- file.path(tempdir(), "DF0")
dir.create(dest)
move_adf_entry(
  virtual_path(my_device, "DF0:"),
  dest
)

## cleanup the temp directory
unlink(dest, recursive = TRUE)

close(my_device)
```
