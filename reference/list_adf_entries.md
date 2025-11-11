# List entries in a directory of a virtual ADF device

Get an overview of all entries (files and directories) in a specific
directory.

## Usage

``` r
list_adf_entries(x, path, recursive = FALSE, nested = FALSE, ...)

# S3 method for class 'adf_device'
list_adf_entries(x, path, recursive = FALSE, nested = FALSE, ...)

# S3 method for class 'virtual_path'
list_adf_entries(x, path, recursive = FALSE, nested = FALSE, ...)

# S3 method for class 'character'
list_adf_entries.adf_device(x, path, recursive = FALSE, nested = FALSE, ...)

# S3 method for class 'virtual_path'
list_adf_entries.adf_device(x, path, recursive = FALSE, ...)
```

## Arguments

- x:

  Either an `adf_device` class object, in which case the `virtual_path`
  argument needs to be specified; or, a `virtual_path` class object.

- path:

  The virtual path for which you wish to obtain a list of entries (see
  also
  [`vignette("virtual_paths")`](https://pepijn-devries.github.io/adfExplorer/articles/virtual_paths.md)).
  When missing, entries for the current directory
  ([`adf_directory()`](https://pepijn-devries.github.io/adfExplorer/reference/adf_directory.md))
  are returned, wen `x` is an `adf_device` class object. If `x` is a
  `virtual_path` class object, content of the path defined in that
  object is listed

- recursive:

  A `logical` value. When set to `TRUE`, the function is called
  recursively for all subdirectories in `path`.

- nested:

  A `logical` value. When set to The directory tree is returned as a
  nested list.

- ...:

  Ignored

## Value

A vector of `virtual_path` class objects, or a nested `list` in case
`nested` is `TRUE`.

## Author

Pepijn de Vries

## Examples

``` r
## First setup a connection to a virtual device
my_device <- demo_adf()

## List all entries in the disk's root:
list_adf_entries(my_device)
#> DIR  DEWR...     Devs
#> DIR  DEWR...        S
#> DIR  DEWR...     this
#> DIR  DEWR...     mods

## List all entries on the disk as a vector of `virtual paths`:
list_adf_entries(my_device, recursive = TRUE)
#> DIR  DEWR...     Devs
#> FILE DEWR...iguration
#> DIR  DEWR...        S
#> FILE DEWR...-Sequence
#> DIR  DEWR...     this
#> DIR  DEWR...       is
#> DIR  DEWR...        a
#> DIR  DEWR...     deep
#> DIR  DEWR...     path
#> DIR  DEWR...     mods
#> FILE DEWR...mod.intro

##  List all entries on the disk as a nested list:
list_adf_entries(my_device, recursive = TRUE, nested = TRUE)
#> $Devs
#> $Devs$`system-configuration`
#> NULL
#> 
#> 
#> $S
#> $S$`Startup-Sequence`
#> NULL
#> 
#> 
#> $this
#> $this$is
#> $this$is$a
#> $this$is$a$deep
#> $this$is$a$deep$path
#> list()
#> 
#> 
#> 
#> 
#> 
#> $mods
#> $mods$mod.intro
#> NULL
#> 
#> 

close(my_device)
```
