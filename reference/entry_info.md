# Retrieve information from entry headers on virtual ADF devices

Retrieve information from entry (file and directory) headers on virtual
ADF devices. Get information like entry name, modification date, file
size etc.

## Usage

``` r
adf_entry_info(x, path, ...)

# S3 method for class 'adf_device'
adf_entry_info(x, path, ...)

# S3 method for class 'virtual_path'
adf_entry_info.adf_device(x, path, ...)

# S3 method for class 'character'
adf_entry_info.adf_device(x, path, ...)

# S3 method for class 'virtual_path'
adf_entry_info(x, path, ...)

# S3 method for class 'adf_file_con'
adf_entry_info(x, path, ...)
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

Returns a `list` of named `list`s of entry properties. Elements included
in the named `list` depend on the type of entry (root, directory or
file).

## Author

Pepijn de Vries

## Examples

``` r
## First setup a connection to a virtual device
my_device <- demo_adf()

adf_entry_info(my_device, "DF0:")
#> [[1]]
#> [[1]]$type
#> [1] "HEADER"
#> 
#> [[1]]$headerKey
#> [1] 0
#> 
#> [[1]]$highSeq
#> [1] 0
#> 
#> [[1]]$firstData
#> [1] 0
#> 
#> [[1]]$checkSum
#> [1] -161095681
#> 
#> [[1]]$hashTable
#>  [1]   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
#> [20]   0   0   0 882   0 885   0   0   0   0   0   0   0   0   0   0   0   0   0
#> [39]   0   0   0   0   0   0   0   0   0   0 889   0   0 894   0   0   0   0   0
#> [58]   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
#> 
#> [[1]]$bitmapFlag
#> [1] TRUE
#> 
#> [[1]]$bmPages
#>  [1] 881   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
#> [20]   0   0   0   0   0   0
#> 
#> [[1]]$bmExt
#> [1] 0
#> 
#> [[1]]$creation
#> [1] "2025-11-13 09:21:27 UTC"
#> 
#> [[1]]$diskName
#> <CHARSXP: "adfExampleOFS">
#> 
#> [[1]]$access
#> [1] "2025-11-13 09:21:27 UTC"
#> 
#> [[1]]$creation_o
#> [1] "2025-11-13 09:21:27 UTC"
#> 
#> [[1]]$nextSameHash
#> [1] 0
#> 
#> [[1]]$parent
#> [1] 0
#> 
#> [[1]]$extension
#> [1] 0
#> 
#> [[1]]$secType
#> [1] "ROOT"
#> 
#> 
adf_entry_info(my_device, "s")
#> [[1]]
#> [[1]]$type
#> [1] "HEADER"
#> 
#> [[1]]$sector
#> [1] 885
#> 
#> [[1]]$highSeq
#> [1] 0
#> 
#> [[1]]$checkSum
#> [1] -22238753
#> 
#> [[1]]$hashTable
#>  [1]   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
#> [20]   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
#> [39]   0   0   0   0   0   0   0   0   0   0   0 886   0   0   0   0   0   0   0
#> [58]   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
#> 
#> [[1]]$access
#>     D     E     W     R     A     P     S     H 
#> FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE 
#> 
#> [[1]]$comment
#> <CHARSXP: "">
#> 
#> [[1]]$modified
#> [1] "2025-11-13 09:21:27 UTC"
#> 
#> [[1]]$dirname
#> <CHARSXP: "S">
#> 
#> [[1]]$real
#> [1] 0
#> 
#> [[1]]$nextLink
#> [1] 0
#> 
#> [[1]]$nextSameHash
#> [1] 0
#> 
#> [[1]]$parent
#> [1] 880
#> 
#> [[1]]$extension
#> [1] 0
#> 
#> [[1]]$secType
#> [1] "DIR"
#> 
#> 
adf_entry_info(my_device, "s/startup-sequence")
#> [[1]]
#> [[1]]$type
#> [1] "HEADER"
#> 
#> [[1]]$headerKey
#> [1] 886
#> 
#> [[1]]$highSeq
#> [1] 2
#> 
#> [[1]]$dataSize
#> [1] 0
#> 
#> [[1]]$firstData
#> [1] 887
#> 
#> [[1]]$checkSum
#> [1] 1971249839
#> 
#> [[1]]$dataBlocks
#>  [1]   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
#> [20]   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
#> [39]   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
#> [58]   0   0   0   0   0   0   0   0   0   0   0   0   0 888 887
#> 
#> [[1]]$access
#>     D     E     W     R     A     P     S     H 
#> FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE 
#> 
#> [[1]]$byteSize
#> [1] 664
#> 
#> [[1]]$comment
#> <CHARSXP: "">
#> 
#> [[1]]$modified
#> [1] "2025-11-13 09:21:27 UTC"
#> 
#> [[1]]$filename
#> <CHARSXP: "Startup-Sequence">
#> 
#> [[1]]$real
#> [1] 0
#> 
#> [[1]]$nextLink
#> [1] 0
#> 
#> [[1]]$nextSameHash
#> [1] 0
#> 
#> [[1]]$parent
#> [1] 885
#> 
#> [[1]]$extension
#> [1] 0
#> 
#> [[1]]$secType
#> [1] "FILE"
#> 
#> 

close(my_device)
```
