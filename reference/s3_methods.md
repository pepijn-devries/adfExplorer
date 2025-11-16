# Basic methods for S3 class objects

Format and print methods for all S3 class objects created with
`adfExplorer`

## Usage

``` r
# S3 method for class 'adf_device'
format(x, ...)

# S3 method for class 'adf_file_con'
format(x, ...)

# S3 method for class 'adf_block'
format(x, ...)

# S3 method for class 'virtual_path'
format(x, width = 20L, ...)

# S3 method for class 'adf_device'
print(x, ...)

# S3 method for class 'adf_file_con'
print(x, ...)

# S3 method for class 'adf_block'
print(x, ...)

# S3 method for class 'virtual_path'
print(x, ...)

# S3 method for class 'virtual_path'
as.character(x, ...)
```

## Arguments

- x:

  Object to be formatted or printed

- ...:

  Ignored or passed on to next methods

- width:

  Set the text width for formatting virtual paths

## Examples

``` r
my_device <- demo_adf()
vp        <- list_adf_entries(my_device, recursive = TRUE)
con       <- adf_file_con(my_device, "s/startup-sequence")
block     <- read_adf_block(my_device, 0L)

format(my_device)
#> [1] "Bootable write protected DOS Floppy DD\n  Volume 0 [-i-]: adfExampleOFS (2.1%)"
format(vp)
#>  [1] "DIR  DEWR...     Devs" "FILE DEWR...iguration" "DIR  DEWR...        S"
#>  [4] "FILE DEWR...-Sequence" "DIR  DEWR...     this" "DIR  DEWR...       is"
#>  [7] "DIR  DEWR...        a" "DIR  DEWR...     deep" "DIR  DEWR...     path"
#> [10] "DIR  DEWR...     mods" "FILE DEWR...mod.intro"
format(con)
#> [1] "A read only connection to virtual file:\nadfExampleOFS:S/Startup-Sequence"
format(block)

print(my_device)
#> Bootable write protected DOS Floppy DD
#>   Volume 0 [-i-]: adfExampleOFS (2.1%)
print(vp)
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
print(con)
#> A read only connection to virtual file:
#> adfExampleOFS:S/Startup-Sequence
print(block)
#> 0x000  444F5302 31AC2977 00000370 43FA003E  DOS.1¬)w...pCú.>
#> 0x010  70254EAE FDD84A80 670C2240 08E90006  p.N®ýØJg..@.é..
#> 0x020  00224EAE FE6243FA 00184EAE FFA04A80  ..N®þbCú..N®ÿ J
#> 0x030  670A2040 20680016 70004E75 70FF4E75  g. @ h..p.NupÿNu
#> 0x040  646F732E 6C696272 61727900 22657870  dos.library..exp
#> 0x050  616E7369 6F6E2E6C 69627261 72790000  ansion.library..
#> 0x060  00000000 00000000 00000000 00000000  ................
#> 0x070  00000000 00000000 00000000 00000000  ................
#> 0x080  00000000 00000000 00000000 00000000  ................
#> 0x090  00000000 00000000 00000000 00000000  ................
#> 0x0A0  00000000 00000000 00000000 00000000  ................
#> 0x0B0  00000000 00000000 00000000 00000000  ................
#> 0x0C0  00000000 00000000 00000000 00000000  ................
#> 0x0D0  00000000 00000000 00000000 00000000  ................
#> 0x0E0  00000000 00000000 00000000 00000000  ................
#> 0x0F0  00000000 00000000 00000000 00000000  ................
#> 0x100  00000000 00000000 00000000 00000000  ................
#> 0x110  00000000 00000000 00000000 00000000  ................
#> 0x120  00000000 00000000 00000000 00000000  ................
#> 0x130  00000000 00000000 00000000 00000000  ................
#> 0x140  00000000 00000000 00000000 00000000  ................
#> 0x150  00000000 00000000 00000000 00000000  ................
#> 0x160  00000000 00000000 00000000 00000000  ................
#> 0x170  00000000 00000000 00000000 00000000  ................
#> 0x180  00000000 00000000 00000000 00000000  ................
#> 0x190  00000000 00000000 00000000 00000000  ................
#> 0x1A0  00000000 00000000 00000000 00000000  ................
#> 0x1B0  00000000 00000000 00000000 00000000  ................
#> 0x1C0  00000000 00000000 00000000 00000000  ................
#> 0x1D0  00000000 00000000 00000000 00000000  ................
#> 0x1E0  00000000 00000000 00000000 00000000  ................
#> 0x1F0  00000000 00000000 00000000 00000000  ................

close(con)
close(my_device)
```
