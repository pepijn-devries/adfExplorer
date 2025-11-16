# Get virtual disks bitmap flags

Amiga devices had one (or more) blocks dedicated to registering which
blocks are used by the file system and which are available to allocate.
This block is called the bitmap block. This function returns the bitmap
table for the specified volume.

## Usage

``` r
get_adf_bitmap(dev, vol = 0L, ...)
```

## Arguments

- dev:

  The virtual adf device for which information needs to be obtained. It
  should be of class `adf_device` which can be created with
  [`create_adf_device()`](https://pepijn-devries.github.io/adfExplorer/reference/create_adf_device.md)
  or
  [`connect_adf()`](https://pepijn-devries.github.io/adfExplorer/reference/connect_adf.md).

- vol:

  Volume index number on the device starting at `0`. Default is `0`.
  Note that floppy disks can only have 1 volume installed.

- ...:

  Ignored

## Value

Returns the bitmap table as a `logical` `vector`. Each, element is named
after its referencing block number, the value indicates if the block is
reserved by the file system.

## Examples

``` r
my_device <- demo_adf()

bitmap <- get_adf_bitmap(my_device)

## Show blocks used by the file system
bitmap[bitmap]
#>  880  881  882  883  884  885  886  887  888  889  890  891  892  893  894  895 
#> TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE 
#>  896  897  898  899  900  901  902  903  904  905  906  907  908  909  910  911 
#> TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE 
#>  912  913  914 
#> TRUE TRUE TRUE 

close(my_device)
```
