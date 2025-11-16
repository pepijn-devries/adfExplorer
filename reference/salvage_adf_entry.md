# Salvage entries on ADF disks

Functions to attempt to salvage files and directories on virtual disks
(ADF).

## Usage

``` r
adf_dumpster_dive(dev, vol = 0L, ...)

salvage_adf_entry(dev, vol = 0L, sector, ...)
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

- sector:

  Sector number of the file/directory header that you wish to salvage

## Value

In case of `adf_dumpster_dive()` a `data.frame` is returned with records
that could potentially be salvaged from the disk. In case of
`salvage_adf_entry()`, the `disk` (`adf_device` class object) is
returned, where (if successful) the record is restored.

## Details

When a file or directory is removed from a virtual device (or a physical
device for that matter), the record is removed from its parent directory
hash table and its bitmap flags are flipped to mark the file blocks as
available. This means that the actual record of the file and its
contents is still on the disk, until they are overwritten. You can use
these functions to salvage such entries.

You can use `adf_dumster_dive()` to scan for entries that could
potentially be salvaged. Than you can use `salvage_adf_entry()` to
attempt to salvage an entry.

Note that these functions won't work when you have flushed the file (see
[`remove_adf_entry()`](https://pepijn-devries.github.io/adfExplorer/reference/remove_adf_entry.md)).
In that case the actual record of the file and its data have been
permanently removed.

## Examples

``` r
disk <- demo_adf(write_protected = FALSE)
# The demo disk contains a deleted file that could be salvaged:
salvageable <- adf_dumpster_dive(disk)

# Let's recover it:
salvage_adf_entry(disk, sector = salvageable$sect)
#> Bootable DOS Floppy DD
#>   Volume 0 [-i-]: adfExampleOFS (2.2%)

# It is now listed as an entry on the disk:
list_adf_entries(disk, recursive = TRUE)
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
#> FILE DEWR...r egg.txt
```
