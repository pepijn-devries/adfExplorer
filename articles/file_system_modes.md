# ADF File System Modes

## Fast File System vs. Old File System

With Amiga OS version 2.0, the so-called Fast File System (FFS) was
introduced. The previous file system was not named at the time, but is
now commonly referred to as the Old File System (OFS). The OFS
sacrifices disk space for validation purposes, making it more robust in
case of data recovery when a disk got damaged. This advantage was
dropped with the FFS making it slightly faster (on original machines)
and gaining disk space for file data. The FFS does not have backward
compatability. So disks formatted with this file systems cannot be read
by Amiga OS versions \<2.0.

## International mode

In OS version 2.0 the ‘international mode’ was also introduced. This
mode was meant to correct for a mistake in the routine to convert text
into upper case. On the Amiga file names can have both lower and upper
case characters. But during file name matching, the case is ignored. For
that purpose, file names are shifted to upper case in file name matching
routines.

The Amiga uses the ISO 8859 Latin-1 character set, where in older
operating systems (\<2.0), international characters (e.g., ‘ø’) were not
capitalised. This mistake was corrected in OS 2.0, but is optional. In
combination with the ‘directory cache mode’ (see below), the
international mode is mandatory.

## Directory Caching

With Amiga OS 3.0, the ‘directory cache mode’ was introduced. With the
directory cache mode, one or more blocks are stored for each directory
(including the root) with basic information about the files stored in
that directory. In older versions, the directory header only stored
pointers to the files in that directory. This meant that in older OS
versions the header of each file (all scattered around the disk) needed
to be loaded, in order to list all files in that directory. As with the
directory cache mode all information was stored in one block (or more
when necessary), it was faster at listing directory content. On the
original machine that is, as floppy disk drives were pretty slow.

Note that using the ‘directory cache’ mode requires a larger storage
overhead for the file system, leaving less capacity for actual file
data.

## Creating disks with diferent modes

When you create a blank disk with
[`create_adf_device()`](https://pepijn-devries.github.io/adfExplorer/reference/create_adf_device.md),
it contains no data at all. In order to store files on the virtual disk,
you need to format it with a file system. This can be achieved with
[`prepare_adf_device()`](https://pepijn-devries.github.io/adfExplorer/reference/create_adf_device.md).
With this function you can also specify which of the modes listed above
you want to use for that disk:

``` r
library(adfExplorer, warn.conflicts = FALSE)

disk_file <- tempfile(fileext = ".adf")

## Create a blank device and create a connection to it:
new_device <- create_adf_device(disk_file, write_protected = FALSE)

## Format the device and install a file system:
prepare_adf_device(
  dev           = new_device,
  name          = "Example_disk",
  ffs           = TRUE,  ## Use fast file system
  international = TRUE,  ## Use international mode
  dircache      = FALSE) ## Don't use directory caching.
#> Bootable DOS Floppy DD
#>   Volume 0 [fi-]: Example_disk (0.2%)

## Don't forget to close the connection when you are done:
close(new_device)
```
