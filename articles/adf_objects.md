# ADF S3 Class Objects

The `adfExplorer` package has defined several S3 class objects to make
interaction with virtual ADF devices easier. This page presents these
objects, what they are for and how they can be used.

## `adf_device`

### What is it?

Amiga Disk Files (ADF) or file representations of hardware disks. The
`adf_device` class is used to represent a connection to such files. It
can be seen as a virtual device. The file remains on disk, the
`adf_device` opens a file connection to it. Underneath the S3 class the
object has the type `externalptr`.

### How can it be initialized?

The `adf_device` can be initiated by opening an ADF or ADZ (a zipped
ADF) file with
[`connect_adf()`](https://pepijn-devries.github.io/adfExplorer/reference/connect_adf.md).

``` r
library(adfExplorer, warn.conflicts = FALSE)
adz_file <- system.file("example.adz", package = "adfExplorer")
my_device <- connect_adf(adz_file)
```

It can also be initiated by creating a new device with
[`create_adf_device()`](https://pepijn-devries.github.io/adfExplorer/reference/create_adf_device.md).
As the device needs to be stored as a file, you need to specify its
destination path. The example below uses a temporary file for this
purpose.

``` r
adf_file <- tempfile(fileext = ".adf")
new_device <- create_adf_device(adf_file)
```

### What can I do with it?

Well, that depends. When you just created a device with
[`create_adf_device()`](https://pepijn-devries.github.io/adfExplorer/reference/create_adf_device.md),
like the object named `new_device` in the example above, it does not
contain a file system (see
[`vignette("file_system_modes")`](https://pepijn-devries.github.io/adfExplorer/articles/file_system_modes.md)).
Virtual disks without a file system, could contain unspecified data or a
custom track loader, which can contain instructions running
independently from the operating system. You can inspect those disks by
reading and writing [blocks](#adf_block), the logical unit of data on a
disk.

When a virtual disk does contain a file system, like the one opened with
[`connect_adf()`](https://pepijn-devries.github.io/adfExplorer/reference/connect_adf.md)
stored as the object named `my_device`, you can do a lot more. You can
query the files and directories on the disk. Read, write, copy, move,
manipulate and delete those files. The example below shows how to list
the entries (files and directories) on the disk’s root.

``` r
list_adf_entries(my_device)
#> DIR  DEWR...     Devs
#> DIR  DEWR...        S
#> DIR  DEWR...     this
#> DIR  DEWR...     mods
```

### End of life

As any `externalptr` object an `adf_device` will be cleaned up
automatically by R’s garbage collector when it goes out of scope.
However, as it maintains an open connection to a file on disk, it is
always wise to [`close()`](https://rdrr.io/r/base/connections.html) the
device when you are done with it. Calling
[`close()`](https://rdrr.io/r/base/connections.html) on an `adf_device`
will also automatically close all nested connections to files on the
virtual device (see also [`adf_file_con`](#adf_file_con))

``` r
close(new_device)
## Let's keep `my_device` open to be used in examples below
```

## `adf_file_con`

### What is it?

It is a connection to a file on a virtual device represented by the
[`adf_device`](#adf_device) class objects. Well… Technically, it isn’t a
connection really, because CRAN’s policy does not allow to call non API
entry points into R. This would be required to setup a proper
connection. Instead, the `adf_file_con` is a mockup using an
`externalptr` type to mimic R connections. In essence, it behaves very
much like any other connection in R (more details below).

### How can it be initialized?

An `adf_file_con` object can be initiated using a call to
[`adf_file_con()`](https://pepijn-devries.github.io/adfExplorer/reference/adf_file_con.md).
For this purpose, you first need to connect to a virtual device
containing a file system. We’ll use `my_device` opened in earlier
examples. We can use the path to a file on a virtual device to open a
connection as shown below.

``` r
con <- adf_file_con(my_device, "DF0:mods/mod.intro")

summary(con)
#> $description
#> [1] "mod.intro"
#> 
#> $class
#> [1] "adf_file_con"
#> 
#> $mode
#> [1] "rb"
#> 
#> $text
#> [1] "binary"
#> 
#> $opened
#> [1] "opened"
#> 
#> $`can read`
#> [1] "yes"
#> 
#> $`can write`
#> [1] "no"
```

If you want to open writable connections to a virtual device, you need
to initiate the device without write protection. A writable connection
can also be used to create new files on the virtual device.

### What can I do with it?

Depending on how you set the option `writable` when calling
`adf_file_con`, you can either read and / or write to the connection. By
default the connection is opened as read-only. You can use
[`readBin()`](https://pepijn-devries.github.io/adfExplorer/reference/read_write.md)
to read binary data from the connection.

``` r
readBin(con, "raw", 20L)
#>  [1] 69 6e 74 72 6f 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
```

Note that
[`adf_file_con()`](https://pepijn-devries.github.io/adfExplorer/reference/adf_file_con.md)
always opens the connection as ‘binary’, but you can also use it for
reading and writing text. So basically,
[`readLines()`](https://pepijn-devries.github.io/adfExplorer/reference/read_write.md),
[`writeBin()`](https://pepijn-devries.github.io/adfExplorer/reference/read_write.md)
and
[`writeLines()`](https://pepijn-devries.github.io/adfExplorer/reference/read_write.md)
are all available (the latter two obviously when the connection is
writable).

In addition, you can tell where the current byte offset in the file is
located with [`seek()`](https://rdrr.io/r/base/seek.html). You can also
use it to set the offset to a specific location.

``` r
seek(con)
#> [1] 20
seek(con, 30L)
#> [1] 30
```

### End of life

Like any connection it is good practice to
[`close()`](https://rdrr.io/r/base/connections.html) it when you are
done. Any `adf_file_con` still open when its parent `adf_device` is
closed, will automatically be closed. It can no longer be accessed when
the virtual device is not available.

``` r
close(con)
```

## `virtual_path`

### What is it?

It is a vectorised `list` of `list`s. The nested list contains two
elements:

1.  An `adf_device` class object
2.  A `character` string, specifying a path to a file or directory on
    the virtual device.

The outer list just contains a collection of these.

### How can it be initialized?

It can be initialised by calling
[`virtual_path()`](https://pepijn-devries.github.io/adfExplorer/reference/virtual_path.md).

``` r
virtual_path(my_device, "DF0:s/startup-sequence")
#> FILE DEWR----   0.6 kB Startup-Sequence
virtual_path(my_device, "idontexist")
#> Invalid path
```

Note that the file does not necessarily have to exist in order to create
a virtual path. It doesn’t create any kind of connection to the file,
but you can use it to open one.

### What can I do with it?

The `virtual_path` is a means to refer to files and directory on a
virtual device. It also helps to set up processes with the pipe operator
(`|>`). There is a `vignette("virtual_path")` dedicated to describing
the object and its usage in more detail.

### End of life

It is just a list. You can just call
[`rm()`](https://rdrr.io/r/base/rm.html) on it to get rid of it.

## `adf_block`

### What is it?

A representation of `raw` data of a logical unit on a virtual device of
512 bytes. It is simply a `vector` of `raw` data.

### How can it be initialized?

A new block block can be created with
[`new_adf_block()`](https://pepijn-devries.github.io/adfExplorer/reference/adf_block.md).
It will be initialised with null bytes. You can also coerce a `raw`
`vector` to an `adf_block`.

``` r
block1 <- new_adf_block()
## a block with random data
block2 <- as_adf_block(as.raw(sample.int(n=256L, size = 512L, replace = TRUE) - 1L))
```

You can also intialise a block by reading it from a virtual device

``` r
## This will read the initial 'boot' block
## from the virtual device
block3 <- read_adf_block(my_device, 0L)
```

### What can I do with it?

You can read blocks from a virtual disk with
[`read_adf_block()`](https://pepijn-devries.github.io/adfExplorer/reference/adf_block.md)
and write them to a specific sector on the virtual disk with
[`write_adf_block()`](https://pepijn-devries.github.io/adfExplorer/reference/adf_block.md).
But be careful you can damage the file system or track loader of the
virtual disk if you don’t know what you are doing.

### End of life

It can just be removed from memory by calling
[`rm()`](https://rdrr.io/r/base/rm.html). As there is no actual link to
the virtual device, removing an `adf_block` class object from memory
does not affect your virtual device.

``` r
rm(block1, block2, block3)
```
