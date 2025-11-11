# Virtual Paths

## Virtual paths

In computing, a path is a unique identifier of a file in a directory
system. And is usually represented by a character string. The
`adfExplorer` package introduces a virtual path as a path to a file or
directory on a virtual device. In order to define a virtual path, we
first need a virtual device to work with. The code below show how to set
up a connection to the Amiga Disk File provided with this package as an
example.

``` r
library(adfExplorer, warn.conflicts = FALSE)

adz_file <- system.file("example.adz", package = "adfExplorer")
my_device <- connect_adf(adz_file)
```

### Full path

A full path always starts at the root of the file system. There are
several alternatives to refer to the disk’s root, but each needs to be
followed by a colon character.

Option 1 is to use the devices logical name. For a floppy disk in the
first station is called `"DF0:"`:

``` r
virtual_path(my_device, "DF0:")
#> ROOT                   adfExampleOFS
```

If the device happens to be the system’s device you can refer to it as
`"SYS:"`. Here any device is also treated as the system’s device. So you
can use:

``` r
virtual_path(my_device, "SYS:")
#> ROOT                   adfExampleOFS
```

Of course, if you know the disk’s name, you can also use that to refer
to the root:

``` r
virtual_path(my_device, "adfExampleOFS:")
#> ROOT                   adfExampleOFS
```

It doesn’t matter which of these options you use, they all refer to the
root.

From the root, you can specify the subsequent path to a file or
directory. directories are separated by forward slashes on the Amiga. So
you could use:

``` r
virtual_path(my_device, "DF0:s/startup-sequence")
#> FILE DEWR----   0.6 kB Startup-Sequence
```

This refers to the file ‘startup-sequence’ in the directory ‘s’ which in
turn is located on the disk’s root. Note that the resulting displayed
file name contains uppercase characters, whereas the requested path does
not. This is because Amiga paths are case insensitive.

### Relative path

You can also use relative paths. These are paths specified from the
current directory on the device onward. When you connect to a device,
the current directory is automatically set to the disk’s root. So you
could skip the root if you wish to refer to the same file as above:

``` r
virtual_path(my_device, "s/startup-sequence")
#> FILE DEWR----   0.6 kB Startup-Sequence
```

## Cleaning up

Don’t forget to cleanup after yourself and close the connection to the
virtual device

``` r
close(my_device)
```

## A note on disk, file and directory names

The name of a disk, file and directories should consist of at least 1
UTF8 character, or a maximum of 30 characters. You can use both lower
and upper case characters in a name. You can use almost any character in
a name, but it is advisable to use only alphanumerical characters,
spaces and periods. The only characters that are really not allowed in
Amiga file names are forward slashes and colons, as these are used as
path separators.
