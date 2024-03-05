
# adfExplorer

<img src="https://upload.wikimedia.org/wikipedia/commons/e/e8/Kickstart1_3.png" alt="Kickstart V1.3 splash screen" align="right" />

Read and mutate virtual Commodore Amiga Disks, stored as files (a.k.a
Amiga Disk Files).

## Installation

> Get CRAN version

``` r
install.packages("adfExplorer")
```

> Get development version from R-Universe

``` r
install.packages("adfExplorer", repos = c("https://pepijn-devries.r-universe.dev", "https://cloud.r-project.org"))
```

## Usage

The package comes with an example disk for which files and directories
can be listed:

``` r
library(adfExplorer)
data("adf.example")

list.adf.files(adf.example)
#> [1] "Devs" "S"    "this" "mods"
```

You can also pull files from the virtual disk:

``` r
## get startup-sequence file and store as a vector of raw data
startup <- get.adf.file(adf.example, "DF0:s/startup-sequence")

## As this particular file is UTF8 text we can convert it from raw:
startup  |> rawToChar() |> iconv(from = "ISO-8859-1", to = "UTF-8") |> cat()
#> ; The Startup-Sequence is executed after booting
#> ; Everything after semicolons are comments and is ignored
#> ; By default standard commands are loaded from
#> ; the ROM kickstart. Additional commands should be
#> ; stored on the disk in the SYS:C directory.
#> ; For demonstration purposes we only echo some
#> ; text to the screen... Note that this will not
#> ; work on Amiga OS <2.0 as "Echo" is not available
#> ; in older ROM kickstart versions.
#> 
#> Echo "c[22m[32mADF Explorer Example Disk" ; Note that the weird characters at the start are escape-codes to format the text
#> Echo "[0mThis disk was created as an example for the"
#> Echo "R package 'adfExplorer' by Pepijn de Vries."
```

For more in depth examples see `vignette("amigaDiskFiles")`.

## Developmental status

It has been years since the initial release of this package. Since its
release a lot has changed in R and in my skill set. I still have plans
for this package, but this requires drastic changes. Therefore, I will
only apply minimal maintenance to this package. Meanwhile I will work on
a complete overhaul which will be released in due time as a new package.
This new package will:

- have the same or similar features as the current package.
- will implement modern R features and try to adhere to tidyverse
  principles.
- implement recursive procedures in C/C++ giving the package a speed
  boost.

But until then, please enjoy the current package as is.
