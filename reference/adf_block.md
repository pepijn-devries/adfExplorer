# Read or write raw data blocks to a virtual device

The Amiga file system is structured around 512 byte blocks. A double
density floppy disk consists of 1760 blocks of 512 bytes.
`read_adf_block` and `write_adf_block` can be used to transform raw data
from and to virtual devices (created with
[`create_adf_device()`](https://pepijn-devries.github.io/adfExplorer/reference/create_adf_device.md)
or
[`connect_adf()`](https://pepijn-devries.github.io/adfExplorer/reference/connect_adf.md)).
Note that writing raw data to a disk could corrupt the file system on
the device. So it is generally not advised unless you know what you are
doing.

## Usage

``` r
read_adf_block(dev, sector, ...)

# S3 method for class 'adf_device'
read_adf_block(dev, sector, ...)

write_adf_block(dev, sector, data, ...)

# S3 method for class 'adf_device'
write_adf_block(dev, sector, data, ...)

# S3 method for class 'raw'
write_adf_block.adf_device(dev, sector, data, ...)

# S3 method for class 'adf_block'
write_adf_block.adf_device(dev, sector, data, ...)

# Default S3 method
write_adf_block.adf_device(dev, sector, data, ...)

as_adf_block(data, ...)

new_adf_block()
```

## Arguments

- dev:

  The virtual adf device for which information needs to be obtained. It
  should be of class `adf_device` which can be created with
  [`create_adf_device()`](https://pepijn-devries.github.io/adfExplorer/reference/create_adf_device.md)
  or
  [`connect_adf()`](https://pepijn-devries.github.io/adfExplorer/reference/connect_adf.md).

- sector:

  Sector ID of the block you wish to read/write. It is an integer value.
  For double density disks, the ID ranges from 0 to 1759.

- ...:

  Ignored

- data:

  Block data (`raw` vector of length 512) you wish to write to a virtual
  device

## Value

In case of `write_adf_block` `NULL` is returned invisibly. In case of
`read_adf_block` the `raw` data is returned as a `adf_block` class
object.

## Author

Pepijn de Vries

## Examples

``` r
my_device <- demo_adf(write_protected = FALSE)

info <- adf_entry_info(my_device, "S/startup-sequence")

filedata_block <- read_adf_block(my_device, rev(info[[1]]$dataBlocks)[[1]])
filedata_block
#> 0x000  00000008 00000376 00000001 000001E8  .......v.......è
#> 0x010  00000378 5161C6D5 3B205468 65205374  ...xQaÆÕ; The St
#> 0x020  61727475 702D5365 7175656E 63652069  artup-Sequence i
#> 0x030  73206578 65637574 65642061 66746572  s executed after
#> 0x040  20626F6F 74696E67 0A3B2045 76657279   booting.; Every
#> 0x050  7468696E 67206166 74657220 73656D69  thing after semi
#> 0x060  636F6C6F 6E732061 72652063 6F6D6D65  colons are comme
#> 0x070  6E747320 616E6420 69732069 676E6F72  nts and is ignor
#> 0x080  65640A3B 20427920 64656661 756C7420  ed.; By default 
#> 0x090  7374616E 64617264 20636F6D 6D616E64  standard command
#> 0x0A0  73206172 65206C6F 61646564 2066726F  s are loaded fro
#> 0x0B0  6D0A3B20 74686520 524F4D20 6B69636B  m.; the ROM kick
#> 0x0C0  73746172 742E2041 64646974 696F6E61  start. Additiona
#> 0x0D0  6C20636F 6D6D616E 64732073 686F756C  l commands shoul
#> 0x0E0  64206265 0A3B2073 746F7265 64206F6E  d be.; stored on
#> 0x0F0  20746865 20646973 6B20696E 20746865   the disk in the
#> 0x100  20535953 3A432064 69726563 746F7279   SYS:C directory
#> 0x110  2E0A3B20 466F7220 64656D6F 6E737472  ..; For demonstr
#> 0x120  6174696F 6E207075 72706F73 65732077  ation purposes w
#> 0x130  65206F6E 6C792065 63686F20 736F6D65  e only echo some
#> 0x140  0A3B2074 65787420 746F2074 68652073  .; text to the s
#> 0x150  63726565 6E2E2E2E 204E6F74 65207468  creen... Note th
#> 0x160  61742074 68697320 77696C6C 206E6F74  at this will not
#> 0x170  0A3B2077 6F726B20 6F6E2041 6D696761  .; work on Amiga
#> 0x180  204F5320 3C322E30 20617320 22456368   OS <2.0 as .Ech
#> 0x190  6F222069 73206E6F 74206176 61696C61  o. is not availa
#> 0x1A0  626C650A 3B20696E 206F6C64 65722052  ble.; in older R
#> 0x1B0  4F4D206B 69636B73 74617274 20766572  OM kickstart ver
#> 0x1C0  73696F6E 732E0A0A 4563686F 20221B63  sions...Echo ..c
#> 0x1D0  1B5B3232 6D1B5B33 326D4144 46204578  .[22m.[32mADF Ex
#> 0x1E0  706C6F72 65722045 78616D70 6C652044  plorer Example D
#> 0x1F0  69736B22 203B204E 6F746520 74686174  isk. ; Note that

empty_block <- new_adf_block()
empty_block <- as_adf_block(raw(512L))

## Write some random data to block 5 on the device
## Note that this could break the file system on the virtual device!
write_adf_block(my_device, 5, as.raw(runif(512, 0, 255)))
## converting the data to an adf block object first
## is optional:
write_adf_block(my_device, 6, as_adf_block(as.raw(runif(512, 0, 255))))
close(my_device)
```
