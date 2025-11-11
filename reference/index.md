# Package index

## All functions

- [`read_adf_block()`](https://pepijn-devries.github.io/adfExplorer/reference/adf_block.md)
  [`write_adf_block()`](https://pepijn-devries.github.io/adfExplorer/reference/adf_block.md)
  [`write_adf_block.adf_device(`*`<raw>`*`)`](https://pepijn-devries.github.io/adfExplorer/reference/adf_block.md)
  [`write_adf_block.adf_device(`*`<adf_block>`*`)`](https://pepijn-devries.github.io/adfExplorer/reference/adf_block.md)
  [`write_adf_block.adf_device(`*`<default>`*`)`](https://pepijn-devries.github.io/adfExplorer/reference/adf_block.md)
  [`as_adf_block()`](https://pepijn-devries.github.io/adfExplorer/reference/adf_block.md)
  [`new_adf_block()`](https://pepijn-devries.github.io/adfExplorer/reference/adf_block.md)
  : Read or write raw data blocks to a virtual device

- [`adf_directory()`](https://pepijn-devries.github.io/adfExplorer/reference/adf_directory.md)
  [`` `adf_directory<-`() ``](https://pepijn-devries.github.io/adfExplorer/reference/adf_directory.md)
  [`make_adf_dir()`](https://pepijn-devries.github.io/adfExplorer/reference/adf_directory.md)
  [`make_adf_dir.adf_device(`*`<character>`*`)`](https://pepijn-devries.github.io/adfExplorer/reference/adf_directory.md)
  [`make_adf_dir.adf_device(`*`<virtual_path>`*`)`](https://pepijn-devries.github.io/adfExplorer/reference/adf_directory.md)
  : Changing and creating directories on a virtual device

- [`adf_file_con()`](https://pepijn-devries.github.io/adfExplorer/reference/adf_file_con.md)
  [`adf_file_con.adf_device(`*`<character>`*`)`](https://pepijn-devries.github.io/adfExplorer/reference/adf_file_con.md)
  : Open a connection to a file on a virtual ADF device

- [`close_all_devices()`](https://pepijn-devries.github.io/adfExplorer/reference/close_all_devices.md)
  : Close all virtual devices

- [`compress_adf()`](https://pepijn-devries.github.io/adfExplorer/reference/compress.md)
  [`decompress_adz()`](https://pepijn-devries.github.io/adfExplorer/reference/compress.md)
  : Compress ADF to ADZ files and vice versa

- [`connect_adf()`](https://pepijn-devries.github.io/adfExplorer/reference/connect_adf.md)
  : Create a connection to a virtual disk

- [`create_adf_device()`](https://pepijn-devries.github.io/adfExplorer/reference/create_adf_device.md)
  [`prepare_adf_device()`](https://pepijn-devries.github.io/adfExplorer/reference/create_adf_device.md)
  : Create and format a virtual ADF device

- [`demo_adf()`](https://pepijn-devries.github.io/adfExplorer/reference/demo_adf.md)
  : Connect with a demonstration ADF file

- [`device_type()`](https://pepijn-devries.github.io/adfExplorer/reference/device_info.md)
  [`device_capacity()`](https://pepijn-devries.github.io/adfExplorer/reference/device_info.md)
  [`volume_capacity()`](https://pepijn-devries.github.io/adfExplorer/reference/device_info.md)
  [`volume_name()`](https://pepijn-devries.github.io/adfExplorer/reference/device_info.md)
  [`` `volume_name<-`() ``](https://pepijn-devries.github.io/adfExplorer/reference/device_info.md)
  [`n_volumes()`](https://pepijn-devries.github.io/adfExplorer/reference/device_info.md)
  [`bytes_free()`](https://pepijn-devries.github.io/adfExplorer/reference/device_info.md)
  [`is_bootable()`](https://pepijn-devries.github.io/adfExplorer/reference/device_info.md)
  [`is_fast_file_system()`](https://pepijn-devries.github.io/adfExplorer/reference/device_info.md)
  [`is_international()`](https://pepijn-devries.github.io/adfExplorer/reference/device_info.md)
  [`is_dircache()`](https://pepijn-devries.github.io/adfExplorer/reference/device_info.md)
  [`is_write_protected()`](https://pepijn-devries.github.io/adfExplorer/reference/device_info.md)
  :

  Obtain information about an `adf_device` connection

- [`adf_entry_info()`](https://pepijn-devries.github.io/adfExplorer/reference/entry_info.md)
  [`adf_entry_info.adf_device(`*`<virtual_path>`*`)`](https://pepijn-devries.github.io/adfExplorer/reference/entry_info.md)
  [`adf_entry_info.adf_device(`*`<character>`*`)`](https://pepijn-devries.github.io/adfExplorer/reference/entry_info.md)
  : Retrieve information from entry headers on virtual ADF devices

- [`adf_file_exists()`](https://pepijn-devries.github.io/adfExplorer/reference/exists.md)
  [`adf_dir_exists()`](https://pepijn-devries.github.io/adfExplorer/reference/exists.md)
  : Test if an entry exists on a virtual device

- [`list_adf_entries()`](https://pepijn-devries.github.io/adfExplorer/reference/list_adf_entries.md)
  [`list_adf_entries.adf_device(`*`<character>`*`)`](https://pepijn-devries.github.io/adfExplorer/reference/list_adf_entries.md)
  [`list_adf_entries.adf_device(`*`<virtual_path>`*`)`](https://pepijn-devries.github.io/adfExplorer/reference/list_adf_entries.md)
  : List entries in a directory of a virtual ADF device

- [`copy_adf_entry()`](https://pepijn-devries.github.io/adfExplorer/reference/move.md)
  [`copy_adf_entry.character(`*`<virtual_path>`*`)`](https://pepijn-devries.github.io/adfExplorer/reference/move.md)
  [`copy_adf_entry.virtual_path(`*`<virtual_path>`*`)`](https://pepijn-devries.github.io/adfExplorer/reference/move.md)
  [`copy_adf_entry.virtual_path(`*`<character>`*`)`](https://pepijn-devries.github.io/adfExplorer/reference/move.md)
  [`move_adf_entry()`](https://pepijn-devries.github.io/adfExplorer/reference/move.md)
  [`move_adf_entry.character(`*`<virtual_path>`*`)`](https://pepijn-devries.github.io/adfExplorer/reference/move.md)
  [`move_adf_entry.virtual_path(`*`<virtual_path>`*`)`](https://pepijn-devries.github.io/adfExplorer/reference/move.md)
  [`move_adf_entry.virtual_path(`*`<character>`*`)`](https://pepijn-devries.github.io/adfExplorer/reference/move.md)
  : Copy or move files between physical and virtual devices

- [`` `adf_entry_name<-`() ``](https://pepijn-devries.github.io/adfExplorer/reference/name.md)
  [`adf_entry_name()`](https://pepijn-devries.github.io/adfExplorer/reference/name.md)
  : Obtain or modify an entry name on a virtual device

- [`readBin()`](https://pepijn-devries.github.io/adfExplorer/reference/read_write.md)
  [`readLines()`](https://pepijn-devries.github.io/adfExplorer/reference/read_write.md)
  [`writeBin()`](https://pepijn-devries.github.io/adfExplorer/reference/read_write.md)
  [`writeLines()`](https://pepijn-devries.github.io/adfExplorer/reference/read_write.md)
  : Transfer binary data to and from connections

- [`remove_adf_entry()`](https://pepijn-devries.github.io/adfExplorer/reference/remove_adf_entry.md)
  [`remove_adf_entry.adf_device(`*`<character>`*`)`](https://pepijn-devries.github.io/adfExplorer/reference/remove_adf_entry.md)
  [`remove_adf_entry.adf_device(`*`<virtual_path>`*`)`](https://pepijn-devries.github.io/adfExplorer/reference/remove_adf_entry.md)
  : Remove entry (file / directory) from a virtual ADF device

- [`format(`*`<adf_device>`*`)`](https://pepijn-devries.github.io/adfExplorer/reference/s3_methods.md)
  [`format(`*`<adf_file_con>`*`)`](https://pepijn-devries.github.io/adfExplorer/reference/s3_methods.md)
  [`format(`*`<adf_block>`*`)`](https://pepijn-devries.github.io/adfExplorer/reference/s3_methods.md)
  [`format(`*`<virtual_path>`*`)`](https://pepijn-devries.github.io/adfExplorer/reference/s3_methods.md)
  [`print(`*`<adf_device>`*`)`](https://pepijn-devries.github.io/adfExplorer/reference/s3_methods.md)
  [`print(`*`<adf_file_con>`*`)`](https://pepijn-devries.github.io/adfExplorer/reference/s3_methods.md)
  [`print(`*`<adf_block>`*`)`](https://pepijn-devries.github.io/adfExplorer/reference/s3_methods.md)
  [`print(`*`<virtual_path>`*`)`](https://pepijn-devries.github.io/adfExplorer/reference/s3_methods.md)
  [`as.character(`*`<virtual_path>`*`)`](https://pepijn-devries.github.io/adfExplorer/reference/s3_methods.md)
  : Basic methods for S3 class objects

- [`virtual_path()`](https://pepijn-devries.github.io/adfExplorer/reference/virtual_path.md)
  : A path pointing to a file or directory on a virtual ADF device
