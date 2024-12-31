deprecated <- list(
  amigaDateToRaw, amigaIntToRaw, bitmapToRaw, displayRawData, rawToAmigaDate,
  rawToAmigaInt, rawToBitmap, `adf.disk.name<-`, `adf.file.mode<-`,
  `adf.file.time<-`, `amigaBlock<-`, `current.adf.dir<-`, adf.disk.name,
  adf.file.exists, adf.file.info, adf.file.mode, adf.file.remove, adf.file.size,
  adf.file.time, amigaBlock, blank.amigaDOSDisk, current.adf.dir, dir.create.adf,
  dir.exists.adf, get.blockID, get.diskLocation, is.amigaDOS, is.bootable,
  list.adf.files, put.adf.file, read.adf, read.adz, write.adf, write.adz)

for (i in seq_along(deprecated)) {
  test_that(sprintf("Deprecated function %02i warns users", i), {
    expect_warning({
      deprecated[[i]]()
    })
  })
}