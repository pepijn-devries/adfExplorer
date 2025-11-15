library(adfExplorer)
library(AmigaFFH)
library(ProTrackR2)

startup <-
  c("; The Startup-Sequence is executed after booting",
    "; Everything after semicolons are comments and is ignored",
    "; By default standard commands are loaded from",
    "; the ROM kickstart. Additional commands should be",
    "; stored on the disk in the SYS:C directory.",
    "; For demonstration purposes we only echo some",
    "; text to the screen... Note that this will not",
    "; work on Amiga OS <2.0 as \"Echo\" is not available",
    "; in older ROM kickstart versions.",
    "",
    "Echo \"\033c\033[22m\033[32mADF Explorer Example Disk\" ; Note that the weird characters at",
    "the start are escape-codes to format the text",
    "Echo \"\033[0mThis disk was created as an example for the\"",
    "Echo \"R package 'adfExplorer' by Pepijn de Vries.\"") |>
  paste(collapse = "\n")

df <- file.path(tempdir(), "example.adf")
disk <- create_adf_device(df, write_protected = FALSE)
prepare_adf_device(disk, "adfExampleOFS", ffs = FALSE)

make_adf_dir(disk, "Devs")
con <- adf_file_con(disk, "Devs/system-configuration", writable = TRUE)
writeBin(
  simpleSysConfig() |> as.raw(),
  con
)
close(con)

make_adf_dir(disk, "S")
con <- adf_file_con(disk, "S/Startup-Sequence", writable = TRUE)
writeBin(startup |> charToRaw(), con)
close(con)

for (i in 1:5) make_adf_dir(disk, "this/is/a/deep/path")

make_adf_dir(disk, "mods")
con <- adf_file_con(disk, "mods/mod.intro", writable = TRUE)
mod <- pt2_read_mod(pt2_demo()) |> as.raw()
writeBin(mod, con)
close(con)

con <- adf_file_con(disk, "easter egg.txt", writable = TRUE)
writeBin("Have you found me yet?" |> charToRaw(), con)
close(con)

remove_adf_entry(disk, "easter egg.txt")

close(disk)
compress_adf(df, file.path("inst", "example.adz"))
