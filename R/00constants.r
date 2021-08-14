# Physical number of sides of a floppy disk
NUMBER_OF_SIDES        <- 2
# Number of cylinders on a floppy disk
NUMBER_OF_CYLINDERS    <- 80
# Number of sectors on a double density disk
NUMBER_OF_SECTORS_DD   <- 11
# Number of sectors on a high density disk
NUMBER_OF_SECTORS_HD   <- 22
# Size of a block in bytes (i.e., raw values):
BLOCK_SIZE             <- 512

# Primary block types on a Amiga DOS-formatted disk:
TYPES     <- data.frame(type = c("T_HEADER", "T_DATA", "T_LIST", "DIRCACHE"),
                        value = c (2, 8, 16, 33))

# Secondary (header) block types on a Amiga DOS-formatted disk:
SEC_TYPES <- data.frame(type = c("ST_ROOT", "ST_FILE", "ST_USERDIR", "ST_LINKFILE", "ST_LINKDIR", "ST_SOFTLINK"),
                        value = c(1, 0x100000000-3, 2, 0x100000000-4, 4, 3))
