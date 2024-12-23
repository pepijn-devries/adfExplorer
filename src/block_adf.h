#ifndef __BLOCK_ADF__
#define __BLOCK_ADF__
#pragma once

#include "dev_info.h"
using namespace cpp11;

std::string headerKey_to_str(int headerKey);
std::string secType_to_str(int secType);
SEXP dmt_to_POSIXct(int days, int minutes, int ticks);
logicals access_from_int(int access);
list interpret_root_header_internal(AdfDevice *dev, int vol_num);
list interpret_file_header_internal(AdfDevice *dev, int vol_num, int sectnum);
list interpret_dir_header_internal(AdfDevice *dev, int vol_num, int sectnum);
SEXP read_adf_block_(SEXP extptr, int sector);
SEXP write_adf_block_(SEXP extptr, int sector, raws block);
list interpret_root_header(SEXP extptr, int vol_num);
list interpret_file_header(SEXP extptr, int vol_num, int sectnum);
list interpret_dir_header(SEXP extptr, int vol_num, int sectnum);
raws adf_bootable_code(void);
RETCODE updateBootSum ( struct AdfVolume * const vol );

#endif /* __BLOCK_ADF__ */
