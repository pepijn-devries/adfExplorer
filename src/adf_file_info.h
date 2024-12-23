#ifndef __FILE_INFO_ADF__
#define __FILE_INFO_ADF__
#pragma once

#include "block_adf.h"
#include "open_adf_file.h"
#include "dev_info.h"
#include <cpp11.hpp>
using namespace cpp11;

/* masks for different modes for getting entry from path name */
#define ADF_FI_EXPECT_FILE                0b1
#define ADF_FI_EXPECT_DIR                0b10
#define ADF_FI_THROW_ERROR              0b100
#define ADF_FI_WARN                    0b1000
#define ADF_FI_EXPECT_EXIST           0b10000
#define ADF_FI_EXPECT_VALID_CHECKSUM 0b100000

list adf_path_to_entry(SEXP extptr, std::string filename, int mode);
void adf_change_dir_internal(SEXP extptr, SECTNUM sector, int volume);
void check_adf_name(std::string name);
std::string adf_entry_to_path(SEXP extptr, int vol_num, int sectnum, bool full);
std::string adf_entry_to_path_internal(AdfDevice * dev, int vol_num, int sectnum, bool full);

#endif /* __FILE_INFO_ADF__ */
