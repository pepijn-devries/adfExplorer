#pragma once
#ifndef __DEV_INFO__
#define __DEV_INFO__

#include <cpp11.hpp>
#include "open_adf.h"

void check_volume_number(AdfDevice *dev, int vol_num);
int get_adf_vol(SEXP extptr);
AdfContainer * getAC(SEXP extptr);
AdfDevice * get_adf_dev(SEXP extptr);
void set_adf_vol(SEXP extptr, int cur_vol);
std::string adf_dev_name_internal(AdfDevice * dev, int vol_num);
bool adf_set_dev_name(SEXP extptr, int vol_num, std::string new_name);

#endif /* __DEV_INFO__ */
