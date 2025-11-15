#include "adf_file_info.h"

[[cpp11::register]]
logicals get_bitmap(SEXP extptr, int vol_num) {
  AdfDevice * dev = get_adf_dev(extptr);
  check_volume_number(dev, vol_num);
  AdfVolume * vol = dev->volList[vol_num];
  int n = vol->lastBlock - vol->firstBlock;
  writable::logicals result((R_xlen_t)(n - 1));
  writable::strings nm((R_xlen_t)(n - 1));

  for (int i = 2; i <= n; i++) {
    int bl = i + vol->firstBlock;
    result[i - 2] = !adfIsBlockFree(vol, bl);
    nm[i - 2] = std::to_string(i);
  }
  result.attr("names") = nm;
  
  return result;
}