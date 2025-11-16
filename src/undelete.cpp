#include "adf_file_info.h"

[[cpp11::register]]
list dumpster_dive(SEXP extptr, int vol_num) {
  AdfDevice * dev = get_adf_dev(extptr);
  check_volume_number(dev, vol_num);
  AdfVolume * vol = dev->volList[vol_num];
  writable::strings dumpster_names({
    "sect", "parent", "type", "secType", "name"
  });
  writable::integers sect((R_xlen_t)0);
  writable::integers parent((R_xlen_t)0);
  writable::integers type((R_xlen_t)0);
  writable::integers secType((R_xlen_t)0);
  writable::strings name((R_xlen_t)0);
  
  AdfList *root, * alist;
  root = alist = adfGetDelEnt ( vol );
  GenBlock * gb;
  while (alist) {
    gb = (GenBlock *)alist->content;
    sect.push_back(gb->sect);
    parent.push_back(gb->parent);
    type.push_back(gb->type);
    secType.push_back(gb->secType);
    name.push_back(gb->name);
    alist = alist->next;
  }
  
  adfFreeDelList(root);
  
  writable::list result({ sect, parent, type, secType, name });
  
  result.attr("names") = dumpster_names;
  result.attr("class") = "data.frame";
  result.attr("row.names") = sect;
  return result;
}

[[cpp11::register]]
SEXP undelete_adf_entry(SEXP extptr, int vol_num, int sect) {
  AdfDevice * dev = get_adf_dev(extptr);
  list dumpster = dumpster_dive(extptr, vol_num);
  writable::integers sects(dumpster["sect"]);
  writable::integers par(dumpster["parent"]);
  int j = -1;
  for (int i = 0; i < sects.size(); i++) {
    if (sects[i] == sect) {
      j = i;
      break;
    }
  }
  if (j < 0) stop("No entry found at `sect`");
  SECTNUM dest_sect = par[j];

  check_volume_number(dev, vol_num);
  AdfVolume * vol = dev->volList[vol_num];
  
  RETCODE rc = adfUndelEntry(vol, dest_sect, sect);
  
  if (rc != RC_OK) stop("Failed to salvage entry");

  return extptr;
}