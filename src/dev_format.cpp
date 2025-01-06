#include "adf_file_info.h"
#include "block_adf.h"

[[cpp11::register]]
SEXP adf_dev_format(
    SEXP extptr, std::string name,
    bool ffs, bool intl, bool dircache,
    bool bootable) {
  AdfDevice * dev = get_adf_dev(extptr);
  if (dev->readOnly) cpp11::stop("Cannot format 'read-only' device.");
  uint8_t boot_code[1024] = {0};
  uint8_t vol_type = 0;
  
  if (!intl && dircache)
    Rf_warning("International mode is switched to TRUE as required for dircache mode");
  
  if (ffs) vol_type |= FSMASK_FFS;
  // It seems that OS >=3 does not allow intl=T when dircache=T
  // when directory cache mode is set to TRUE, this is only allowed in 'international mode'.
  // The flag for the international mode is turned off however
  if (intl && !dircache) vol_type |= FSMASK_INTL;
  if (dircache) vol_type |= FSMASK_DIRCACHE;
  
  if (dev->readOnly) cpp11::stop("Cannot format a write protected device");
  if (dev->nVol > 0) cpp11::stop("Cannot format a device with existing volumes");
  check_adf_name(name);
  const char * name_c = name.c_str();
  
  AdfVolume * vol = adfCreateVol(dev, 0, dev->cylinders, name_c, vol_type);
  if (!vol) cpp11::stop("Failed to format device");

  if (dev->devType == DEVTYPE_FLOPDD || dev->devType == DEVTYPE_FLOPHD) {
    
    set_adf_vol(extptr, 0);
    if (adfMountFlop(dev) != RC_OK ) cpp11::stop("Failed to mount floppy");
    free(vol->volName);
    free(vol);

  } else {

    if (adfMountHdFile(dev) != RC_OK)
      cpp11::stop("Failed to mount harddisk");
    set_adf_vol(extptr, 0);

  }
  
  if (bootable) {
    cpp11::raws bc = adf_bootable_code();
    for (int i = 0; i < bc.size(); i ++)
      boot_code[i + 12] = (uint8_t)(bc.at(i));
  }
  RETCODE rc;
  for (int i = 0; i < dev->nVol; i++) {
    AdfVolume * vol = adfMount(dev, i, dev->readOnly);
    rc = adfInstallBootBlock(vol, boot_code);
    rc = updateBootSum(vol);
    if (rc != RC_OK) cpp11::stop("Failed to install boot block");
  }

  return R_NilValue;
}
