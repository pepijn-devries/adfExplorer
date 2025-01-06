#include <sstream>
#include "adf_file_info.h"

[[cpp11::register]]
std::string adf_upper(std::string x, bool intl) {
  char* upper = new char[x.length() + 1];
  const char * input = x.c_str();
  BOOL intl_b = intl ? TRUE : FALSE;
  adfStrToUpper((uint8_t *)upper, (uint8_t *)input,
                x.length(), intl_b);
  std::string result = upper;
  delete[] upper;
  return result;
}

bool adf_check_volume(AdfDevice * dev, std::string vol_name,
                      int & cur_vol, int & cur_pos) {
  if (vol_name.length() == 0) return false;
  for (int i = 0; i < dev->nVol; i++) {
    bool intl = isINTL(dev->volList[i]->dosType) == TRUE ||
      isDIRCACHE(dev->volList[i]->dosType) == TRUE;
    std::string vol_name2 = adf_upper(vol_name, intl);
    
    if (std::string("SYS").compare(vol_name2) == 0) {
      cur_pos = dev->volList[i]->rootBlock;
      return true;
    }
    std::string num_dev;
    if (dev->devType == DEVTYPE_FLOPDD ||
        dev->devType == DEVTYPE_FLOPHD) {
      num_dev = "DF" + std::to_string(i);
    } else if (dev->devType == DEVTYPE_HARDDISK ||
      dev->devType == DEVTYPE_HARDFILE) {
      num_dev = "DH" + std::to_string(i);
    }
    
    std::string dev_vol_name = adf_upper(adf_dev_name_internal(dev, i), intl);

    if (vol_name2.compare(dev_vol_name) == 0 ||
        num_dev.compare(vol_name2) == 0) {
      cur_vol = i;
      cur_pos = dev->volList[i]->rootBlock;
      return true;
    }
  }
  return false;
}

[[cpp11::register]]
list adf_path_to_entry(SEXP extptr, std::string filename, int mode) {
  
  std::string entry_name = "";
  writable::list result({
    "volume"_nm = (int)-1,
      "sector"_nm = (int)-1,
      "header_sectype"_nm = (int)-1,
      "parent"_nm = (int)-1,
      "name"_nm = writable::strings(r_string(entry_name)),
      "remainder"_nm = writable::strings(r_string(entry_name))});
  
  AdfDevice * dev = get_adf_dev(extptr);
  int cur_vol     = get_adf_vol(extptr);
  check_volume_number(dev, cur_vol);
  int parent      = -1;
  SECTNUM cur_pos = dev->volList[cur_vol]->curDirPtr;
  auto vol = dev->volList[cur_vol];

  std::stringstream ss(filename);
  
  if (filename.find(":") != std::string::npos) {
    std::string vol_name;
    std::getline(ss, vol_name, ':');
    if (!adf_check_volume(dev, vol_name, cur_vol, cur_pos)) {
      const char* message = "Could not find the specified volume on device";
      if ((mode & ADF_FI_THROW_ERROR) != 0) cpp11::stop("%s", message); else {
        if ((mode & ADF_FI_WARN) != 0) Rf_warning("%s", message);
        return result;
      }
    } else {
      vol = dev->volList[cur_vol];
    }
  }

  std::string path_chunk;
  
  uint8_t entry_buf[512] = {0};
  bEntryBlock * entry = (bEntryBlock *)entry_buf;
  
  const char* message1 = "No entry block for current dir.";
  RETCODE rc = adfReadEntryBlock ( vol, cur_pos, entry );
  if (rc != RC_OK) {
    if ((mode & ADF_FI_THROW_ERROR) != 0) cpp11::stop("%s", message1); else {
      if ((mode & ADF_FI_WARN) != 0) Rf_warning("%s", message1);
      return result;
    }
  }    
  while (std::getline(ss, path_chunk, '/')) {
    RETCODE rc = adfReadEntryBlock ( vol, cur_pos, entry );
    if (rc != RC_OK) {
      if ((mode & ADF_FI_THROW_ERROR) != 0) cpp11::stop("%s", message1); else {
        if ((mode & ADF_FI_WARN) != 0) Rf_warning("%s", message1);
        return result;
      }
    }
    parent = cur_pos;
    cur_pos = adfGetEntryByName ( vol, cur_pos, path_chunk.c_str(), entry );
    if (cur_pos < vol->firstBlock || cur_pos > vol->lastBlock) {
      const char* message = "Path not found.";
      if ((mode & (ADF_FI_EXPECT_EXIST | ADF_FI_THROW_ERROR)) != 0) {
        cpp11::stop("%s", message);
      } else {
        if ((mode & ADF_FI_WARN) != 0) Rf_warning("%s", message);
        result["remainder"] = writable::strings(r_string(path_chunk));
        break;
      }
    }
  }

  int nl = entry->nameLen;
  if (nl > MAXNAMELEN) nl = MAXNAMELEN;
  
  entry_name = (std::string)(std::string(entry->name).substr(0, nl));
  int entry_sectype = entry->secType;
  
  result["name"]           = writable::strings(r_string(entry_name));
  result["volume"]         = as_sexp(cur_vol);
  result["sector"]         = as_sexp((int)cur_pos);
  result["header_sectype"] = as_sexp((int)entry_sectype);
  result["parent"]         = as_sexp((int)parent);
  
  
  if (((mode & ADF_FI_EXPECT_FILE) != 0) &&
      entry_sectype != ST_FILE) {
    const char * message = "Path does not point to a file";
    if ((mode & ADF_FI_THROW_ERROR) != 0)
      cpp11::stop("%s", message); else if (mode & ADF_FI_WARN)
        Rf_warning("%s", message);
  }
  if (((mode & ADF_FI_EXPECT_DIR) != 0) &&
      entry_sectype != ST_DIR && entry_sectype != ST_ROOT) {
    const char * message = "Path does not point to a directory";
    if ((mode & ADF_FI_THROW_ERROR) != 0)
      cpp11::stop("%s", message); else if (mode & ADF_FI_WARN)
        Rf_warning("%s", message);
  }

  return result;
}

void adf_change_dir_internal(SEXP extptr, SECTNUM sector, int volume) {
  AdfDevice * dev = get_adf_dev(extptr);
  check_volume_number(dev, volume);
  AdfVolume * vol = dev->volList[volume];
  vol->curDirPtr = sector;
}

void check_adf_name(std::string name) {
  if (name.find(':') != std::string::npos ||
      name.find('/') != std::string::npos)
    cpp11::stop("File, directory or volume names cannot contain `/` or `:`.");
}

[[cpp11::register]]
bool adf_file_exists_(SEXP extptr, std::string path) {
  list entry = adf_path_to_entry(extptr, path, 0);
  return integers(entry["sector"]).at(0) != -1;
}

[[cpp11::register]]
bool adf_dir_exists_(SEXP extptr, std::string path) {
  list entry = adf_path_to_entry(extptr, path, 0);
  int sec_type = integers(entry["header_sectype"]).at(0);
  return integers(entry["sector"]).at(0) != -1 &&
    (sec_type == ST_DIR || sec_type == ST_ROOT);
}

[[cpp11::register]]
list adf_entry_info_(SEXP extptr, std::string path) {
  int sector, vol_num, sectype;
  writable::list result;
  if (Rf_inherits(extptr, "adf_device")) {
    AdfDevice * dev = get_adf_dev(extptr);
    
    list entry = adf_path_to_entry(extptr, path, 0);
    sector  = integers(entry["sector"]).at(0);
    vol_num = integers(entry["volume"]).at(0);
    sectype = integers(entry["header_sectype"]).at(0);
    check_volume_number(dev, vol_num);
    if (sectype == ST_ROOT) result = interpret_root_header(extptr, vol_num);
    if (sectype == ST_DIR)  result = interpret_dir_header(extptr, vol_num, sector);
    if (sectype == ST_FILE) result = interpret_file_header(extptr, vol_num, sector);
  } else if (Rf_inherits(extptr, "adf_file_con")) {
    AdfFile *af = get_adffile(extptr);
    sector = af->fileHdr->headerKey;
    sectype = af->fileHdr->secType;
    vol_num = get_adf_file_volnum(af);
    AdfDevice * dev = af->volume->dev;
    if (sectype == ST_ROOT) result = interpret_root_header_internal(dev, vol_num);
    if (sectype == ST_DIR)  result = interpret_dir_header_internal(dev, vol_num, sector);
    if (sectype == ST_FILE) result = interpret_file_header_internal(dev, vol_num, sector);
  } else {
    cpp11::stop("External pointer should by of class `adf_device` or `adf_file_con`.");
  }
  
  return result;
}

std::string adf_entry_to_path_internal(AdfDevice * dev, int vol_num, int sectnum, bool full) {
  check_volume_number(dev, vol_num);
  AdfVolume * vol = dev->volList[vol_num];
  
  uint8_t buf[512] = {0};
  bEntryBlock * entry = (bEntryBlock *) buf;
  
  std::string result("");
  int fail_safe = 0;
  int sn = sectnum;
  do {
    RETCODE rc = adfReadEntryBlock(vol, sn, entry);
    if (rc != RC_OK) cpp11::stop("Failed to read entry block");
    
    uint8_t namelen = entry->nameLen;
    if (namelen > MAXNAMELEN) {
      namelen = MAXNAMELEN;
      Rf_warning("Faulty entry name length. Entry name is truncated");
    }
    char name[MAXNAMELEN+3] = {0};
    memcpy(name, entry->name, namelen);
    std::string sep("");
    if (entry->secType == ST_DIR) sep = "/";
    if (entry->secType == ST_ROOT) sep = ":";
    result = std::string(name) + sep + result;
    fail_safe++;
    sn = entry->parent;
  } while (entry->secType != ST_ROOT && full && fail_safe < 1000);
  if (fail_safe == 1000) cpp11::stop("Unrealistically deep path");
  return result;
}

std::string adf_entry_to_path(SEXP extptr, int vol_num, int sectnum, bool full) {
  AdfDevice * dev = get_adf_dev(extptr);
  return adf_entry_to_path_internal(dev, vol_num, sectnum, full);
}

[[cpp11::register]]
list adf_con_summary(SEXP extptr) {
  AdfFile * af = get_adffile(extptr);
  int nl = af->fileHdr->nameLen;
  if (nl > MAXNAMELEN) nl = MAXNAMELEN;
  writable::list result({
    "description"_nm = (std::string)(std::string(af->fileHdr->fileName).substr(0, nl)),
    "class"_nm = "adf_file_con",
    "mode"_nm = af->modeWrite ? "r+b" : "rb",
    "text"_nm = "binary",
    "opened"_nm = "opened",
    "can read"_nm = af->modeRead ? "yes" : "no",
    "can write"_nm = af->modeWrite ? "yes" : "no"
  });

  return result;
}
