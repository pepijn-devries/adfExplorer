#include <cpp11.hpp>
#include "open_adf.h"
#include "dev_info.h"
#include "adf_file_info.h"
#include "open_adf_file.h"
using namespace cpp11;

std::vector<AdfFileContainer *> openfiles;

void closeAdfFileInternal(AdfFileContainer * afc) {
  if (afc->isopen) {
    AdfFile * f = afc->f;
    adfFileFlush(f);
    adfFileClose(f); // This closes the adf file and frees all allocated mem for f
    afc->isopen = false;
    for (long unsigned i = 0; i < openfiles.size(); i++) {// it = openfiles.begin(); it != openfiles.end(); ++it) {
      AdfFileContainer * afc = openfiles.at(i);
      if (afc->f == f) {
        openfiles.erase(openfiles.begin() + i);
        break;
      }
    }
  }
}

[[cpp11::register]]
void adf_close_file_con(SEXP extptr) {
  AdfFileContainer * afc = get_adffilecontainer(extptr);
  closeAdfFileInternal(afc);
}

void freeAdfFileContainer(AdfFileContainer * afc) {
  closeAdfFileInternal(afc);
  delete afc;
  return;
}

AdfFileContainer * get_adffilecontainer(SEXP extptr) {
  bool success = TYPEOF(extptr) == EXTPTRSXP && Rf_inherits(extptr, "adf_file_con");
  if (success) {
    AdfFileContainer * afc = reinterpret_cast<AdfFileContainer *>(R_ExternalPtrAddr(extptr));
    if (afc->isopen) return afc;
  }
  cpp11::stop("Object should be an external pointer and inherit 'adf_file_con'.");
  return NULL;
}

AdfFile * get_adffile(SEXP extptr) {
  AdfFileContainer * afc = get_adffilecontainer(extptr);
  return afc->f;
}

static double adf_seek_internal(AdfFile * af, double where, int origin) {
  
  // origin 1 = start;
  // origin 2 = current;
  // origin 3 = end of file;
  
  int filesize = af->fileHdr->byteSize;
  int pos      = af->pos;
  
  if (ISNA(where)) return pos;
  
  switch(origin) {
  case 2: break;
  case 3: pos += (int)where; break;
  default: pos = (int)where;
  }
  if (pos < 0) pos = 0;
  if (pos > filesize) pos = filesize;
  adfFileSeek(af, pos);
  return (double)pos;
}

[[cpp11::register]]
double seek_adf(SEXP extptr, double where, int origin) {
  AdfFile *af = get_adffile(extptr);
  return adf_seek_internal(af, where, origin);
}

bool check_adf_file_state(AdfFile * adf_file) {
  for(const AdfFileContainer * afc : openfiles) {
    if (afc->f == adf_file) return true;
  }
  return false;
}

int get_adf_file_volnum(AdfFile * adf_file) {
  bool success = check_adf_file_state(adf_file);
  AdfDevice * dev = nullptr;
  AdfVolume * vol = nullptr;
  if (success) {
    vol = adf_file->volume;
    success = check_adf_volume_state(vol);
    if (success) {
      dev = vol->dev;
      success = check_adf_device_state(dev);
    }
  }
  if (success) {
    if (dev->nVol <= 0) return -1;
    int result = -1;
    for (int i = 0; i < dev->nVol; i++) {
      AdfVolume * test_vol = dev->volList[i];
      
      if (test_vol == vol) {
        result = i;
        break;
      }
    }
    return result;
  } else {
    cpp11::stop("Virtual device is no longer available!");
  }
  return -1;
}

bool adf_check_file_state(AdfDevice *dev, int vol, SECTNUM sect) {
  for (long unsigned i = 0; i < openfiles.size(); i++) {// it = openfiles.begin(); it != openfiles.end(); ++it) {
    AdfFileContainer * afc = openfiles.at(i);
    int testvol = get_adf_file_volnum(afc->f);
    if (dev == afc->f->volume->dev && testvol == vol && afc->f->fileHdr->headerKey == sect)
      return true;
  }
  return false;
}

[[cpp11::register]]
SEXP adf_file_con_(SEXP extptr, std::string filename, bool writable) {
  AdfDevice * dev = get_adf_dev(extptr);
  if (dev->readOnly && writable)
    cpp11::stop("Cannot open a writable connection from a write-protected disk");
  int mode = ADF_FI_EXPECT_FILE | ADF_FI_EXPECT_VALID_CHECKSUM;
  if (!writable) mode = mode | ADF_FI_EXPECT_EXIST | ADF_FI_THROW_ERROR;
  
  list entry_pos = adf_path_to_entry(extptr, filename, mode);
  int vol_num  = integers(entry_pos["volume"]).at(0);
  SECTNUM sect = integers(entry_pos["sector"]).at(0);
  SECTNUM parent = integers(entry_pos["parent"]).at(0);
  
  check_volume_number(dev, vol_num);
  bool file_check = adf_check_file_state(dev, vol_num, sect);
  if (file_check)
    cpp11::stop("Can only open 1 connection per file on a virtual device");

  auto vol = dev->volList[vol_num];
  int vol_old = get_adf_vol(extptr);
  SECTNUM cur_dir = vol->curDirPtr;
  
  adf_change_dir_internal(extptr, parent, vol_num);
  
  AdfFileMode fmode = writable ? ADF_FILE_MODE_WRITE : ADF_FILE_MODE_READ;
  std::string fns;
  
  if (writable && sect == -1) {
    // The remainder of the path that does not exist:
    fns = (std::string)(cpp11::strings(entry_pos["remainder"]).at(0));
    check_adf_name(fns);
    const char * fn2 = fns.c_str();
    auto fhead = new bFileHeaderBlock;
    RETCODE fcret = adfCreateFile(vol, parent, fn2, fhead);
    delete(fhead);
    if (fcret != RC_OK) cpp11::stop("Failed to create file for writing.");
  } else {
    fns = (std::string)cpp11::strings(entry_pos["name"]).at(0);
  }
  const char* fn = fns.c_str();
  AdfFile * adf_file = adfFileOpen (vol, fn, fmode);
  adf_change_dir_internal(extptr, cur_dir, vol_old);
  
  if (!adf_file) cpp11::stop("Failed to open file connection");
  
  AdfFileContainer * afc = (AdfFileContainer *)new AdfFileContainer;
  afc->f      = adf_file;
  afc->isopen = true;
  openfiles.push_back(afc);
  
  external_pointer<AdfFileContainer, freeAdfFileContainer>adfptr(afc);
  sexp result = as_sexp(adfptr);
  result.attr("class") = strings({"adf_file_con", "connection"});
  
  return result;
}

[[cpp11::register]]
std::string adf_file_con_info(SEXP extptr) {
  AdfFile *af = get_adffile(extptr);
  
  AdfDevice *parent = af->volume->dev;
  int vol_num = get_adf_file_volnum(af);
  
  std::string path = adf_entry_to_path_internal(
    parent, vol_num, af->fileHdr->headerKey, TRUE);
  
  std::string access = "read only";
  if (af->modeWrite) access = "writable";
  
  std::string result = "A " + access + " connection to virtual file:\n" + path;
  return result;
}

void close_adf_internal(AdfContainer * ac) {
  if (ac->isopen) {
    ac->isopen = false;
    AdfDevice * dev = ac->dev;
    for (long i = openfiles.size() - 1; i>= 0; i--) {
      if (i < 0) break;
      AdfFileContainer * afc = openfiles.at(i);
      if (afc->isopen && afc->f->volume->dev == dev)
        closeAdfFileInternal(afc);
    }
    if (dev->nVol > 0) {
      for (int i = 0; i < dev->nVol; i++) {
        adfUnMount(dev->volList[i]); // This frees memory reserved for the bitmaps
      }
    }
    adfCloseDev(dev); // This closes the adf file and frees all allocated mem for dev
  }
  return;
}

[[cpp11::register]]
void close_adf(SEXP extptr) {
  auto ac = getAC(extptr);
  close_adf_internal(ac);
  return;
}
