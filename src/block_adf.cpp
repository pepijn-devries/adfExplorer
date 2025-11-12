#include "block_adf.h"

using namespace cpp11;

[[cpp11::register]]
SEXP read_adf_block_(SEXP extptr, int sector) {
  AdfDevice * dev = get_adf_dev(extptr);
  uint8_t buf[512] = {0};
  RETCODE rc = adfReadBlockDev(dev, sector, 512, buf);
  if (rc != RC_OK) cpp11::stop("Failed to read block");
  writable::raws result_raw((R_xlen_t)512);
  for (int i = 0; i < result_raw.size(); i++)
    result_raw.at(i) = buf[i];
  
  sexp result = as_sexp(result_raw);
  
  result.attr("class") = "adf_block";
  
  return result;
}

[[cpp11::register]]
SEXP write_adf_block_(SEXP extptr, int sector, raws block) {
  AdfDevice * dev = get_adf_dev(extptr);
  if (block.size() != 512) cpp11::stop("Unexpected block size");
  if (dev->readOnly) cpp11::stop("Cannot write to read only device");
  uint8_t buf[512];
  for (int i = 0; i < 512; i++) {
    buf[i] = block.at(i);
  }
  RETCODE rc = adfWriteBlockDev(dev, sector, 512, buf);
  if (rc != RC_OK) cpp11::stop("Failed to write block");
  return R_NilValue;
}

list interpret_file_header_internal(AdfDevice *dev, int vol_num, int sectnum) {
  check_volume_number(dev, vol_num);
  AdfVolume * vol = dev->volList[vol_num];
  uint8_t buf[512] = {0};
  bEntryBlock * entry = (bEntryBlock *) buf;
  RETCODE rc = adfReadEntryBlock(vol, sectnum, entry);
  if (rc != RC_OK) cpp11::stop("Failed to read entry block");
  bFileHeaderBlock * fheader = (bFileHeaderBlock *) entry;
  
  writable::integers datab(MAX_DATABLK);
  for (int i = 0; i < datab.size(); i++) datab[i] = fheader->dataBlocks[i];
  
  uint8_t commlen = fheader->commLen;
  if (commlen > MAXCMMTLEN) {
    commlen = MAXCMMTLEN;
    Rf_warning("Faulty comment length. Comment text is truncated");
  }
  char comment[MAXCMMTLEN+3] = {0};
  memcpy(comment, fheader->comment, commlen);
  
  uint8_t namelen = fheader->nameLen;
  if (namelen > MAXNAMELEN) {
    namelen = MAXNAMELEN;
    Rf_warning("Faulty file name length. File name is truncated");
  }
  char name[MAXNAMELEN+3] = {0};
  memcpy(name, fheader->fileName, namelen);
  
  writable::strings hdr_names({
    "type", "headerKey", "highSeq", "dataSize", "firstData", "checkSum",
    "dataBlocks", "access", "byteSize", "comment", "modified", "filename",
    "real", "nextLink", "nextSameHash", "parent", "extension", "secType"
  });
  writable::list result({
    writable::strings((r_string)headerKey_to_str(fheader->type)),
    as_sexp((int)fheader->headerKey),
    as_sexp((int)fheader->highSeq),
    as_sexp((int)fheader->dataSize),
    as_sexp((int)fheader->firstData),
    as_sexp((int)fheader->checkSum),
    datab,
    access_from_int(fheader->access),
    as_sexp((int)fheader->byteSize),
    r_string(comment),
    dmt_to_POSIXct(fheader->days, fheader->mins, fheader->ticks),
    r_string(name),
    as_sexp((int)fheader->real),
    as_sexp((int)fheader->nextLink),
    as_sexp((int)fheader->nextSameHash),
    as_sexp((int)fheader->parent),
    as_sexp((int)fheader->extension),
    as_sexp(secType_to_str(fheader->secType))});
  result.attr("names") = hdr_names;
  return result;
}

[[cpp11::register]]
list interpret_file_header(SEXP extptr, int vol_num, int sectnum) {
  AdfDevice * dev = get_adf_dev(extptr);
  return interpret_file_header_internal(dev, vol_num, sectnum);
}

list interpret_dir_header_internal(AdfDevice *dev, int vol_num, int sectnum) {
  check_volume_number(dev, vol_num);
  AdfVolume * vol = dev->volList[vol_num];
  uint8_t buf[512] = {0};
  bEntryBlock * entry = (bEntryBlock *) buf;
  RETCODE rc = adfReadEntryBlock(vol, sectnum, entry);
  if (rc != RC_OK) cpp11::stop("Failed to read entry block");
  bDirBlock * dheader = (bDirBlock *) entry;
  
  writable::integers hashtab(HT_SIZE);
  for (int i = 0; i < hashtab.size(); i++) hashtab[i] = dheader->hashTable[i];
  
  uint8_t commlen = dheader->commLen;
  if (commlen > MAXCMMTLEN) {
    commlen = MAXCMMTLEN;
    Rf_warning("Faulty comment length. Comment text is truncated");
  }
  char comment[MAXCMMTLEN+3] = {0};
  memcpy(comment, dheader->comment, commlen);
  
  uint8_t namelen = dheader->nameLen;
  if (namelen > MAXNAMELEN) {
    namelen = MAXNAMELEN;
    Rf_warning("Faulty file name length. File name is truncated");
  }
  char name[MAXNAMELEN+3] = {0};
  memcpy(name, dheader->dirName, namelen);
  
  writable::strings hdr_names({
    "type", "sector", "highSeq", "checkSum", "hashTable", "access", "comment",
    "modified", "dirname", "real", "nextLink", "nextSameHash", "parent",
    "extension", "secType"
  });
  writable::list result({
    as_sexp(headerKey_to_str(dheader->type)),
    as_sexp((int)dheader->headerKey),
    as_sexp((int)dheader->highSeq),
    as_sexp((int)dheader->checkSum),
    hashtab,
    access_from_int(dheader->access),
    r_string(comment),
    dmt_to_POSIXct(dheader->days, dheader->mins, dheader->ticks),
    r_string(name),
    as_sexp((int)dheader->real),
    as_sexp((int)dheader->nextLink),
    as_sexp((int)dheader->nextSameHash),
    as_sexp((int)dheader->parent),
    as_sexp((int)dheader->extension),
    as_sexp(secType_to_str(dheader->secType))});
  result.attr("names") = hdr_names;
  return result;
}

[[cpp11::register]]
list interpret_dir_header(SEXP extptr, int vol_num, int sectnum) {
  AdfDevice * dev = get_adf_dev(extptr);
  return interpret_dir_header_internal(dev, vol_num, sectnum);
}

list interpret_root_header_internal(AdfDevice *dev, int vol_num) {
  check_volume_number(dev, vol_num);
  AdfVolume * vol = dev->volList[vol_num];
  uint8_t buf_root[512] = {0};
  bRootBlock * root = (bRootBlock *) buf_root;
  RETCODE rc = adfReadRootBlock(vol, vol->rootBlock, root);
  if (rc != RC_OK) cpp11::stop("Failed to read root block");
  
  uint8_t namelen = root->nameLen;
  if (namelen > MAXNAMELEN) {
    namelen = MAXNAMELEN;
    Rf_warning("Faulty namelength. Disk name is truncated");
  }
  char diskname[35] = {0};
  memcpy(diskname, root->diskName, namelen);
  
  writable::integers hashtab(HT_SIZE);
  for (int i = 0; i < hashtab.size(); i++) hashtab[i] = root->hashTable[i];
  
  writable::integers bmpag(BM_SIZE);
  for (int i = 0; i < bmpag.size(); i++) bmpag[i] = root->bmPages[i];
  
  writable::strings hdr_names({
    "type", "headerKey", "highSeq", "firstData", "checkSum", "hashTable", "bitmapFlag",
    "bmPages", "bmExt", "creation", "diskName", "access", "creation_o", "nextSameHash",
    "parent", "extension", "secType"
  });
  writable::list result({
    as_sexp(headerKey_to_str(root->type)),
      as_sexp(root->headerKey),
      as_sexp((int)root->highSeq),
      as_sexp((int)root->firstData),
      as_sexp((int)root->checkSum),
      hashtab,
      writable::logicals({(r_bool)(root->bmFlag == -1)}),
      bmpag,
      as_sexp((int)root->bmExt),
      dmt_to_POSIXct(root->cDays, root->cMins, root->cTicks),
      r_string(diskname),
      dmt_to_POSIXct(root->days, root->mins, root->ticks),
      dmt_to_POSIXct(root->coDays, root->coMins, root->coTicks),
      as_sexp((int)root->nextSameHash),
      as_sexp((int)root->parent),
      as_sexp((int)root->extension),
      as_sexp(secType_to_str(root->secType))});
  result.attr("names") = hdr_names;
  return result;
}

[[cpp11::register]]
list interpret_root_header(SEXP extptr, int vol_num) {
  AdfDevice * dev = get_adf_dev(extptr);
  return interpret_root_header_internal(dev, vol_num);
}

std::string headerKey_to_str(int headerKey) {
  std::string result;
  switch(headerKey) {
  case 0:
    result = "NULL";
    break;
  case T_HEADER:
    result = "HEADER";
    break;
  case T_LIST:
    result = "LIST";
    break;
  case T_DATA:
    result = "DATA";
    break;
  case T_DIRC:
    result = "DIRC";
    break;
  default:
    result = "Unknown";
  break;
  }
  return result;
}

std::string secType_to_str(int secType) {
  std::string result;
  switch(secType) {
  case 0:
    result = "NULL";
    break;
  case ST_ROOT:
    result = "ROOT";
    break;
  case ST_DIR:
    result = "DIR";
    break;
  case ST_FILE:
    result = "FILE";
    break;
  case ST_LFILE:
    result = "LFILE";
    break;
  case ST_LDIR:
    result = "LDIR";
    break;
  case ST_LSOFT:
    result = "LSOFT";
    break;
  default:
    result = "Unknown";
  break;
  }
  return result;
}

SEXP dmt_to_POSIXct(int days, int minutes, int ticks) {
  // POSIXct is seconds since 1970-01-01 00:00
  // Amiga date-time is measured from origin 1978-01-01
  double result_d = 252460800 + days*86400 + minutes*60 + ticks/50;
  if (minutes >  1440 || ticks > 3000) Rf_warning("Corrupt date time data");
  sexp result = as_sexp(doubles({result_d}));
  
  result.attr("class") = strings({"POSIXct", "POSIXt"});
  result.attr("tzone") = "";
  
  return result;
}

logicals access_from_int(int access) {
  writable::strings acc_names({
    "D", "E", "W", "R", "A", "P", "S", "H"
  });
  writable::logicals result({
    (r_bool)hasD(access), (r_bool)hasE(access), (r_bool)hasW(access),
      (r_bool)hasR(access), (r_bool)hasA(access), (r_bool)hasP(access),
      (r_bool)hasS(access), (r_bool)hasH(access)
  });
  result.attr("names") = acc_names;
  return result;
}

raws adf_bootable_code(void) {
  return raws({
    0x43, 0xfa, 0x00, 0x3e, 0x70, 0x25, 0x4e, 0xae, 0xfd, 0xd8, 0x4a, 0x80, 0x67, 0x0c,
    0x22, 0x40, 0x08, 0xe9, 0x00, 0x06, 0x00, 0x22, 0x4e, 0xae, 0xfe, 0x62, 0x43, 0xfa,
    0x00, 0x18, 0x4e, 0xae, 0xff, 0xa0, 0x4a, 0x80, 0x67, 0x0a, 0x20, 0x40, 0x20, 0x68,
    0x00, 0x16, 0x70, 0x00, 0x4e, 0x75, 0x70, 0xff, 0x4e, 0x75, 0x64, 0x6f, 0x73, 0x2e,
    0x6c, 0x69, 0x62, 0x72, 0x61, 0x72, 0x79, 0x00, 0x22, 0x65, 0x78, 0x70, 0x61, 0x6e,
    0x73, 0x69, 0x6f, 0x6e, 0x2e, 0x6c, 0x69, 0x62, 0x72, 0x61, 0x72, 0x79
  });
}

RETCODE updateBootSum ( struct AdfVolume * const vol ) {
  uint8_t buf[1024];
  RETCODE rc = RC_OK;
  
  rc = adfReadBlock (vol, 0, buf);
  if (rc == RC_OK) rc = adfReadBlock (vol, 1, buf + LOGICAL_BLOCK_SIZE);
  if (rc != RC_OK) return rc;
  
  uint32_t calc_checksum = adfBootSum(buf);
  swLong(buf+4, calc_checksum);
  
  rc = adfWriteBlock ( vol, 0, buf );
  if ( rc != RC_OK )
    return rc;
  
  rc = adfWriteBlock ( vol, 1, buf + 512 );
  return rc;
}
