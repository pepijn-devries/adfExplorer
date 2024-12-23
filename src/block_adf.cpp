#include "block_adf.h"

using namespace cpp11;

[[cpp11::register]]
SEXP read_adf_block_(SEXP extptr, int sector) {
  AdfDevice * dev = get_adf_dev(extptr);
  uint8_t buf[512] = {0};
  RETCODE rc = adfReadBlockDev(dev, sector, 512, buf);
  if (rc != RC_OK) Rf_error("Failed to read block");
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
  if (block.size() != 512) Rf_error("Unexpected block size");
  if (dev->readOnly) Rf_error("Cannot write to read only device");
  uint8_t buf[512];
  for (int i = 0; i < 512; i++) {
    buf[i] = block.at(i);
  }
  RETCODE rc = adfWriteBlockDev(dev, sector, 512, buf);
  if (rc != RC_OK) Rf_error("Failed to write block");
  return R_NilValue;
}

list interpret_file_header_internal(AdfDevice *dev, int vol_num, int sectnum) {
  check_volume_number(dev, vol_num);
  AdfVolume * vol = dev->volList[vol_num];
  uint8_t buf[512] = {0};
  bEntryBlock * entry = (bEntryBlock *) buf;
  RETCODE rc = adfReadEntryBlock(vol, sectnum, entry);
  if (rc != RC_OK) Rf_error("Failed to read entry block");
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
  
  writable::list result({
    "type"_nm           = headerKey_to_str(fheader->type),
      "headerKey"_nm    = (int)fheader->headerKey,
      "highSeq"_nm      = (int)fheader->highSeq,
      "dataSize"_nm     = (int)fheader->dataSize,
      "firstData"_nm    = (int)fheader->firstData,
      "checkSum"_nm     = (int)fheader->checkSum,
      "dataBlocks"_nm   = datab,
      "access"_nm       = access_from_int(fheader->access),
      "byteSize"_nm     = (int)fheader->byteSize,
      "comment"_nm      = r_string(comment),
      "modified"_nm     = dmt_to_POSIXct(fheader->days, fheader->mins, fheader->ticks),
      "filename"_nm     = r_string(name),
      "real"_nm         = (int)fheader->real,
      "nextLink"_nm     = (int)fheader->nextLink,
      "nextSameHash"_nm = (int)fheader->nextSameHash,
      "parent"_nm       = (int)fheader->parent,
      "extension"_nm    = (int)fheader->extension,
      "secType"_nm      = secType_to_str(fheader->secType)});
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
  if (rc != RC_OK) Rf_error("Failed to read entry block");
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
  
  writable::list result({
    "type"_nm           = headerKey_to_str(dheader->type),
      "sector"_nm       = (int)dheader->headerKey,
      "highSeq"_nm      = (int)dheader->highSeq,
      "checkSum"_nm     = (int)dheader->checkSum,
      "hashTable"_nm    = hashtab,
      "access"_nm       = access_from_int(dheader->access),
      "comment"_nm      = r_string(comment),
      "modified"_nm     = dmt_to_POSIXct(dheader->days, dheader->mins, dheader->ticks),
      "dirname"_nm      = r_string(name),
      "real"_nm         = (int)dheader->real,
      "nextLink"_nm     = (int)dheader->nextLink,
      "nextSameHash"_nm = (int)dheader->nextSameHash,
      "parent"_nm       = (int)dheader->parent,
      "extension"_nm    = (int)dheader->extension,
      "secType"_nm      = secType_to_str(dheader->secType)});
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
  if (rc != RC_OK) Rf_error("Failed to read root block");
  
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
  
  writable::list result({
    "type"_nm    = headerKey_to_str(root->type),
      "headerKey"_nm  = root->headerKey,
      "highSeq"_nm      = (int)root->highSeq,
      "firstData"_nm    = (int)root->firstData,
      "checkSum"_nm     = (int)root->checkSum,
      "hashTable"_nm    = hashtab,
      "bitmapFlag"_nm   = (r_bool)(root->bmFlag == -1),
      "bmPages"_nm      = bmpag,
      "bmExt"_nm        = (int)root->bmExt,
      "creation"_nm     = dmt_to_POSIXct(root->cDays, root->cMins, root->cTicks),
      "diskName"_nm     = r_string(diskname),
      "access"_nm       = dmt_to_POSIXct(root->days, root->mins, root->ticks),
      "creation_o"_nm   = dmt_to_POSIXct(root->coDays, root->coMins, root->coTicks),
      "nextSameHash"_nm = (int)root->nextSameHash,
      "parent"_nm       = (int)root->parent,
      "extension"_nm    = (int)root->extension,
      "secType"_nm      = secType_to_str(root->secType)});
  
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
  writable::logicals result({
    "D"_nm = (r_bool)hasD(access),
      "E"_nm = (r_bool)hasE(access),
      "W"_nm = (r_bool)hasW(access),
      "R"_nm = (r_bool)hasR(access),
      "A"_nm = (r_bool)hasA(access),
      "P"_nm = (r_bool)hasP(access),
      "S"_nm = (r_bool)hasS(access),
      "H"_nm = (r_bool)hasH(access)
  });
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
