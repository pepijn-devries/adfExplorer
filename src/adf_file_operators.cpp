#include "adf_file_info.h"

static size_t adf_file_read(AdfFile * af, size_t req_size, void *target) {
  int filesize = af->fileHdr->byteSize;
  int pos      = af->pos;
  size_t get_size = min(filesize - pos, (int)req_size);
  adfFileRead(af, get_size, (uint8_t *)target);
  return get_size;
}

static int adf_getc(AdfFile * af) {
  int x = 0;
#ifdef WORDS_BIGENDIAN
  return adf_file_read(af, 1, &x) ? BSWAP_32(x) : R_EOF;
#else
  return adf_file_read(af, 1, &x) ? x : R_EOF;
#endif
}

[[cpp11::register]]
SEXP adf_change_dir(SEXP extptr, std::string path) {
  int mode = ADF_FI_EXPECT_DIR | ADF_FI_THROW_ERROR | ADF_FI_EXPECT_EXIST |
    ADF_FI_EXPECT_VALID_CHECKSUM;
  
  list entry = adf_path_to_entry(extptr, path, mode);
  adf_change_dir_internal(
    extptr,
    (SECTNUM)integers(entry["sector"]).at(0),
    integers(entry["volume"]).at(0)
  );
  return R_NilValue;
}

[[cpp11::register]]
SEXP adf_get_current_dir(SEXP extptr) {
  AdfDevice * dev = get_adf_dev(extptr);
  int cur_vol = get_adf_vol(extptr);
  AdfVolume * vol = dev->volList[cur_vol];
  writable::list dev_l((R_xlen_t)0);
  dev_l.push_back(extptr);
  writable::list result({
    "device"_nm = dev_l,
      "path"_nm = adf_entry_to_path(extptr, cur_vol, vol->curDirPtr, TRUE)
  });
  return result;
}

list list_adf_entries3_(SEXP extptr, AdfVolume * vol,
                        SECTNUM sector, int vol_num, bool recursive) {
  writable::list result;
  auto alist = new AdfList;
  auto entry = new AdfEntry;
  
  alist = adfGetRDirEnt ( vol, sector, FALSE );
  while ( alist ) {
    entry = (AdfEntry *)alist->content;
    
    result.push_back({
      "name"_nm = list()
    });
    writable::strings(result.attr("names")).at(result.size() - 1) = entry->name;
    alist = alist->next;
    if (entry->type == ST_DIR && recursive) {
      result[entry->name] =
        list_adf_entries3_(extptr, vol, entry->sector, vol_num, recursive);
    }
    
  }
  adfFreeDirList(alist);
  delete alist;
  delete entry;
  return result;
}

list list_adf_entries2_(SEXP extptr, AdfVolume * vol,
                        SECTNUM sector, int vol_num, bool recursive) {
  writable::list result;
  auto alist = new AdfList;
  auto entry = new AdfEntry;
  
  alist = adfGetRDirEnt ( vol, sector, FALSE );
  while ( alist ) {
    entry = (AdfEntry *)alist->content;
    
    result.push_back(
      writable::strings({
        adf_entry_to_path(extptr, vol_num, entry->sector, TRUE)
      })
    );
    alist = alist->next;
    if (entry->type == ST_DIR && recursive) {
      result.push_back(
        list_adf_entries2_(extptr, vol, entry->sector, vol_num, recursive)
      );
    }
    
  }
  adfFreeDirList(alist);
  delete alist;
  delete entry;
  return result;
}

[[cpp11::register]]
list list_adf_entries_(SEXP extptr, std::string filename, bool recursive, bool nested) {
  writable::list result;
  AdfDevice * dev = get_adf_dev(extptr);
  
  int mode = ADF_FI_EXPECT_DIR | ADF_FI_THROW_ERROR | ADF_FI_EXPECT_EXIST |
    ADF_FI_EXPECT_VALID_CHECKSUM;
  list entry_pos = adf_path_to_entry(extptr, filename, mode);
  int vol_num  = integers(entry_pos["volume"]).at(0);
  SECTNUM sect = integers(entry_pos["sector"]).at(0);
  
  if (vol_num < 0 || sect < (SECTNUM)0)
    Rf_error("Path does not exist");
  
  AdfVolume * vol = dev->volList[vol_num];
  if (nested) {
    result = list_adf_entries3_(extptr, vol, sect, vol_num, recursive);
  } else {
    result = list_adf_entries2_(extptr, vol, sect, vol_num, recursive);
  }
  return result;
}

[[cpp11::register]]
SEXP adf_mkdir(SEXP extptr, std::string path) {
  AdfDevice * dev = get_adf_dev(extptr);
  
  list entry = adf_path_to_entry(extptr, path, 0);
  std::string remainder = strings(entry["remainder"]).at(0);
  int vol_num = integers(entry["volume"]).at(0);
  int sectype = integers(entry["header_sectype"]).at(0);
  if (sectype != ST_ROOT && sectype != ST_DIR)
    Rf_error("Parent of a new directory needs to be the root or another directory.");
  check_volume_number(dev, vol_num);
  AdfVolume * vol = dev->volList[vol_num];
  
  int parent = integers(entry["parent"]).at(0);
  if (parent < vol->firstBlock || parent > vol->lastBlock) Rf_error("Invalid path");
  
  check_adf_name(remainder);
  RETCODE rc = adfCreateDir(vol, parent, remainder.c_str());
  if (rc != RC_OK) Rf_error("Failed to create directory '%s'.", remainder.c_str());
  return extptr;
}

[[cpp11::register]]
SEXP adf_remove_entry(SEXP extptr, std::string path, bool flush) {
  AdfDevice * dev = get_adf_dev(extptr);
  
  int mode = ADF_FI_THROW_ERROR | ADF_FI_EXPECT_EXIST |
    ADF_FI_EXPECT_VALID_CHECKSUM;
  
  list entry    = adf_path_to_entry(extptr, path, mode);
  int sectype   = integers(entry["header_sectype"]).at(0);
  if (sectype == ST_ROOT) Rf_error("Cannot remove a device's root");
  
  int vol_num   = integers(entry["volume"]).at(0);
  SECTNUM psect = integers(entry["parent"]).at(0);
  
  bool file_state = adf_check_file_state(dev, vol_num, integers(entry["sector"]).at(0));
  if (file_state)
    Rf_error("Cannot remove files with open connection. Close file first then try again.");
  
  check_volume_number(dev, vol_num);
  AdfVolume * vol = dev->volList[vol_num];
  std::string name_s = (std::string)strings(entry["name"]).at(0);
  
  int nblocks = (vol->lastBlock - vol->firstBlock);
  writable::logicals block_bit((R_xlen_t)nblocks - 2);
  
  int i;
  // Store current state of bitmap if file needs to be flushed  
  if (flush) {
    for(i = vol->firstBlock + 2; i <= nblocks; i++) {
      block_bit[i - 2] = (r_bool)adfIsBlockFree(vol, i);
    }
  }
  
  const char * name = name_s.c_str();
  
  RETCODE rc = adfRemoveEntry(vol, psect, name);
  
  if (rc == RC_DIR_NOT_EMPTY) Rf_error("Can remove directory only when it is empty.");
  
  if (rc != RC_OK) Rf_error("Failed to remove entry from device.");
  
  writable::raws empty((R_xlen_t)512);
  for (i = 0; i < 512; i++) empty[i] = 0;
  
  // Erase blocks that have been set to free in the bitmap
  if (flush) {
    for (i = vol->firstBlock + 2; i <= nblocks; i++) {
      r_bool new_state = (r_bool)adfIsBlockFree(vol, i);
      r_bool old_state = (r_bool)block_bit[i - 2];
      if (!old_state && new_state) {
        write_adf_block_(extptr, i, empty);
      }
    }
  }
  
  return R_NilValue;
}

[[cpp11::register]]
SEXP adf_set_entry_name_(SEXP extptr, std::string path, std::string replacement) {
  int parent, vol_num, sectype;
  std::string name;
  RETCODE rc = RC_OK;
  if (Rf_inherits(extptr, "adf_device")) {
    AdfDevice * dev = get_adf_dev(extptr);
    if (dev->readOnly) Rf_error("Cannot change entry name on 'readonly' device.");
    
    list entry = adf_path_to_entry(extptr, path, 0);
    vol_num = integers(entry["volume"]).at(0);
    sectype = integers(entry["header_sectype"]).at(0);
    parent  = integers(entry["parent"]).at(0);
    name    = (std::string)strings(entry["name"]).at(0);
    check_volume_number(dev, vol_num);
    AdfVolume * vol = dev->volList[vol_num];
    if (sectype == ST_ROOT) {
      if (!adf_set_dev_name(extptr, vol_num, replacement))
        rc = RC_ERROR;
    } else {
      rc =
        adfRenameEntry (vol, parent, name.c_str(), parent, replacement.c_str());
    }
  } else if (Rf_inherits(extptr, "adf_file_con")) {
    AdfFile *af = get_adffile(extptr);
    int nl = af->fileHdr->nameLen;
    if (nl > MAXNAMELEN) nl = MAXNAMELEN;
    
    std::string entry_name =
      (std::string)(std::string(af->fileHdr->fileName).substr(0, nl));
    
    rc =
      adfRenameEntry (af->volume, af->fileHdr->parent,
                      entry_name.c_str(), af->fileHdr->parent,
                      replacement.c_str());
    if (rc == RC_OK) {
      af->fileHdr->nameLen = replacement.length();
      strcpy(af->fileHdr->fileName, replacement.c_str());
      // Don't think it is necesarry to update the checksum here
    }
    
  } else {
    Rf_error("External pointer should by of class `adf_device` or `adf_file_con`.");
  }
  if (rc != RC_OK) Rf_error("Failed to rename entry.");
  
  return extptr;
}

[[cpp11::register]]
SEXP move_adf_internal(SEXP extptr, std::string source, std::string destination) {
  if (source.size() != 1 && destination.size() != 1)
    Rf_error("`move_adf_internal` can only handle length 1 arguments");
  int mode = ADF_FI_THROW_ERROR | ADF_FI_EXPECT_EXIST |
    ADF_FI_EXPECT_VALID_CHECKSUM;
  
  list entry_src = adf_path_to_entry(extptr, source, mode);
  list entry_dst = adf_path_to_entry(extptr, destination, mode);
  writable::list entry_traverse;
  
  int sec_type_src = integers(entry_src["header_sectype"]).at(0);
  int sec_type_dst = integers(entry_dst["header_sectype"]).at(0);
  if (sec_type_src == ST_ROOT)
    Rf_error("Cannot move the root to elsewhere on the device.");
  if (sec_type_dst != ST_DIR && sec_type_dst != ST_ROOT)
    Rf_error("'destination' does not point at a directory.");
  
  int dst_check = integers(entry_dst["sector"]).at(0);
  int dst_parent = integers(entry_dst["parent"]).at(0);
  int src_check = integers(entry_src["sector"]).at(0);
  int cur_vol = integers(entry_src["volume"]).at(0);
  
  AdfDevice * dev = get_adf_dev(extptr);
  auto vol = dev->volList[cur_vol];
  
  // Check if the source header is not in the path of the destination
  uint8_t buf[512] = {0};
  bEntryBlock * entry = (bEntryBlock *) buf;
  
  bool test = (dst_check == src_check || dst_parent == src_check);
  
  int failsafe = 0L;
  while (sec_type_dst != ST_ROOT) {
    RETCODE rc = adfReadEntryBlock ( vol, dst_parent, entry );
    if (rc != RC_OK) Rf_error("Failed to check destination path");
    dst_parent = (int)entry->parent;
    sec_type_dst = (int)entry->secType;
    if (test || dst_parent == src_check)
      Rf_error("'source' is already in `destination` path, cannot move.");
    failsafe++;
    if (failsafe > 1000L) Rf_error("Unexpectedly deep path");
  }
  
  // Currently this function only checks if the move is allowed
  // It doesn't actually move anything
  return R_NilValue;
}

/*================================== */

static void swapb(void *result, int size)
{
  int i;
  char *p = (char *)result, tmp;
  
  if (size == 1) return;
  for (i = 0; i < size/2; i++) {
    tmp = p[i];
    p[i] = p[size - i - 1];
    p[size - i - 1] = tmp;
  }
}

static SEXP readOneString(AdfFile * af)
{
  char buf[10001], *p;
  int pos, m;
  
  for(pos = 0; pos < 10000; pos++) {
    p = buf + pos;
    m = (int) adf_file_read(af, sizeof(char), (uint8_t *)p);
    if (m < 0) Rf_error("error reading from the connection");
    if(!m) {
      if(pos > 0)
        Rf_warning("incomplete string at end of file has been discarded");
      return R_NilValue;
    }
    if(*p == '\0') break;
  }
  if(pos == 10000)
    Rf_warning("null terminator not found: breaking string at 10000 bytes");
  return strings(r_string(std::string(buf)));
}

[[cpp11::register]]
SEXP adf_readbin(SEXP extptr, int what, int n, int sz, bool sgn, bool swap) {
  size_t block = 8096;
  int sizedef= 4, mode = 1;
  
  SEXP ans = R_NilValue;
  R_xlen_t i, m = 0, size = sz;
  void *p = NULL;
  AdfFile * af = get_adffile(extptr);
  if (what == 7) { // =========================character
    writable::strings res((R_xlen_t)0);
    res.reserve(n);
    SEXP onechar;
    for(i = 0, m = 0; i < n; i++) {
      onechar = readOneString(af);
      if (onechar == R_NilValue) break;
      res.push_back(r_string(onechar));
    }
    return res;
  } else if (what == 5) { // ==================complex
    if(size == NA_INTEGER) size = sizeof(Rcomplex);
    if(size != sizeof(Rcomplex))
      Rf_error("size changing is not supported for complex vectors");
    PROTECT(ans = Rf_allocVector(CPLXSXP, n));
    p = (void *) COMPLEX(ans);
    uint8_t *pp = (uint8_t *)p;
    R_xlen_t m0, n0 = n;
    m = 0;
    while(n0) {
      size_t n1 = (n0 < (R_xlen_t)block) ? n0 : block;
      m0 = (int) adf_file_read(af, n1 * size, pp);
      if (m0 < 0) Rf_error("error reading from the connection");
      m += m0;
      if ((uint32_t)m0 < n1) break;
      n0 -= n1;
      pp += n1 * size;
    }
    if(swap) {
      for(i = 0; i < m; i++) {
        swapb(&(COMPLEX(ans)[i].r), sizeof(double));
        swapb(&(COMPLEX(ans)[i].i), sizeof(double));
      }
    }
    UNPROTECT(1);
    return ans;
  } else if (what == 3 || what == 4) { // =========integer or int
    sizedef = sizeof(int); mode = 1;
    
#if (SIZEOF_LONG == 8) && (SIZEOF_LONG > SIZEOF_INT)
#  define CASE_LONG_ETC case sizeof(long):
#elif (SIZEOF_LONG_LONG == 8) && (SIZEOF_LONG_LONG > SIZEOF_INT)
#  define CASE_LONG_ETC case sizeof(_lli_t):
#else
#  define CASE_LONG_ETC
#endif
    
#define CHECK_INT_SIZES(SIZE, DEF) do {					                        \
    if(SIZE == NA_INTEGER) SIZE = DEF;				                          \
    switch (SIZE) {						                                          \
    case sizeof(signed char):					                                  \
    case sizeof(short):						                                      \
    case sizeof(int):						                                        \
      CASE_LONG_ETC						                                          \
      break;							                                              \
    default:							                                              \
      Rf_error("size %d is unknown on this machine", (int)SIZE);	      \
    }								                                                    \
} while(0)

CHECK_INT_SIZES(size, sizedef);
PROTECT(ans = Rf_allocVector(INTSXP, n));
p = (void *) INTEGER(ans);
  }  else if (what == 5) { // =======================logical
    sizedef = sizeof(int); mode = 1;
    CHECK_INT_SIZES(size, sizedef);
    PROTECT(ans = Rf_allocVector(LGLSXP, n));
    p = (void *) LOGICAL(ans);
  } else if (what == 8) { // =========================raw
    sizedef = 1; mode = 1;
    if(size == NA_INTEGER) size = sizedef;
    switch (size) {
    case 1:
      break;
    default:
      Rf_error("raw is always of size 1");
    }
    PROTECT(ans = Rf_allocVector(RAWSXP, n));
    p = (void *) RAW(ans);
  } else if (what == 2 || what == 3) { //===============double or integer
    sizedef = sizeof(double); mode = 2;
    if(size == NA_INTEGER) size = sizedef;
    switch (size) {
    case sizeof(double):
    case sizeof(float):
#if HAVE_LONG_DOUBLE && (SIZEOF_LONG_DOUBLE > SIZEOF_DOUBLE)
    case sizeof(long double):
#endif
      break;
    default:
      Rf_error("size %d is unknown on this machine", (int)size);
    }
    PROTECT(ans = Rf_allocVector(REALSXP, n));
    p = (void *) REAL(ans);
  } else
    Rf_error("invalid 'what' argument");
  
  if(!sgn && (mode != 1 || size > 2))
    warning("'signed = FALSE' is only valid for integers of sizes 1 and 2");
  
  if (size == sizedef) {
    
    /* Do this in blocks to avoid large buffers in the connection */
    char *pp = (char *)p;
    R_xlen_t m0, n0 = n;
    m = 0;
    while(n0) {
      size_t n1 = (n0 < (R_xlen_t)block) ? n0 : block;
      m0 = (int) adf_file_read(af, size*n1, (uint8_t *)pp);
      if (m0 < 0) Rf_error("error reading from the connection");
      m += m0;
      if ((uint32_t)m0 < n1) break;
      n0 -= n1;
      pp += n1 * size;
    }
    if(swap && size > 1)
      for(i = 0; i < m; i++) swapb((char *)p+i*size, size);
  } else {
    R_xlen_t s;
    union {
      signed char sch;
      unsigned char uch;
      signed short ssh;
      unsigned short ush;
      long l;
      long long ll;
      float f;
#if HAVE_LONG_DOUBLE
      long double ld;
#endif
    } u;
    if (size > (R_xlen_t)sizeof u)
      Rf_error("size %d is unknown on this machine", (int)size);
    if(mode == 1) { /* integer result */
    for(i = 0, m = 0; i < n; i++) {
      s = (int) adf_file_read(af, size, (uint8_t *)&u);
      if (s < 0) Rf_error("error reading from the connection");
      if(s) m++; else break;
      if(swap && size > 1) swapb((char *) &u, size);
      switch(size) {
      case sizeof(signed char):
        if(sgn)
          INTEGER(ans)[i] = u.sch;
        else
          INTEGER(ans)[i] = u.uch;
        break;
      case sizeof(short):
        if(sgn)
          INTEGER(ans)[i] = u.ssh;
        else
          INTEGER(ans)[i] = u.ush;
        break;
#if SIZEOF_LONG == 8
      case sizeof(long):
        INTEGER(ans)[i] = (int) u.l;
        break;
#elif SIZEOF_LONG_LONG == 8
      case sizeof(_lli_t):
        INTEGER(ans)[i] = (int) u.ll;
        break;
#endif
      default:
        Rf_error("size %d is unknown on this machine", (int)size);
      }
    }
    } else if (mode == 2) { /* double result */
    for(i = 0, m = 0; i < n; i++) {
      s = (int) adf_file_read(af, size, (uint8_t *)&u);
      if (s < 0) Rf_error("error reading from the connection");
      if(s) m++; else break;
      if(swap && size > 1) swapb((char *) &u, size);
      switch(size) {
      case sizeof(float):
        REAL(ans)[i] = u.f;
        break;
#if HAVE_LONG_DOUBLE && (SIZEOF_LONG_DOUBLE > SIZEOF_DOUBLE)
      case sizeof(long double):
        REAL(ans)[i] = (double) u.ld;
        break;
#endif
      default:
        Rf_error("size %d is unknown on this machine", (int)size);
      }
    }
    }
    
  }
  if(m < n)
    ans = Rf_xlengthgets(ans, m);
  UNPROTECT(1);
  return ans;
}


[[cpp11::register]]
SEXP adf_readlines(SEXP extptr, int n_, bool ok, bool warn, std::string encoding, bool skipNul) {
  Rboolean utf8locale = (Rboolean)false; // Note that this is an R definition that is not part of its API
  size_t nbuf, buf_size = 1000;
  int oenc = CE_NATIVE, c;
  if (encoding.compare("UTF-8") == 0) oenc = CE_UTF8;
  else if (encoding.compare("latin1") == 0) oenc = CE_LATIN1;
  else if (encoding.compare("bytes") == 0) oenc = CE_BYTES;
  
  char *buf = (char *) malloc(buf_size);
  if(!buf)
    Rf_error("cannot allocate buffer in readLines");
  
  R_xlen_t n = n_, nnn, nread;
  AdfFile * af = get_adffile(extptr);
  writable::strings result((R_xlen_t)0);
  result.reserve((R_xlen_t)1000);
  
  nnn = (n < 0) ? R_XLEN_T_MAX : n;
  for(nread = 0; nread < nnn; nread++) {
    nbuf = 0;
    while((c = adf_getc(af)) != R_EOF) {
      if(nbuf == buf_size-1) {  /* need space for the terminator */
        buf_size *= 2;
        char *tmp = (char *) realloc(buf, buf_size);
        if(!tmp) {
          free(buf);
          Rf_error("cannot allocate buffer in readLines");
        } else buf = tmp;
      }
      if(skipNul && c == '\0') continue;
      if(c != '\n')
        /* compiler-defined conversion behaviour */
        buf[nbuf++] = (char) c;
      else
        break;
    }
    buf[nbuf] = '\0';
    /* Remove UTF-8 BOM */
    const char *qbuf = buf;
    // avoid valgrind warning if < 3 bytes
    if (nread == 0 && utf8locale && strlen(buf) >= 3 &&
        !memcmp(buf, "\xef\xbb\xbf", 3)) qbuf = buf + 3;
    result.push_back(r_string(Rf_mkCharCE(qbuf, (cetype_t)oenc)));
    if (warn && strlen(buf) < nbuf)
      Rf_warning("line %lld appears to contain an embedded nul",
              (long long)nread + 1);
    if(c == R_EOF) goto no_more_lines;
  }
  
  return result;
  no_more_lines:
    if(nbuf > 0) { /* incomplete last line */
      nread++;
      if(warn)
        Rf_warning("incomplete final line found on connection");
    }
    free(buf);
    if(nread < nnn && !ok)
      Rf_error("too few lines read in readLines");

    return result;
    
}

const char *translateChar0(SEXP x) {
  if (TYPEOF(x) != CHARSXP)
    Rf_error("`translateChar0` in `adf_writebin` was not called with CHARSXP as expected");
  int ct = Rf_getCharCE(x);
  if (ct == CE_BYTES) return CHAR(x);
  return Rf_translateChar(x);
}

[[cpp11::register]]
SEXP adf_writebin(SEXP object, SEXP extptr, int size, bool swap, bool useBytes) {
  AdfFile * af = get_adffile(extptr);
  if (!af->modeWrite)
    Rf_error("cannot write to this connection");
  
  if(swap == NA_LOGICAL) Rf_error("invalid 'swap' argument");
  if(useBytes == NA_LOGICAL) Rf_error("invalid 'useBytes' argument");
  R_xlen_t i, len = XLENGTH(object);
  
  if (len == 0) return R_NilValue;

#ifndef LONG_VECTOR_SUPPORT
  /* without long vectors RAW vectors are limited to 2^31 - 1 bytes */
  if(len * (double)size > INT_MAX)
      Rf_error("only 2^31-1 bytes can be written in a single writeBin() call");
#endif
  SEXP ans = R_NilValue;
  if(TYPEOF(object) == STRSXP) {
    const char *s;
    /* translateChar0() is the same as CHAR for IS_BYTES strings */
    for(i = 0; i < len; i++) {
      if(useBytes)
        s = CHAR(STRING_ELT(object, i));
      else
        s = translateChar0(STRING_ELT(object, i));
      size_t nwrite = adfFileWrite(af, sizeof(char) * strlen(s) + 1, (uint8_t *)s);
      if(!nwrite) {
        Rf_warning("problem writing to connection");
        break;
      }
    }
  } else {
    switch(TYPEOF(object)) {
    case LGLSXP:
    case INTSXP:
      CHECK_INT_SIZES(size, sizeof(int));
      break;
    case REALSXP:
      if(size == NA_INTEGER) size = sizeof(double);
      switch (size) {
      case sizeof(double):
      case sizeof(float):
#if HAVE_LONG_DOUBLE && (SIZEOF_LONG_DOUBLE > SIZEOF_DOUBLE)
      case sizeof(long double):
#endif
        break;
      default:
        Rf_error("size %d is unknown on this machine", size);
      }
      break;
    case CPLXSXP:
      if(size == NA_INTEGER) size = sizeof(Rcomplex);
      if(size != sizeof(Rcomplex))
        Rf_error("size changing is not supported for complex vectors");
      break;
    case RAWSXP:
      if(size == NA_INTEGER) size = 1;
      if(size != 1)
        Rf_error("size changing is not supported for raw vectors");
      break;
    default:
      Rf_error("writBin is not implemented for the provided object");
    }
    char *buf = (char *)malloc(len * size);
    R_xlen_t j;
    switch(TYPEOF(object)) {
    case LGLSXP:
    case INTSXP:
      switch (size) {
      case sizeof(int):
        memcpy(buf, INTEGER(object), size * len);
        break;
#if SIZEOF_LONG == 8
      case sizeof(long):
{
  for (i = 0, j = 0; i < len; i++, j += size) {
  long l1 = (long) INTEGER(object)[i];
  memcpy(buf + j, &l1, size);
}
  break;
}
#elif SIZEOF_LONG_LONG == 8
      case sizeof(_lli_t):
{
  for (i = 0, j = 0; i < len; i++, j += size) {
  _lli_t ll1 = (_lli_t) INTEGER(object)[i];
  memcpy(buf + j, &ll1, size);
}
  break;
}
#endif
      case 2:
{
  for (i = 0, j = 0; i < len; i++, j += size) {
  short s1 = (short) INTEGER(object)[i];
  memcpy(buf + j, &s1, size);
}
  break;
}
      case 1:
        for (i = 0; i < len; i++)
          /* compiler-defined conversion behavior */
          buf[i] = (signed char) INTEGER(object)[i];
        break;
      default:
        Rf_error("size %d is unknown on this machine", size);
      }
      break;
    case REALSXP:
      switch (size) {
      case sizeof(double):
        memcpy(buf, REAL(object), size * len);
        break;
      case sizeof(float):
        {
          for (i = 0, j = 0; i < len; i++, j += size) {
          float f1 = (float) REAL(object)[i];
          memcpy(buf+j, &f1, size);
        }
          break;
        }
#if HAVE_LONG_DOUBLE && (SIZEOF_LONG_DOUBLE > SIZEOF_DOUBLE)
      case sizeof(long double):
{
  /* some systems have problems with memcpy from
   the address of an automatic long double,
   e.g. ix86/x86_64 Linux with gcc4 */
  static long double ld1;
  for (i = 0, j = 0; i < len; i++, j += size) {
    ld1 = (long double) REAL(object)[i];
    memcpy(buf+j, &ld1, size);
  }
  break;
}
#endif
      default:
        Rf_error("size %d is unknown on this machine", size);
      }
      break;
    case CPLXSXP:
      memcpy(buf, COMPLEX(object), size * len);
      break;
    case RAWSXP:
      memcpy(buf, RAW(object), len); /* size = 1 */
  break;
    }
    
    if(swap && size > 1) {
      if (TYPEOF(object) == CPLXSXP)
        for(i = 0; i < len; i++) {
          int sz = size/2;
          swapb(buf+sz*2*i, sz);
          swapb(buf+sz*(2*i+1), sz);
        }
        else
          for(i = 0; i < len; i++) swapb(buf+size*i, size);
    }
    
    /* write it now */
    size_t nwrite = adfFileWrite(af, size*len, (uint8_t *)buf);
    if (nwrite < (uint32_t)len) Rf_warning("problem writing to connection");
    free(buf);
  }
  return ans;
}

[[cpp11::register]]
SEXP adf_writelines(strings text, SEXP extptr, std::string sep, bool useBytes) {
  AdfFile * af = get_adffile(extptr);
  const char *ssep;
  
  if (!af->modeWrite)
    Rf_error("cannot write to this connection");
  if(!Rf_isString(text)) Rf_error("invalid 'text' argument");
  if(useBytes == NA_LOGICAL)
    Rf_error("invalid 'useBytes' argument");
  
  if(useBytes)
    ssep = R_CHAR(r_string(sep));
  else
    ssep = translateChar0(r_string(sep));
  
  for(R_xlen_t i = 0; i < text.size(); i++) {
    const char * t = useBytes ? R_CHAR(text.at(i)) : translateChar0(text.at(i));
    adfFileWrite(af, sizeof(char) * strlen(t), (uint8_t *)t);
    adfFileWrite(af, sizeof(char) * strlen(ssep), (uint8_t *)ssep);
  }

  return R_NilValue;
}
