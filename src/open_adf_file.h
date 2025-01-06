#pragma once
#ifndef __OPEN_ADF_FILE__
#define __OPEN_ADF_FILE__

#define R_EOF -1

/* Define BSWAP_32 on Big Endian systems */
#ifdef WORDS_BIGENDIAN
#if (defined(__sun) && defined(__SVR4))
#include <sys/byteorder.h>
#elif (defined(__APPLE__) && defined(__ppc__) || defined(__ppc64__))
#include <libkern/OSByteOrder.h>
#define BSWAP_32 OSSwapInt32
#elif (defined(__OpenBSD__))
#define BSWAP_32(x) swap32(x)
#elif (defined(__NetBSD__))
#include <sys/types.h>
#include <machine/bswap.h>
#define BSWAP_32(x) bswap32(x)
#elif (defined(__GLIBC__))
#include <byteswap.h>
#define BSWAP_32(x) bswap_32(x)
#elif (defined(_AIX))
#define BSWAP_32(x) __builtin_bswap32(x)
#endif
#endif

struct AdfFileContainer {
  AdfFile * f;
  bool isopen;
};

AdfFile * get_adffile(SEXP extptr);
AdfFileContainer * get_adffilecontainer(SEXP extptr);
int get_adf_file_volnum(AdfFile * adf_file);
bool adf_check_file_state(AdfDevice *dev, int vol, SECTNUM sect);
void freeAdfFileContainer(AdfFileContainer * afc);

#endif /* __OPEN_ADF_FILE__ */
