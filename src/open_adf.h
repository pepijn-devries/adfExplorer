#pragma once
#ifndef __OPEN_ADF__
#define __OPEN_ADF__

#include <vector>
#include "adflib/adflib.h"

struct AdfContainer {
  AdfDevice * dev;
  int currentVol;
  bool isopen;
};

#endif /* __OPEN_ADF__ */