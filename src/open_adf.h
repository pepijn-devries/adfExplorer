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

bool check_adf_volume_state(AdfVolume * vol);
bool check_adf_device_state(AdfDevice * dev);

#endif /* __OPEN_ADF__ */