#include <cpp11.hpp>
#include "open_adf.h"
using namespace cpp11;

std::vector<AdfContainer *> opendevices;
void close_adf_internal(AdfContainer * ac); // defined in open_adf_file.cpp

void freeAdfContainer(AdfContainer * ac) {
  for (long unsigned i = 0; i < opendevices.size(); i++) {
    AdfContainer * ac2 = opendevices.at(i);
    if (ac2 == ac) {
      opendevices.erase(opendevices.begin() + i);
      break;
    }
  }
  
  if (ac->isopen) {
    AdfDevice * dev = ac->dev;
    adfCloseDev(dev); // This closes the adf file and frees all allocated mem for dev
  }
  delete ac;
  return;
}

[[cpp11::register]]
SEXP open_adf_(std::string filename, bool write_protected) {
  AdfDevice * dev;
  dev = adfMountDev(filename.c_str(), write_protected);
  if (!dev) {
    dev = adfOpenDev(filename.c_str(), write_protected);
    if (! dev) cpp11::stop("Could not mount virtual device");
  }
  
  if (dev->nVol > 0) {
    for (int i = 0; i < dev->nVol; i++) {
      adfMount(dev, i, write_protected);
    }
  }
  
  AdfContainer * ac = (AdfContainer *)new AdfContainer;
  ac->currentVol    = (dev->nVol > 0) ? 0 : -1;
  ac->dev           = dev;
  ac->isopen        = true;

  opendevices.push_back(ac);
  external_pointer<AdfContainer, freeAdfContainer>adfdev(ac);
  sexp result = as_sexp(adfdev);
  result.attr("class") = "adf_device";
  
  return result;
}

bool check_adf_device_state(AdfDevice * dev) {
  for(const AdfContainer * ac : opendevices) {
    if (ac->dev == dev) return true;
  }
  return false;
}

bool check_adf_volume_state(AdfVolume * vol) {
  for(const AdfContainer * ac : opendevices) {
    if (ac->isopen && ac->dev->nVol > 0) {
      for (int i = 0; i < ac->dev->nVol; i++) {
        if (ac->dev->volList[i] == vol) return true;
      }
    }
  }
  return false;
}

[[cpp11::register]]
void close_all_devices_() {
  if (opendevices.size() == 0) return;
  for (long i = opendevices.size() - 1; i>= 0; i--) {
    auto ac = opendevices.at(i);
    close_adf_internal(ac);
    opendevices.erase(opendevices.begin() + i);
  }
}
