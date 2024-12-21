#include <cpp11.hpp>
#include "open_adf.h"
using namespace cpp11;

void freeAdfContainer(AdfContainer * ac) {
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
    if (! dev) Rf_error("Could not mount virtual device");
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

  external_pointer<AdfContainer, freeAdfContainer>adfdev(ac);
  sexp result = as_sexp(adfdev);
  result.attr("class") = "adf_device";
  
  return result;
}
