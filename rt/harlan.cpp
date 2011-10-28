#define NO_GLOBALS
#include "harlan.hpp"

#include <stdlib.h>
#include <string>

using namespace std;

cl_device_type get_device_type()
{
  const char *cfg = getenv("HARLAN_DEVICE");

  if(cfg) {
    string s = cfg;
    if(s == "gpu") {
      return CL_DEVICE_TYPE_GPU | CL_DEVICE_TYPE_ACCELERATOR;
    }
    else if(s == "cpu") {
      return CL_DEVICE_TYPE_CPU;
    }
  }
  return (CL_DEVICE_TYPE_GPU |
          CL_DEVICE_TYPE_CPU |
          CL_DEVICE_TYPE_ACCELERATOR |
          0);
}
