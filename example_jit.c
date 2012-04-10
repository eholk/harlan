
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include "harlan_jit.h"

int main(int argc, char* argv[]) {

  printf("[client] Calling harlan init...\n");
  HarlanInit();

  printf("[client] Compiling a simple kernel:\n");
  harlan_handle_t hndl = HarlanJit("1F", "1F", "foo", "(define (foo arr) (print arr))");

  printf("[client] Next invoking the kernel with a small array...\n");
  HarlanRun(hndl, "1F", "1F", NULL, NULL);

  printf("[client] Finally, calling shutdown...\n");
  HarlanShutdown();
  printf("[client] Shutdown finished\n");
}

