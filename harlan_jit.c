
/*================================================================================*
 * harlan_jit.c
 * 
 * This compiles to a library that can be linked into any C program to
 * give access to Harlan/OpenCL JIT capabilities
 *================================================================================*/

// This is Chez Scheme specific.
// Embedding a different Scheme runtime system would be possible but
// substantially different.


// NOTES: HOWTO ADD A NEW ENTRYPOINT INTO SCHEME:
// ------------------------------------------------------------
// (1) Add a C prototype for the function pointer (Scheme_*).
// (2) Add a wrapper that's exposed to C code (HARLAN_*).
// (3) Extend the Harlan_Init to initialize the function pointer by doing
//     a lookup in the global scheme symbol table.
//     (These names end in -entry because of the define-entrypoint macro.)


#include <stdlib.h>
#include <string.h>	
#include <stdio.h>
#include <sys/types.h>
#include <limits.h>
#include <errno.h>

#include "harlan_jit.h"

//const char *S_date_stamp = "10102009080511";
// const char *S_date_stamp = "0";

#include "scheme.h"

  int verbose = 0;

//==============================================================================
// First, function pointers to all of the relevant calls.  These point
// to scheme functions.

int (*Scheme_HarlanJit)(const char* inputsig, const char* outputsig, const char* name, const char* definition);

void (*Scheme_HarlanRun)(int fn, const char* inputsig, const char* outputsig, void* inputs, void* outputs);


//==============================================================================
/* The functions exposed through the C API are just wrappers for the
   above scheme functions.  The only reason that these are separate
   (annoying I know) is that in our harlan_jit.h header file we don't
   want to expose the fact that we are using function pointers.
*/

int HarlanJit(const char* inputsig, const char* outputsig, const char* name, const char* definition) {
  if (verbose>=3) printf(" <Harlan> Calling scheme function for HarlanJit...\n");
  return Scheme_HarlanJit(inputsig, outputsig, name, definition);
}

void HarlanRun(harlan_handle_t fn, const char* inputsig, const char* outputsig, void* inputs, void* outputs) {
  if (verbose>=3) printf(" <Harlan> Calling scheme function for HarlanJit...\n");
  Scheme_HarlanRun(fn, inputsig, outputsig, inputs, outputs);
}


// typedef void (*intfun) (int);
// typedef void (*voidfun) ();

// Run a script to get the machine type identifier used by Chez (e.g. "ta6le")
char* get_machine_type() {
    FILE* strm = popen("$HARLAND/util/chez_machine_type", "r");
    char* format = malloc(100);
    fscanf(strm, "%s\n", format);
    return format;
}

void HarlanInit(const char* outfile) {
  char* harland = getenv("HARLAND");
  char* chezd = getenv("CHEZD");
  char bootfile[2048]; // MAX path length
  
  if (!chezd) {
    printf("Environment variable CHEZD must be set to the Chez-Scheme install directory, usually called csv<ver>.");
    exit(1);
  }
  if (!harland) {
    printf("Environment variable HARLAND must be set Harlan installation directory.");
    exit(1);
  }
  char* machinetype = get_machine_type();

  const char* v = getenv("HARLAN_VERBOSE");
  if (v) verbose = strtol(v, NULL, 10);
  if (errno) { printf("HARLAN_VERBOSE set to invalid value: %s\n", v); abort(); }

  // Initialize Chez Scheme runtime:
  Sscheme_init(0);  
  if (verbose>=3) printf(" <Harlan> Scheme init finished...\n");

  // To establish new FFI bindings we need full Chez:
  sprintf(bootfile, "%s/boot/%s/petite.boot", chezd, machinetype);
  Sregister_boot_file(bootfile);
  sprintf(bootfile, "%s/boot/%s/scheme.boot", chezd, machinetype);
  Sregister_boot_file(bootfile);

  if (verbose>=3) printf(" <Harlan> Scheme boot files registered, building heap...\n");

  // This actually loads the boot files registered above:
  Sbuild_heap(".", 0);
  //Sbuild_heap(argv[0], 0);

  Senable_expeditor(0);

  char script_src[2048];
  sprintf(script_src, "%s/harlan_jit.scm", harland);

  // NOTE: This is a real oddity.  I must *pad* the argument list??
  // Seems like there might be a bug.
  const char* new_args[] = {"apparently_ignored", script_src};

  // [2010.09.24] Trying to make it quiet...
  // It seems to treat all arguments as files to be loaded, even empty strings!
  //const char* new_args[] = {"", "-q"};
  //const char* new_args[] = {"", "--quiet", script_src};
  // 
  // Tell chez scheme not to print the greeting:
  Scall1(Stop_level_value( Sstring_to_symbol("suppress-greeting")), Strue);

  if (verbose>=1) printf(" <Harlan> Starting Scheme runtime system (Sscheme_start).\n");
  int result = Sscheme_start(2, new_args);
  //int result = Sscheme_start(3, new_args);
  if (result) { 
      printf("ERROR: Exited from scheme initialization with non-zero code %d\n", result);
      abort(); 
  }

  if (verbose>=2) printf(" <Harlan> Scheme is up!\n");
  
  ptr jitptr =  Stop_level_value( Sstring_to_symbol("HarlanJit-entry"));
  ptr runptr =  Stop_level_value( Sstring_to_symbol("HarlanRun-entry"));

  Scheme_HarlanJit = (int(*)(const char*,const char*,const char*, const char*))Sinteger_value(jitptr);
  Scheme_HarlanRun = (void(*)(int,const char*,const char*,void*,void*))        Sinteger_value(runptr);

  if (verbose>=2) printf(" <Harlan> Scheme entrypoint handles acquired, initialization complete.\n");   
}


void HarlanShutdown() {
  // Scheme_Shutdown();
  // Sscheme_deinit(); // Chez call to bring down the runtime.
}

