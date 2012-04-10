
#ifdef __cplusplus 
extern "C" {
#endif 

// A handle on a compiled harlan function.
typedef int harlan_handle_t;

// These bring up and bring down the runtime (including initializing
// the underlying scheme runtime).
void HarlanInit();
void HarlanShutdown();

// Compile individual function definitions and invoke them:
harlan_handle_t HarlanJit(const char* inputsig, const char* outputsig, const char* name, const char* definition);
void HarlanRun(harlan_handle_t fn, const char* inputsig, const char* outputsig, void* inputs, void* outputs);

#ifdef __cplusplus 
} // End extern C
#endif 
