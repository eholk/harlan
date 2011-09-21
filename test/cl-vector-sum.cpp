//xfail
#include "harlan.hpp"
#include <iostream>

using namespace std;

const char *g_src =
  "__kernel void vector_sum(__global int *x, __global int *y, "
  "                         __global int *out) {"
  "  int i = get_global_id(0);"
  "  out[i] = x[i] + y[i];"
  "}";

cl::program g_prog = g_ctx.createProgramFromSource(g_src);

uint64_t do_vector_sum(int size) {
  int *x = new int[size];
  int *y = new int[size];
  int *out = new int[size];

  for(int i = 0; i < size; i++) {
	x[i] = 1;
	y[i] = 1;
  }

  uint64_t start = nanotime();
  cl::buffer<int> gx = g_ctx.createBuffer<int>(size,
											   CL_MEM_READ_WRITE);
  cl::buffer<int> gy = g_ctx.createBuffer<int>(size,
										  CL_MEM_READ_WRITE);
  cl::buffer<int> go = g_ctx.createBuffer<int>(size,
										  CL_MEM_READ_WRITE);

  g_queue.write_buffer(gx, x);
  g_queue.write_buffer(gy, y);

  cl::kernel k = g_prog.createKernel("vector_sum");

  k.setArg(0, gx);
  k.setArg(1, gy);
  k.setArg(2, go);

  g_queue.execute(k, size, 1);

  g_queue.read_buffer(go, out);

  uint64_t stop = nanotime();

  for (int i = 0; i < size; i++) {
	assert(out[i] == 2);
  }

  delete [] x;
  delete [] y;
  delete [] out;

  return stop - start;
}

int main() {
  g_prog.build();
  
  const int iters = 10;
  
  for(int len = 100000; len <= 10000000; len += 100000) {
	uint64_t ttime = 0;
	for(int k = 0; k < iters; k++) {
	  ttime += do_vector_sum(len);
	}

	float itime = (float)(ttime / iters) / 1000000;
	cout << len << "\t" << itime << endl;
  }

  return 0;
}
