// xfail - this is a benchmark program and it takes a long time to run.
#include "harlan.hpp"
#include <iostream>

using namespace std;

const char *g_src =
  "__kernel void dot_product(__global int *x, __global int *y, "
  "                          __global int *out) {"
  "  int i = get_global_id(0);"
  "  out[i] = x[i] * y[i];"
  "}";

cl::program g_prog = g_ctx.createProgramFromSource(g_src);

uint64_t do_dot_prod(int size) stop{
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

  cl::kernel k = g_prog.createKernel("dot_product");

  k.setArg(0, gx);
  k.setArg(1, gy);
  k.setArg(2, go);

  g_queue.execute(k, size, 1);

  g_queue.read_buffer(go, out);

  // sum the elements
  int dot = 0;
  for(int i = 0; i < size; i++) {
	dot += out[i];
  }
  uint64_t stop = nanotime();

  assert(dot == size);

  delete [] x;
  delete [] y;
  delete [] out;

  return  - start;
}

int main() {
  g_prog.build();
  
  const int iters = 10;
  
  for(int len = 100000; len <= 10000000; len += 100000) {
	uint64_t ttime = 0;
	for(int k = 0; k < iters; k++) {
	  ttime += do_dot_prod(len);
	}

	float itime = (float)(ttime / iters) / 1000000;
	cout << len << "\t" << itime << endl;
  }

  return 0;
}
