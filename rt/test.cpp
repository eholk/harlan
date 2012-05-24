/*
  This is some code that can determine whether the alloc_header struct
  has the same size on the host and device.
 */

#include "harlan.hpp"

cl::program g_prog(g_ctx.createAndBuildProgramFromSource(
"#include \"rt/gpu_common.h\"\n"
"__kernel void kernel_6(__global int* k_arg_8) {\n"
"    __global int* retval_4 = (&(k_arg_8[get_global_id(0)]));\n"
"    *retval_4 = sizeof(struct alloc_header);\n"
"}"));

int main()
{
    cl::buffer<int> cl_buf = g_ctx.createBuffer<int>(1 * sizeof(int), CL_MEM_READ_WRITE);

    cl::kernel k = g_prog.createKernel("kernel_6");
    k.setArg(0, cl_buf);

    g_queue.execute(k, 1, 1);

    cl::buffer_map<int> p = g_queue.mapBuffer(cl_buf);

    std::cout << sizeof(alloc_header) << "\t" << p[0] << std::endl;

    return 0;
}
