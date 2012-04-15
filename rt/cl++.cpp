/*
 *  cl++.cpp
 *  CLmandelbrot
 *
 *  Created by eholk on 12/3/10.
 *
 */

#include "cl++.h"
#include <iostream>
#include <cassert>
#include <stdlib.h>

extern cl::device_list g_devices;

using namespace std;
using namespace cl;

std::string device::name()
{
	char n[256];
	clGetDeviceInfo(id, CL_DEVICE_NAME, sizeof(n), n, NULL);

	return std::string(n);
}

cl_device_type device::type()
{
	cl_device_type t;
	clGetDeviceInfo(id, CL_DEVICE_TYPE, sizeof(t), &t, NULL);
	return t;
}

device::operator cl_device_id() const
{
	return id;
}

device_list::device_list(cl_device_type type)
: type(type), num_ids(0), devices(NULL)
{
    cl_int status = 0;

    // Find the platforms.
    cl_platform_id *platforms;
    cl_uint nPlatforms = 0;
    CL_CHECK(clGetPlatformIDs(0, NULL, &nPlatforms));
    assert(nPlatforms > 0);

    platforms = new cl_platform_id[nPlatforms];
    CL_CHECK(clGetPlatformIDs(nPlatforms, platforms, &nPlatforms));

    // Find out how many devices there are.
    cl_uint n_dev = 0;
    for(int i = 0; i < nPlatforms; ++i) {
        status = clGetDeviceIDs(platforms[i], type, CL_UINT_MAX, NULL, &n_dev);
        if(CL_DEVICE_NOT_FOUND == status)
            continue;
        CL_CHECK(status);
        num_ids += n_dev;
        break;
    }
  
    // Allocate memory, gather information about all the devices.
    devices = new cl_device_id[num_ids];
  
    size_t offset = 0;
    for(int i = 0; i < nPlatforms; ++i) {
        status = clGetDeviceIDs(platforms[i], type, CL_UINT_MAX,
                                devices + offset, &n_dev);
        if(CL_DEVICE_NOT_FOUND == status)
            continue;
        CL_CHECK(status);
        offset += n_dev;
    }

    cerr << "found " << num_ids << " devices" << endl;

    delete [] platforms;
}

device_list::~device_list()
{
	if(devices)
		delete [] devices;
}

int device_list::count() const
{
	return num_ids;
}

const cl_device_id *device_list::ids() const
{
	return devices;
}

device device_list::operator[](int index)
{
	assert(index < count());
	return device(devices[index]);
}

context::context(device_list &devices)
{
  cl_int status;
  ctx = clCreateContext(0, devices.count(), devices.ids(),
                        sLogError, this, &status);
  CL_CHECK(status);
}

context::~context()
{
	clReleaseContext(ctx);
}

void context::sLogError(const char *errinfo,
						const void *private_info,
						size_t private_info_sz,
						void *pThis)
{
	((context*)pThis)->logError(errinfo, private_info, private_info_sz);
}

void context::logError(const char *errinfo,
					   const void *private_info,
					   size_t private_info_sz)
{
	cerr << "OpenCL Error: " << errinfo << endl;
}

kernel::kernel(cl_kernel k)
: k(k)
{
}

kernel::~kernel()
{
	clReleaseKernel(k);
}

size_t kernel::maxWorkGroupSize(cl_device_id device)
{
	size_t result;
	size_t ret_size;

	clGetKernelWorkGroupInfo(k,
							 device,
							 CL_KERNEL_WORK_GROUP_SIZE,
							 sizeof(result),
							 &result,
							 &ret_size);

	return result;
}

program::program(cl_program prog)
: prog(prog)
{
}

program::~program()
{
	clReleaseProgram(prog);
}

string escape_path(const char *s) {
  string e = "";
  while(*s != '\0') {
	if(*s == ' ') {
	  e += "\\ ";
	}
	else {
	  e += *s;
	}
	++s;
  }
  return e;
}

void program::build()
// TODO: make a variant that can compile for certain devices.
{
    char *cwd = getcwd(NULL, 0);
    string opts = "-I";
    opts += escape_path(cwd);
    opts += " -I/Users/eric/class/osl/dpp/svn/user/webyrd/harlan";
    opts += " -Werror";
    free(cwd);
    cl_int status = clBuildProgram(prog, 0, NULL, opts.c_str(), NULL, NULL);
    if(status != CL_SUCCESS) {
        char log[8192];
        
        CL_CHECK(clGetProgramBuildInfo(prog,
                                       g_devices[0],
                                       CL_PROGRAM_BUILD_LOG,
                                       sizeof(log),
                                       log,
                                       NULL));
        std::cerr << log << std::endl;
    }
    CL_CHECK(status);
}

kernel program::createKernel(string name)
{
	return kernel(clCreateKernel(prog, name.c_str(), NULL));
}

command_queue::command_queue(cl_command_queue queue)
: queue(queue)
{
}

command_queue::command_queue(const command_queue &other)
	: queue(other.queue)
{
	clRetainCommandQueue(queue);
}

command_queue::~command_queue()
{
	clReleaseCommandQueue(queue);
}

void command_queue::execute(kernel &k, size_t global_size)
{
	executeND(k, 1, &global_size, NULL);
}

void command_queue::execute(kernel &k, size_t global_size, size_t local_size)
{
	executeND(k, 1, &global_size, &local_size);
}

void command_queue::executeND(kernel &k, size_t dimensions,
                              size_t global_size[], size_t local_size[])
{
	cl_event e;
	CL_CHECK(clEnqueueNDRangeKernel(queue, k.k, dimensions, NULL,
                                    global_size, local_size, 0, 0, &e));
	CL_CHECK(clEnqueueBarrier(queue));
	CL_CHECK(clWaitForEvents(1, &e));
	CL_CHECK(clReleaseEvent(e));
}

program context::createProgramFromSourceFile(string filename)
{
	ifstream input(filename.c_str());
	return createProgramFromSourceFile(input);
}

program context::createProgramFromSourceFile(ifstream &input)
{
	string src, line;
	while(getline(input, line)) {
		src += line;
		src += "\n";
	}

	return createProgramFromSource(src);
}

program context::createProgramFromSource(string src)
{
  const char *c_src = src.c_str();
  cl_int status;
  cl_program p = clCreateProgramWithSource(ctx, 1, &c_src, NULL, &status);
  CL_CHECK(status);
  return program(p);
}

program context::createAndBuildProgramFromSource(string src)
{
    program p = createProgramFromSource(src);
    p.build();
    return p;
}

command_queue context::createCommandQueue(cl_device_id dev)
{
	cl_int status;

	cerr << "Creating queue for " << device(dev).name() << endl;

	cl_command_queue q = clCreateCommandQueue(ctx,
						  dev,
						  0, // properties
						  &status);
	CL_CHECK(status);
	return command_queue(q);
}

void cl::handle_error(const char *code, cl_int e)
{
#define HANDLE(x)                                                       \
    if(e == x) {                                                        \
        cerr << code << " failed with error " #x " (" << e << ")" << endl; \
		abort();														\
    }
 
    HANDLE(CL_BUILD_PROGRAM_FAILURE)
    HANDLE(CL_COMPILER_NOT_AVAILABLE)
    HANDLE(CL_DEVICE_NOT_FOUND)
    HANDLE(CL_INVALID_BINARY)
    HANDLE(CL_INVALID_BUILD_OPTIONS)
    HANDLE(CL_INVALID_COMMAND_QUEUE)
    HANDLE(CL_INVALID_CONTEXT)
    HANDLE(CL_INVALID_DEVICE)
    HANDLE(CL_INVALID_DEVICE_TYPE)
    HANDLE(CL_INVALID_EVENT_WAIT_LIST)
    HANDLE(CL_INVALID_GLOBAL_OFFSET)
    //HANDLE(CL_INVALID_GLOBAL_WORK_SIZE)
    HANDLE(CL_INVALID_IMAGE_SIZE)
    HANDLE(CL_INVALID_KERNEL)
    HANDLE(CL_INVALID_KERNEL_ARGS)
    HANDLE(CL_INVALID_OPERATION)
    HANDLE(CL_INVALID_PLATFORM)
    HANDLE(CL_INVALID_PROGRAM)
    HANDLE(CL_INVALID_PROGRAM_EXECUTABLE)
    HANDLE(CL_INVALID_QUEUE_PROPERTIES)
    HANDLE(CL_INVALID_VALUE)
    HANDLE(CL_INVALID_WORK_DIMENSION)
    HANDLE(CL_INVALID_WORK_GROUP_SIZE)
    HANDLE(CL_INVALID_WORK_ITEM_SIZE)
    HANDLE(CL_MEM_OBJECT_ALLOCATION_FAILURE)
    //HANDLE(CL_MISALIGNED_SUB_BUFFER_OFFSET)
    HANDLE(CL_OUT_OF_RESOURCES)
    HANDLE(CL_OUT_OF_HOST_MEMORY)

    cerr << code << " failed with unknown error (" << e << ")" << endl;
    abort();
}
