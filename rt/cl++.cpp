/*
 *  cl++.cpp
 *
 *  Created by eholk on 12/3/10.
 *
 */

#include "cl++.h"
#include <iostream>
#include <sstream>
#include <cassert>
#include <stdlib.h>
#include <unistd.h>

extern cl::device_list g_devices;

using namespace std;
using namespace cl;

// in builtin.cpp
uint64_t nanotime();

std::string device::name()
{
  char n[256] = {0};
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
        status = clGetDeviceIDs(platforms[i], type, 0, NULL, &n_dev);
        if(CL_DEVICE_NOT_FOUND == status)
            continue;
        CL_CHECK_MSG(status, "clGetDeviceIDs (count)");
        num_ids += n_dev;
        break;
    }

    cerr << "found " << num_ids << " devices" << endl;

    // We seem to get errors by assigning a program to too many
    // devices, so to prevent these, we arbitrarily restrict ourself
    // to one device here.
    num_ids = 1;
  
    // Allocate memory, gather information about all the devices.
    devices = new cl_device_id[num_ids];
  
    size_t offset = 0;
    for(int i = 0; i < nPlatforms; ++i) {
        status = clGetDeviceIDs(platforms[i], type, n_dev,
                                devices + offset, &n_dev);
        if(CL_DEVICE_NOT_FOUND == status)
            continue;
        CL_CHECK_MSG(status, "clGetDeviceIDs");
        offset += n_dev;
    }

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
  CL_CHECK_MSG(status, "clCreateContext");
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
    char *cwd = ::getcwd(NULL, 0);
    string opts = "-I";
    opts += escape_path(cwd);
    opts += " -I/Users/eric/class/osl/dpp/svn/user/webyrd/harlan";
    // opts += " -Werror";
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
    CL_CHECK_MSG(status, "clBuildProgram");
}

kernel program::createKernel(string name)
{
	return kernel(clCreateKernel(prog, name.c_str(), NULL));
}

command_queue::command_queue(cl_command_queue queue)
    : queue(queue), kernel_time(0)
{
}

command_queue::command_queue(const command_queue &other)
	: queue(other.queue), kernel_time(other.kernel_time)
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

void command_queue::execute2d(kernel &k, size_t dim1, size_t dim2, size_t local_size)
{
  size_t global_size[] = {dim1, dim2};
  size_t local_size_array[] = {local_size, local_size};  
  executeND(k, 2, global_size, local_size_array);
}

void command_queue::executeND(kernel &k, size_t dimensions,
                              size_t global_size[], size_t local_size[])
{
    //cout << "Enqueuing " << dimensions << "-D kernel: (" << global_size[0];
    //for(int i = 1; i < dimensions; ++i)
    //    cout << ", " << global_size[i];
    //cout << ")" << endl;

    // If any dimensions are 0, don't do anything. This may not be the
    // best behavior, because it usually means something else went
    // wrong in the calling program.
    for(int i = 0; i < dimensions; ++i)
        if(0 == global_size[i])
            return;

	cl_event e;
    uint64_t start = nanotime();
	CL_CHECK(clEnqueueNDRangeKernel(queue, k.k, dimensions, NULL,
                                    global_size, local_size, 0, 0, &e));
	//CL_CHECK(clEnqueueBarrier(queue));
	CL_CHECK(clWaitForEvents(1, &e));
    uint64_t stop = nanotime();
	CL_CHECK(clReleaseEvent(e));

    //fprintf(stderr, "Kernel took %f seconds\n",
    //        float(stop - start) / 1e9);

    kernel_time += (stop - start);
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
  CL_CHECK_MSG(status, "clCreateProgramWithSource");
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
	CL_CHECK_MSG(status, "clCreateCommandQueue");
	return command_queue(q);
}

string cl::format_status(cl_int e) {
#define STATUS_STR(x) case x: return #x;

    switch(e) {
        STATUS_STR(CL_SUCCESS);
        STATUS_STR(CL_BUILD_PROGRAM_FAILURE);
        STATUS_STR(CL_COMPILER_NOT_AVAILABLE);
        STATUS_STR(CL_DEVICE_NOT_AVAILABLE);
        STATUS_STR(CL_DEVICE_NOT_FOUND);
        STATUS_STR(CL_INVALID_BINARY);
        STATUS_STR(CL_INVALID_BUILD_OPTIONS);
        STATUS_STR(CL_INVALID_COMMAND_QUEUE);
        STATUS_STR(CL_INVALID_CONTEXT);
        STATUS_STR(CL_INVALID_DEVICE);
        STATUS_STR(CL_INVALID_DEVICE_TYPE);
        STATUS_STR(CL_INVALID_EVENT_WAIT_LIST);
        STATUS_STR(CL_INVALID_GLOBAL_OFFSET);
        STATUS_STR(CL_INVALID_IMAGE_SIZE);
        STATUS_STR(CL_INVALID_MEM_OBJECT);
        STATUS_STR(CL_INVALID_KERNEL);
        STATUS_STR(CL_INVALID_KERNEL_ARGS);
        STATUS_STR(CL_INVALID_OPERATION);
        STATUS_STR(CL_INVALID_PLATFORM);
        STATUS_STR(CL_INVALID_PROGRAM);
        STATUS_STR(CL_INVALID_PROGRAM_EXECUTABLE);
        STATUS_STR(CL_INVALID_QUEUE_PROPERTIES);
        STATUS_STR(CL_INVALID_VALUE);
        STATUS_STR(CL_INVALID_WORK_DIMENSION);
        STATUS_STR(CL_INVALID_WORK_GROUP_SIZE);
        STATUS_STR(CL_INVALID_WORK_ITEM_SIZE);
        STATUS_STR(CL_MEM_OBJECT_ALLOCATION_FAILURE);
        STATUS_STR(CL_OUT_OF_RESOURCES);
        STATUS_STR(CL_OUT_OF_HOST_MEMORY);

    default:
        stringstream s;
        s << "Unknown";
        return s.str();
    }
}

void cl::handle_error(const char *code, cl_int e)
{
    cerr << code << " failed with error "                           
         << format_status(e) << " (" << e << ")" << endl;           
	abort();														
}
