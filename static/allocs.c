#define _BSD_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <time.h>
#include <sys/time.h>

#include <string.h>
#include <assert.h>

#include <CL/cl.h>

#define STRINGIFY(s) #s

#define SUCCEED(e) (assert(e == 0))

const char *kernel_cl = STRINGIFY(
__kernel void put(__global int* p)
{
  *p = 0;
});

int useconds() {
  struct timeval t;
  gettimeofday(&t, NULL);
  return t.tv_sec*1000000+t.tv_usec;
}

int main() {
  cl_int error;
  cl_platform_id platform;
  cl_device_id device;
  cl_uint platforms, devices;

  // Fetch the Platform and Device IDs; we only want one.
  error=clGetPlatformIDs(1, &platform, &platforms);
  error=clGetDeviceIDs(platform, CL_DEVICE_TYPE_GPU, 1, &device, &devices);
  cl_context_properties properties[]={
    CL_CONTEXT_PLATFORM, (cl_context_properties)platform,
    0};
  // Note that nVidia's OpenCL requires the platform property
  cl_context context=clCreateContext(properties, 1, &device, NULL, NULL, &error);
  cl_command_queue cq = clCreateCommandQueue(context, device, 0, &error);

  // Submit the source code of the kernel to OpenCL
  cl_program prog=clCreateProgramWithSource(context, 1, &kernel_cl, NULL, &error);
  // And compile it (after this we could extract the cOmpiled version)
  SUCCEED(clBuildProgram(prog, 0, NULL, "", NULL, NULL));

  // Get a handle and map parameters for the kernel
  cl_kernel k_put=clCreateKernel(prog, "put", &error);

  const size_t workers[] = {1};
  const size_t localworkers[] = {1};

  for (int i = 1; i < 1<<30; i *= 2) {
    cl_mem mem;
    const int num_tries = 100;
    int time = 0;
    int min_time = 0x7FFFFFFF;
    int max_time = 0;
    for (int j = 0; j < num_tries; j++) {
      int start = useconds();
      mem=clCreateBuffer(context, CL_MEM_READ_WRITE, i, NULL, &error);
      SUCCEED(error);
      clSetKernelArg(k_put, 0, sizeof(mem), &mem);
      SUCCEED(clEnqueueNDRangeKernel(cq, k_put, 1, NULL, workers, localworkers, 0, NULL, NULL));
      SUCCEED(clFinish(cq));
      int aft = useconds();
      int this_time = aft-start;
      time += this_time;
      min_time = min_time < this_time ? min_time : this_time;
      max_time = max_time < this_time ? this_time : max_time;
      SUCCEED(clReleaseMemObject(mem));
    }
    printf("%d bytes; average: %dus; min: %dus; max: %dus\n", i, time/num_tries, min_time, max_time);
  }
}
