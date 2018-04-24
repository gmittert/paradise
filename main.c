#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <CL/cl.h>

void checkError(cl_int error) {
  if (error != CL_SUCCESS) {
    printf("OpenCL call failed with error %d\n", error);
    exit(1);
  }
}

int main() {
  cl_uint numPlatforms = 0;
  clGetPlatformIDs(0, NULL, &numPlatforms);

  if (numPlatforms == 0) {
    printf("No OpenCL platforms found\n");
    return 1;
  }
  cl_uint platformIdCount = 0;
  clGetPlatformIDs(0, NULL, &platformIdCount);

  cl_platform_id *platformIds = malloc(numPlatforms);
  clGetPlatformIDs(platformIdCount, platformIds, NULL);

  cl_uint deviceIdCount = 0;
  clGetDeviceIDs(platformIds[0], CL_DEVICE_TYPE_ALL, 0, NULL, &deviceIdCount);

  if (deviceIdCount == 0) {
    printf("No OpenCL devices found\n");
    return 1;
  } else {
    printf("Found %d devices(s)\n", deviceIdCount);
  }

  cl_device_id *deviceIds = malloc(deviceIdCount);
  clGetDeviceIDs(platformIds[0], CL_DEVICE_TYPE_ALL, deviceIdCount, deviceIds,
                 NULL);

  const cl_context_properties contextProperties[] = {
      CL_CONTEXT_PLATFORM, (cl_context_properties)(platformIds[0]), 0};

  cl_int error = CL_SUCCESS;
  cl_context context = clCreateContext(contextProperties, deviceIdCount,
                                       deviceIds, NULL, NULL, &error);
  checkError(error);


  FILE * f = fopen ("samples/opencl/kernels/saxpy.cl", "rb");
  fseek (f, 0, SEEK_END);
  int flength = ftell(f);
  fseek (f, 0, SEEK_SET);
  char* buffer = malloc (flength);
  if (buffer) {
    fread (buffer, 1, flength, f);
  } else {
    fprintf(stderr, "Alloc Buffer failed\n");
  }
  fclose (f);

  size_t lengths[1] = {flength};
  const char *sources[1] = {buffer};

  error = 0;
  cl_program program =
    clCreateProgramWithSource(context, 1, sources, lengths, &error);
  checkError(error);

  checkError(
      clBuildProgram(program, deviceIdCount, deviceIds, NULL, NULL, NULL));

  cl_kernel kernel = clCreateKernel(program, "SAXPY", &error);
  checkError(error);

  // Prepare some test data
  static const size_t testDataSize = 1024;
  float *a = malloc(testDataSize*sizeof(float));
  float *b = malloc(testDataSize*sizeof(float));
  if (!a) {
    printf("alloc a failed\n");
  }
  if (!b) {
    printf("alloc b failed\n");
  }
  for (int i = 0; i < testDataSize; ++i) {
    a[i] = (float)(23 ^ i);
    b[i] = (float)(42 ^ i);
  }

  cl_mem aBuffer =
      clCreateBuffer(context, CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR,
                     sizeof(float) * testDataSize, a, &error);
  checkError(error);

  cl_mem bBuffer =
      clCreateBuffer(context, CL_MEM_READ_WRITE | CL_MEM_COPY_HOST_PTR,
                     sizeof(float) * testDataSize, b, &error);
  checkError(error);


  cl_command_queue queue =
      clCreateCommandQueueWithProperties(context, deviceIds[0], 0, &error);
  checkError(error);

  clSetKernelArg(kernel, 0, sizeof(cl_mem), &aBuffer);
  clSetKernelArg(kernel, 1, sizeof(cl_mem), &bBuffer);
  static const float two = 2.0f;
  clSetKernelArg(kernel, 2, sizeof(float), &two);

  checkError(clEnqueueNDRangeKernel(queue, kernel, 1, NULL, &testDataSize,
                                    NULL, 0, NULL, NULL));

  checkError(clEnqueueReadBuffer(queue, bBuffer, CL_TRUE, 0,
                                 sizeof(float) * testDataSize, b, 0,
                                 NULL, NULL));

  clReleaseCommandQueue(queue);
  clReleaseMemObject(bBuffer);
  clReleaseMemObject(aBuffer);
  clReleaseKernel(kernel);
  clReleaseProgram(program);
  clReleaseContext(context);
  free(platformIds);
  free(deviceIds);
  free(a);
  free(b);
}
