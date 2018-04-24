int main() {

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

  clReleaseKernel(kernel);
  clReleaseContext(context);
  clReleaseProgram(program);
  free(platformIds);
  free(deviceIds);
}
