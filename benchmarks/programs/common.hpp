// Helper facilities shared among all CUDA benchmarks.

#ifndef COMMON_HPP
#define COMMON_HPP

#include <thrust/device_vector.h>
#include <iostream>
#include <cstdlib>

#define SEED 1337
#define MAX 10
#define MIN (-10)

void runs_and_n(int argc, char** argv, int *runs, int *n) {
  if (argc != 3) {
    std::cerr << "Usage: " << argv[0] << " <runs> <n>" << std::endl;
    std::exit(1);
  }
  *runs = atoi(argv[1]);
  *n = atoi(argv[2]);
}

template <typename T>
void init_vector(thrust::device_vector<T> *d) {
  int n = d->size();

  std::srand(SEED);

  thrust::host_vector<T> h(n);
  for (int i = 0; i < n; i++) {
    h[i] = (std::rand()%(MAX-MIN))+MIN;
  }

  *d = h;
  cudaDeviceSynchronize();
}

static cudaEvent_t start, end;

void start_timing() {
  cudaEventCreate(&start);
  cudaEventCreate(&end);

  cudaEventRecord(start);
}

void end_timing() {
  cudaEventRecord(end);

  cudaEventSynchronize(end);
}

void report_time(int runs) {
  float total_ms = 0;
  cudaEventElapsedTime(&total_ms, start, end);

  float mean_ms = total_ms / runs;
  float mean_us = 1000 * mean_ms;

  std::cout << "Runtime: " << int(mean_us) << "us" << std::endl;

}

#endif
