#define _BSD_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <time.h>
#include <sys/time.h>

#include <iostream>
#include <cstdio>

using namespace std;

#define cudaSucceeded(ans) { cudaAssert((ans), __FILE__, __LINE__); }

inline void cudaAssert(cudaError_t code, const char *file, int line, bool abort=true)
{
  if (code != cudaSuccess) {
    std::cerr << "cudaAssert failed: "
              << cudaGetErrorString(code)
              << file << ":" << line
              << std::endl;
    if (abort) {
      exit(code);
    }
  }
}

int useconds() {
  struct timeval t;
  gettimeofday(&t, NULL);
  return t.tv_sec*1000000+t.tv_usec;
}

int main() {
  for (int i = 1; i < 1<<30; i *= 2) {
    int *mem;
    const int num_tries = 100;
    int time = 0;
    int min_time = 0x7FFFFFFF;
    int max_time = 0;
    for (int j = 0; j < num_tries; j++) {
      int start = useconds();
      cudaSucceeded(cudaMalloc(&mem, i));
      cudaSucceeded(cudaMemset(mem, 0, 0));
      cudaSucceeded(cudaDeviceSynchronize());
      int aft = useconds();
      int this_time = aft-start;
      time += this_time;
      min_time = min_time < this_time ? min_time : this_time;
      max_time = max_time < this_time ? this_time : max_time;
      cudaSucceeded(cudaFree(mem));
    }
    printf("%d bytes; average: %dus; min: %dus; max: %dus\n", i, time/num_tries, min_time, max_time);
  }
}
