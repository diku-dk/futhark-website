#include <thrust/device_vector.h>
#include <thrust/transform.h>
#include <thrust/sequence.h>
#include <thrust/copy.h>
#include <thrust/fill.h>
#include <thrust/replace.h>
#include <thrust/functional.h>
#include <iostream>
#include <cstdlib>
#include <sys/time.h>

#include "common.hpp"

int main(int argc, char **argv) {
  int runs, n;
  runs_and_n(argc, argv, &runs, &n);

  thrust::device_vector<int> d(n);
  init_vector(&d);

  int sum;

  // Warmup
  sum = thrust::reduce(d.begin(), d.end(), (int) 0, thrust::plus<int>());

  start_timing();
  for (size_t i = 0; i < runs; ++i) {
    sum = thrust::reduce(d.begin(), d.end(), (int) 0, thrust::plus<int>());
  }
  end_timing();

  std::cout << "Result: " << sum << std::endl;
  report_time(runs);
}
