#include <thrust/device_vector.h>
#include <thrust/functional.h>
#include <thrust/transform_scan.h>
#include <iostream>
#include <cstdlib>
#include <sys/time.h>

#include "common.hpp"

typedef thrust::tuple<int, int, int, int> MSSTuple;

class prepare : public thrust::unary_function<const int, MSSTuple> {
public:
  __host__ __device__ MSSTuple operator()(const int x) const {
    return MSSTuple(max(x,0), max(x,0), max(x,0), x);
  }
};

class combine : public thrust::binary_function<const MSSTuple &, const MSSTuple &, MSSTuple>  {
public:
  __host__ __device__ MSSTuple operator()(const MSSTuple &x,
                                          const MSSTuple &y) const {
    int mssx = thrust::get<0>(x);
    int misx = thrust::get<1>(x);
    int mcsx = thrust::get<2>(x);
    int tsx  = thrust::get<3>(x);

    int mssy = thrust::get<0>(y);
    int misy = thrust::get<1>(y);
    int mcsy = thrust::get<2>(y);
    int tsy  = thrust::get<3>(y);

    return MSSTuple(max(mssx, max(mssy, mcsx + misy)),
                    max(misx, tsx+misy),
                    max(mcsy, mcsx+tsy),
                    tsx + tsy);
  }
};

int main(int argc, char **argv) {
  int runs, n;
  runs_and_n(argc, argv, &runs, &n);

  thrust::device_vector<int> d(n);
  thrust::device_vector<int> dres(n);

  init_vector(&d);

  // Warmup
  thrust::transform_inclusive_scan
      (d.begin(), d.end(),
       thrust::make_zip_iterator(thrust::make_tuple
                                 (dres.begin(),
                                  thrust::make_discard_iterator(),
                                  thrust::make_discard_iterator(),
                                  thrust::make_discard_iterator())),
       prepare(),
       combine());

  start_timing();
  for (size_t i = 0; i < runs; ++i) {
    thrust::transform_inclusive_scan
      (d.begin(), d.end(),
       thrust::make_zip_iterator(thrust::make_tuple
                                 (dres.begin(),
                                  thrust::make_discard_iterator(),
                                  thrust::make_discard_iterator(),
                                  thrust::make_discard_iterator())),
       prepare(),
       combine());
  }
  end_timing();

  std::cout << "Result: " << dres[n-1] << std::endl;
  report_time(runs);
}
