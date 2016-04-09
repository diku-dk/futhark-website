#include <thrust/device_vector.h>
#include <thrust/functional.h>
#include <thrust/transform.h>
#include <thrust/complex.h>
#include <thrust/iterator/counting_iterator.h>
#include <iostream>
#include <cstdlib>
#include <sys/time.h>

#include "common.hpp"

static const float xmin = -2.23;
static const float ymin = -1.15;
static const float xmax = 0.83;
static const float ymax = 1.15;

__host__ __device__ inline float dot(thrust::complex<float> c) {
  return c.real() * c.real() + c.imag() * c.imag();
}

__host__ __device__ int inline divergence(int depth, thrust::complex<float> c0) {
  thrust::complex<float> c = c0;
  int i = 0;
  while (i < depth && dot(c) < 4.0F) {
    c = c0 + c * c;
    i++;
  }
  return i;
}

class pixel : public thrust::unary_function<int, int> {
public:
  pixel(int n) : m_n(n) {}

  __host__ __device__ int operator()(int pos) const {
    int x = pos / m_n;
    int y = pos % m_n;
    float sizex = xmax - xmin;
    float sizey = ymax - ymin;
    int screenX = m_n;
    int screenY = m_n;

    thrust::complex<float> c0(xmin + x * sizex / screenX,
                              ymin + y * sizey / screenY);

    return divergence(255, c0);
  }

private:
  int m_n;
};

int main(int argc, char **argv) {
  int runs, n;
  runs_and_n(argc, argv, &runs, &n);

  thrust::device_vector<int> d(n*n);

  // Warmup
  thrust::transform
    (thrust::make_counting_iterator(0),
     thrust::make_counting_iterator(n*n),
     d.begin(),
     pixel(n));

  start_timing();
  for (size_t i = 0; i < runs; ++i) {
    thrust::transform
      (thrust::make_counting_iterator(0),
       thrust::make_counting_iterator(n*n),
       d.begin(),
       pixel(n));
  }
  end_timing();

  std::cout << "Result: " << d[0] << std::endl;
  report_time(runs);
}
