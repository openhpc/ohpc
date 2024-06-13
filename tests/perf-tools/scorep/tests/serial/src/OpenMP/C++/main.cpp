#define N 1000

#include <array>

void init(float* arr,
          unsigned int n)
{
  for(unsigned int i = 0; i < n; ++i)
  {
      arr[i] = 1.0f*i;
  }
}

void saxpy(float* res, float a, float* x, float* y, unsigned int n)
{
  #pragma omp parallel for
  for(unsigned int i = 0; i < n; ++i)
  {
    res[i] = a * x[i] + y[i];
  }
}

int main(int    argc,
         char **argv)
{
  float a = 2.0f;
  std::array<float, N> x = {0};
  std::array<float, N> y = {0};
  std::array<float, N> res;

  init(x.data(), N);
  init(x.data(), N);

  saxpy(res.data(), a, x.data(), y.data(), N);

  return 0;
}
