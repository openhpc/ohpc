#define N 1000

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
  float x[N] = {0};
  float y[N] = {0};
  float res[N];

  init(x, N);
  init(y, N);

  saxpy (res, a, x, y, N);

  return 0;
}
