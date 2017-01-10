#include <stdio.h>
#include <unistd.h>
#include <omp.h>

int main(int argc, char ** argv)
{
  int max_threads = omp_get_max_threads();

  printf("%d\n", max_threads);

  return 0;
}
