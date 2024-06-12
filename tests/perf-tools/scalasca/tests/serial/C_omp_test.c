#include <stdio.h>
#include <unistd.h>
#include <omp.h>

void throttle_some(int tid)
{
  if (tid & 1) {
    sleep(1);
  } 
}

int main(int argc, char ** argv)
{
  int max_threads = omp_get_max_threads();

  #pragma omp parallel
  {
    int tid = omp_get_thread_num();
    printf("Hello from thread %d/%d\n", tid, max_threads);
    throttle_some(tid);
  }

  return 0;
}
