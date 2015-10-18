/*
Copyright (c) 2015, Intel Corporation

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

* Redistributions of source code must retain the above copyright notice,
  this list of conditions and the following disclaimer.
* Redistributions in binary form must reproduce the above copyright
  notice, this list of conditions and the following disclaimer in the
  documentation and/or other materials provided with the distribution.
* Neither the name of Intel Corporation nor the names of its contributors
  may be used to endorse or promote products derived from this software
  without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#include <stdio.h>
#include <stdlib.h>
#include <omp.h>

#define N 1024

int main(int argc, char* argv[])
{
  double a[N], b[N], c, sum;
  int nthreads, n, i;

  nthreads = (argc > 1) ? atoi(argv[1]) : 1;
  printf("# threads: %d\n", nthreads);

  omp_set_num_threads(nthreads);
  #pragma omp parallel
  {
    #pragma omp master
       n = omp_get_num_threads();
  }
  if (n != nthreads)
  {
    printf("\nERROR: %d thread(s) detected != %d\n", n, nthreads);
    return -1;
  }

  sum = c = 0.0;
  for (i = 0; i < N; i ++)
    sum += i * i * 2.0;

  #pragma omp parallel private(i)
  {
    #pragma omp for nowait
      for (i = 0; i < N; i ++)
        a[i] = i * 1.0;

    #pragma omp for nowait
      for (i = 0; i < N; i ++)
        b[i] = a[i] * 2.0;

    #pragma omp for reduction(+:c)
      for (i = 0; i < N; i ++)
	c += a[i] * b[i];
  }

  if (c != sum)
  {
    printf("\nERROR: computed sum %.2lf != %.2lf\n", c, sum);
    return -1;
  }
  printf("final sum: %.2lf\n", c);
  return 0;
}
