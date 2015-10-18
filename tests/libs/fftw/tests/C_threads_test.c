/* http://www.metsahovi.fi/~jwagner/fftw/ */
#include <fftw3.h>
#include <malloc.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>
#include <time.h>

#define O_METHOD  FFTW_MEASURE // FFTW_PATIENT, FFTW_MEASURE, FFTW_ESTIMATE
#define O_METHOD_STR "FFTW_MEASURE"

int main(int argc, char** argv) {
   fftw_plan plan;
   double add, mul, fma, flops, deltaT;
   fftw_complex* in;
   fftw_complex* out;

   struct timeval tv_start, tv_stop;

   int N_THREADS;
   int FFT_LEN;
   int N_ITER;
   int i;

   FFT_LEN = 1024;
   N_THREADS = 16;
   N_ITER = 1;
   if (FFT_LEN < 1024 || FFT_LEN > 8192*1024) {
      printf("FFT len not between 1K and 8192M\n"); return -1;
   }
   if (N_THREADS < 1) N_THREADS=1;
   if (N_THREADS > 8) N_THREADS=8;

   #ifdef CELL
   fftw_cell_set_nspe(N_THREADS);
   #else
   fftw_init_threads();
   fftw_plan_with_nthreads(N_THREADS);
   #endif

   in = memalign(128, FFT_LEN*sizeof(double)*2);
   out = memalign(128, FFT_LEN*sizeof(double)*2);
   for (i = 0; i<FFT_LEN; i++) {
      in[i][0] = drand48();
      in[i][1] = drand48();
   }

   /* initialize the plan: this can take a long time! */
//   printf("Allocated in=%p out=%p, FFT len=%d, threads=%d\n", in, out, FFT_LEN, N_THREADS);
   printf("Creating complex-to-complex 1D DFT plan with " O_METHOD_STR "...\n");
   gettimeofday(&tv_start, NULL);
   plan = fftw_plan_dft_1d(FFT_LEN, in, out, FFTW_FORWARD, O_METHOD);
   gettimeofday(&tv_stop, NULL);
   deltaT = tv_stop.tv_sec - tv_start.tv_sec + 1e-6*(tv_stop.tv_usec - tv_start.tv_usec);
   printf("Plan init time        : %f sec\n", deltaT);

   #ifdef I_AM_AN_FFTW_GURU
      fftw_print_plan(plan);
      printf("\n");
   #endif

   /* now run one or more FFT interations */
   gettimeofday(&tv_start, NULL);
   for (i=0; i<N_ITER; i++)
      fftw_execute(plan);
   gettimeofday(&tv_stop, NULL);
   deltaT = tv_stop.tv_sec - tv_start.tv_sec + 1e-6*(tv_stop.tv_usec - tv_start.tv_usec);
   deltaT = deltaT / N_ITER;

   fftw_flops(plan, &add, &mul, &fma);
   flops = add + mul + 2*fma;
   printf("Plan exec time        : %f sec (average over %d iteration(s))\n", deltaT, N_ITER);
   printf("Theoretical FLOP count: %f\n", flops);
   printf("Plan MFLOPS estimate  : %f\n", 1e-6*flops/deltaT);

   #ifndef CELL
   fftw_cleanup_threads();
   #endif
   return 0;
}
