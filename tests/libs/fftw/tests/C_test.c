#include <stdio.h>
#include <fftw3.h>
#define N 8

int main(int argc, char *argv[])
{
    int i;
    double in1[] = { 0.00000, 0.12467, 0.24740, 0.36627,
                     0.47943, 0.58510, 0.68164, 0.76754
    };

    double in2[N];

    fftw_complex  out[N / 2 + 1];
    fftw_plan     p1, p2;

    p1 = fftw_plan_dft_r2c_1d(N, in1, out, FFTW_ESTIMATE);
    p2 = fftw_plan_dft_c2r_1d(N, out, in2, FFTW_ESTIMATE);

    fftw_execute(p1);
    fftw_execute(p2);

    for (i = 0; i < N; i++) {
          printf("%2d %15.10f %15.10f\n", i, in1[i], in2[i] / N);
    }

    fftw_destroy_plan(p1);
    fftw_destroy_plan(p2);

    return 0;
}
