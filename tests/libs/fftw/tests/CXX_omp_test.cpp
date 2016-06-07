// http://numbercrunch.de/blog/2010/03/parallel-fft-performance/
// depending on your compiler and os compile with
//
// g++ -fopenmp -O3 -o time_dirac_fft time_dirac_fft.cc -lfftw3 -lfftw3_threads -pthread
//
// or with
//
// icc -openmp -O3 -o time_dirac_fft time_dirac_fft.cc -lmkl_intel -lmkl_intel_thread -lmkl_core
//
// or with
//
// icc -openmp -O3 -o time_dirac_fft time_dirac_fft.cc -lmkl_intel_lp64 -lmkl_intel_thread -lmkl_core
 
#include <cstdlib>
#include <complex>
#include <iostream>
#include <sstream>
#include <fstream>
#include <omp.h>
#include <fftw3.h>
#if defined __unix__
#include <unistd.h>
#include <sys/time.h>
#include <sys/times.h>
#else
#include <ctime>
#endif
 
// helper class for time measurements
class timer {
private:
  const double _resolution;
  double _t, _t_start;
  bool isrunning;
 
  double get_time() const {
#if defined __unix__
    struct timeval tv;
    gettimeofday(&tv, 0);
    return static_cast<double>(tv.tv_sec)+static_cast<double>(tv.tv_usec)*1e-6;
#else
    return static_cast<double>(std::clock())*_resolution;
#endif
  }
public:
  void reset() {
    _t=0.0;
  }
  void start() {
    _t_start=get_time();
    isrunning=true;
  }
  void stop() {
    if (isrunning) {
      _t+=get_time()-_t_start;
      isrunning=false;
    }
  }
  double time() const {
    return _t+( isrunning ? get_time()-_t_start : 0.0 );
  }
  double resolution() const {
    return _resolution;
  };
  timer() :
#if defined __unix__
    _resolution(1e-6),
#else
    _resolution(1.0/CLOCKS_PER_SEC),
#endif
    _t(0), _t_start(get_time()),
    isrunning(true) {
  }
};
 
typedef std::complex<double> complex;
 
// test FFT performance as required for one-dimensional Dirac equation
void time_1d(int p, std::ostream &out) {
  out << "% one dimensional FFT\n"
      << "% two grids interwoven\n"
      << "%\n"
      << "% number of cpus = " << p << "\n"
      << "%\n"
      << "% n\ttime in sec.\n";
  omp_set_num_threads(p);
  omp_set_dynamic(false);
  fftw_plan_with_nthreads(p);
  for (int n=8192; n<=65536; n*=2) {
    complex *v=reinterpret_cast<complex *>(fftw_malloc(2*n*sizeof(*v)));
    if (v!=0) {
      fftw_iodim io_n[1]={ {n, 2, 2} };
      fftw_iodim io_is[1]={ {2, 1, 1} };
      fftw_plan p1=fftw_plan_guru_dft(1, io_n, 1, io_is,
				      reinterpret_cast<fftw_complex *>(v),
				      reinterpret_cast<fftw_complex *>(v),
				      FFTW_FORWARD, FFTW_MEASURE);
      fftw_plan p2=fftw_plan_guru_dft(1, io_n, 1, io_is,
				      reinterpret_cast<fftw_complex *>(v),
				      reinterpret_cast<fftw_complex *>(v),
				      FFTW_BACKWARD, FFTW_MEASURE);
      for (int i=0; i<n; ++i) {
	v[2*i]=complex(static_cast<double>(i+1)/static_cast<double>(n),
		       static_cast<double>(i+1)/static_cast<double>(n));
	v[2*i+1]=complex(static_cast<double>(i+1)/static_cast<double>(n),
			 static_cast<double>(i+1)/static_cast<double>(n));
      }
      timer T1;
      T1.start();
      int i=0;
      do {
	fftw_execute_dft(p1,
			 reinterpret_cast<fftw_complex *>(v),
			 reinterpret_cast<fftw_complex *>(v));
	fftw_execute_dft(p2,
			 reinterpret_cast<fftw_complex *>(v),
			 reinterpret_cast<fftw_complex *>(v));
	++i;
      } while (T1.time()<4 and i<16);
      double t1=T1.time()/i;
      fftw_free(v);
      fftw_destroy_plan(p1);
      fftw_destroy_plan(p2);
      out << n << '\t' << t1 << std::endl;
      std::cerr << "1d\tp = " << p
		<< "\tN = " << n
		<< "\ttime = " << t1 << std::endl;
    } else
      std::cerr << "2d\tp = " << p
		<< "\tN = " << n
		<< "\tnot enough memory" << std::endl;
  }
}
 
// test FFT performance as required for two-dimensional Dirac equation
void time_2d(int p, std::ostream &out) {
  out << "% two dimensional FFT\n"
      << "% four grids interwoven\n"
      << "%\n"
      << "% number of cpus = " << p << "\n"
      << "%\n"
      << "% n\ttime in sec.\n";
  omp_set_num_threads(p);
  omp_set_dynamic(false);
  fftw_plan_with_nthreads(p);
  for (int n=128; n<=512; n*=2) {
    complex *v=reinterpret_cast<complex *>(fftw_malloc(4*n*n*sizeof(*v)));
    if (v!=0) {
      fftw_iodim io_n[2]={ {n, 4, 4}, {n, 4*n, 4*n} };
      fftw_iodim io_is[1]={ {4, 1, 1} };
      fftw_plan p1=fftw_plan_guru_dft(2, io_n, 1, io_is,
				      reinterpret_cast<fftw_complex *>(v),
				      reinterpret_cast<fftw_complex *>(v),
				      FFTW_FORWARD, FFTW_MEASURE);
      fftw_plan p2=fftw_plan_guru_dft(2, io_n, 1, io_is,
				      reinterpret_cast<fftw_complex *>(v),
				      reinterpret_cast<fftw_complex *>(v),
				      FFTW_BACKWARD, FFTW_MEASURE);
      for (int j=0; j<n; ++j)
	for (int i=0; i<n; ++i) {
	  v[4*(j*n+i)]=complex(static_cast<double>(i+1)/static_cast<double>(n),
			       static_cast<double>(j+1)/static_cast<double>(n));
	  v[4*(j*n+i)+1]=complex(static_cast<double>(i+1)/static_cast<double>(n),
				 static_cast<double>(j+1)/static_cast<double>(n));
	  v[4*(j*n+i)+2]=complex(static_cast<double>(i+1)/static_cast<double>(n),
				 static_cast<double>(j+1)/static_cast<double>(n));
	  v[4*(j*n+i)+3]=complex(static_cast<double>(i+1)/static_cast<double>(n),
				 static_cast<double>(j+1)/static_cast<double>(n));
	}
      timer T1;
      T1.start();
      int i=0;
      do {
	fftw_execute_dft(p1,
			 reinterpret_cast<fftw_complex *>(v),
			 reinterpret_cast<fftw_complex *>(v));
	fftw_execute_dft(p2,
			 reinterpret_cast<fftw_complex *>(v),
			 reinterpret_cast<fftw_complex *>(v));
	++i;
      } while (T1.time()<4 and i<16);
      double t1=T1.time()/i;
      fftw_free(v);
      fftw_destroy_plan(p1);
      fftw_destroy_plan(p2);
      out << n << '\t' << t1 << std::endl;
      std::cerr << "2d\tp = " << p
		<< "\tN = " << n
		<< "\ttime = " << t1 << std::endl;
    } else
      std::cerr << "2d\tp = " << p
		<< "\tN = " << n
		<< "\tnot enough memory" << std::endl;
  }
}
 
int main() {
  fftw_init_threads();
  for (int p=8; p<=8; ++p) {
    {
      time_1d(p, std::cout);
    }
    {
      time_2d(p, std::cout);
    }
  }
  return EXIT_SUCCESS;
}
