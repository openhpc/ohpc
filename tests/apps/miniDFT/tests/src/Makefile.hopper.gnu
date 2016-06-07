#Uncomment the following line to enable OpenMP
#USE_OPENMP = TRUE

FFTW_INCL = -I/opt/fftw/3.3.0.1/x86_64/include
FFTW_LIBS = -L/opt/fftw/3.3.0.1/x86_64/lib -lfftw3
BLAS_LIBS =
SCALAPACK_LIBS = 

DFLAGS = -D__GFORTRAN 
#DFLAGS += -D__IPM

CC = cc 
CFLAGS = -O3

FC = ftn 
FFLAGS = -O3 -cpp -x f95-cpp-input

LD = ftn
LDFLAGS = 

ifeq ($(USE_OPENMP), TRUE)

  #note that this overrides the earlier definition of FFTW_LIBS
  FFTW_LIBS  = -L/opt/fftw/3.3.0.1/x86_64/lib -lfftw3_threads -lfftw3 -lm

  DFLAGS    += 

  FFLAGS    += -fopenmp
  LDFLAGS   += -fopenmp

endif

include Makefile.base
