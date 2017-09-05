#include <iostream>
#include <fstream>
#include <cstdlib>
#include <cassert>

extern "C" {
  void dsyev_(const char* jobz, const char* uplo, int* n,
	      double* a, int* lda,
	      double* w, double* work, int* lwork, int* info);
}

void syev(const char jobz, const char uplo, int n, double* a, double* w)
{ 
  int lwork, info;
  double tmp;
  lwork=-1;
  dsyev_(&jobz,&uplo,&n,a,&n,w,&tmp,&lwork,&info);
  assert(info==0);
  lwork=(int)(tmp+0.1);
  double* work=new double[lwork];
  dsyev_(&jobz,&uplo,&n,a,&n,w,work,&lwork,&info);
  delete[] work;
  assert(info==0);
}

int main() {

  srand48(42);

  for (int k=0;k<10;++k) {

    const int N=200+(int)(200.0*drand48());

    double* mat=new double[N*N];
    double* eig=new double[N];

    for (int j=0;j<N;++j) { 
      for (int i=j;i<N;++i) {
	mat[i+j*N]=drand48();
	mat[j+i*N]=mat[i+j*N];
      }
      mat[j+j*N]+=2.0;
    }
    syev('V','U',N,&mat[0],&eig[0]);

    std::cout << N << "  " << eig[0] << std::endl;

    delete[] eig;
    delete[] mat;

  }
  
}
