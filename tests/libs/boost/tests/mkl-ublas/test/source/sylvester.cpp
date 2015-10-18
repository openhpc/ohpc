/*******************************************************************************
!   Copyright(C) 2008-2013 Intel Corporation. All Rights Reserved.
!   
!   The source code, information  and  material ("Material") contained herein is
!   owned  by Intel Corporation or its suppliers or licensors, and title to such
!   Material remains  with Intel Corporation  or its suppliers or licensors. The
!   Material  contains proprietary information  of  Intel or  its  suppliers and
!   licensors. The  Material is protected by worldwide copyright laws and treaty
!   provisions. No  part  of  the  Material  may  be  used,  copied, reproduced,
!   modified, published, uploaded, posted, transmitted, distributed or disclosed
!   in any way  without Intel's  prior  express written  permission. No  license
!   under  any patent, copyright  or  other intellectual property rights  in the
!   Material  is  granted  to  or  conferred  upon  you,  either  expressly,  by
!   implication, inducement,  estoppel or  otherwise.  Any  license  under  such
!   intellectual  property  rights must  be express  and  approved  by  Intel in
!   writing.
!   
!   *Third Party trademarks are the property of their respective owners.
!   
!   Unless otherwise  agreed  by Intel  in writing, you may not remove  or alter
!   this  notice or  any other notice embedded  in Materials by Intel or Intel's
!   suppliers or licensors in any way.
!
!*******************************************************************************
!  Content:
!      This code solves a very special case of Sylvester equation X-BXC=D
!      Example demonstrates usage of the MKL ublas header.
!      Matrices D and X can be rectangular.
!      Arguments (default: 1 3 4)
!         sylvester [count [rows [columns]]]
!      where count is iterations limit, rows of D or X, columns of D or X
!******************************************************************************/

#include <boost/numeric/ublas/matrix.hpp>
#include <boost/numeric/ublas/io.hpp>

#include <boost/timer.hpp>

#ifdef MKL
#include "mkl_boost_ublas_matrix_prod.hpp"
#endif

using namespace boost::numeric::ublas;

template<class M, class T>
M &
sylvester(const M &B, const M &C, const M &D, M &X, const T &eps, long max_iter);

int main (int argc, char **argv) {
    long i;
    size_t csize1 = 3;
    size_t csize2 = 4;
    long count = 1;
    double epsilon = 1e-15;

    if(argc>1) count = atol(argv[1]);
    if(argc>2) csize1 = atol(argv[2]);
    if(argc>3) csize2 = atol(argv[3]);
    else csize2 = csize1;

#ifdef MKL
    std::cout << std::endl << "  MKL: max iterations: " << count << " size: " << csize1 << "x" << csize2 << std::endl;
#else
    std::cout << std::endl << "uBLAS: max iterations: " << count << " size: " << csize1 << "x" << csize2 << std::endl;
#endif

    matrix<double> B(zero_matrix<double>(csize1, csize1));
    matrix<double> C(zero_matrix<double>(csize2, csize2));
    matrix<double> D(scalar_matrix<double>(csize1, csize2, 1));
    matrix<double> X(csize1, csize2);

    for(i=0;i<csize1;i++) {
        B(i,i) = i/(2.0*csize1);   // fill main diagonal
        if(i+1<csize1)
            B(i,i+1) = 0.6;        // and 1 upper diagonal
    }

    for(i=0;i<csize2;i++) {
        C(i,i) = i/(2.0*csize2);   // fill main diagonal
        if(i>0)
            C(i,i-1) = 0.5;        // and 1 lower diagonal
    }

    X = sylvester(B,C,D,X,epsilon,count);

    return 0;
}

template<class M, class T>
M &
sylvester(const M &B, const M &C, const M &D, M &X, const T &eps, long max_iter) {
    T delta = static_cast<T> (1);
    T Emax = static_cast<T> (1e100);
    long count = 0;
    matrix<T>B1(B);
    matrix<T>C1(C);
    X = D;

    boost::timer tim; // Start timing

    while (eps < delta && delta < Emax) {
        X = X + prod(matrix<T>(prod(B1,X)),C1);         // X = X + B*X*C;
        B1 = prod(B1,B1);                               // B = B*B;
        C1 = prod(C1,C1);                               // C = C*C;
        delta = norm_frobenius(B1) + norm_frobenius(C1);// delta  = norm(B) + norm(C);
        if(count++ > max_iter)                          // k=k+1
            break;
    }
    {
        double tsec = tim.elapsed(); // Finish timing
        int m = (int)(tsec / 60);
        double s = tsec - m*60;
        std::cout << "Total time" << " " << m << ":";
        if(s<10) { std::cout << "0"; }
        std::cout << s << " (" << tsec << ")" <<  std::endl;
    }

    // norm( X - BXC - D )
    T norm = norm_frobenius(X - prod(matrix<T>(prod(B,X)),C) - D);

    std::cout << "Iterations: " << count << " norm: " << norm << std::endl;

    return X;
}
