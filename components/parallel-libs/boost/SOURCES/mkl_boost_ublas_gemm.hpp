//******************************************************************************
//  Copyright(C) 2008-2013 Intel Corporation. All Rights Reserved.
//
//  The source code, information  and  material ("Material") contained herein is
//  owned  by Intel Corporation or its suppliers or licensors, and title to such
//  Material remains  with Intel Corporation  or its suppliers or licensors. The
//  Material  contains proprietary information  of  Intel or  its  suppliers and
//  licensors. The  Material is protected by worldwide copyright laws and treaty
//  provisions. No  part  of  the  Material  may  be  used,  copied, reproduced,
//  modified, published, uploaded, posted, transmitted, distributed or disclosed
//  in any way  without Intel's  prior  express written  permission. No  license
//  under  any patent, copyright  or  other intellectual property rights  in the
//  Material  is  granted  to  or  conferred  upon  you,  either  expressly,  by
//  implication, inducement,  estoppel or  otherwise.  Any  license  under  such
//  intellectual  property  rights must  be express  and  approved  by  Intel in
//  writing.
//
//  *Third Party trademarks are the property of their respective owners.
//
//  Unless otherwise  agreed  by Intel  in writing, you may not remove  or alter
//  this  notice or  any other notice embedded  in Materials by Intel or Intel's
//  suppliers or licensors in any way.
//
//******************************************************************************
// Content:
//     Intel(R) Math Kernel Library (MKL) overloaded Boost/uBLAS prod()
//******************************************************************************

#ifndef _MKL_BOOST_UBLAS_GEMM_
#define _MKL_BOOST_UBLAS_GEMM_

#include <boost/version.hpp>
#if defined (BOOST_VERSION) && (BOOST_VERSION >= 103401)

#include <boost/numeric/ublas/detail/concepts.hpp>
#include <boost/numeric/ublas/matrix.hpp>

#include "mkl_cblas.h"

#ifndef MKL_BOOST_UBLAS_INLINE
#define MKL_BOOST_UBLAS_INLINE inline
#endif

namespace boost { namespace numeric { namespace ublas {

namespace mkl {

    template<class E1, class E2>
    BOOST_UBLAS_INLINE
    typename matrix_matrix_binary_traits<typename E1::value_type, E1,
                                         typename E2::value_type, E2>::result_type
    gemm(const matrix_expression<E1> &e1,
         const matrix_expression<E2> &e2)
    {
        typedef typename matrix_matrix_binary_traits<typename E1::value_type, E1,
                                                                 typename E2::value_type, E2>::storage_category storage_category;
        typedef typename matrix_matrix_binary_traits<typename E1::value_type, E1,
                                                                 typename E2::value_type, E2>::orientation_category orientation_category;
        return prod (e1, e2, storage_category (), orientation_category ());
    } // For unsupported matrix types.
    template<class T>
    MKL_BOOST_UBLAS_INLINE
    void gemm(const  CBLAS_ORDER Order, const  CBLAS_TRANSPOSE TransA,
            const  CBLAS_TRANSPOSE TransB, const MKL_INT m, const MKL_INT n,
            const MKL_INT k, const T alpha, const T *a,
            const MKL_INT lda, const T *b, const MKL_INT ldb,
            const T beta, T *c, const MKL_INT ldc)
    {}// To resolve externals for unsupported matrix types. Never is called.
    template<>
    MKL_BOOST_UBLAS_INLINE
    void
    gemm(const  CBLAS_ORDER Order, const  CBLAS_TRANSPOSE TransA,
         const  CBLAS_TRANSPOSE TransB, const MKL_INT m, const MKL_INT n,
         const MKL_INT k, const double alpha, const double *a,
         const MKL_INT lda, const double *b, const MKL_INT ldb,
         const double beta, double *c, const MKL_INT ldc)
    {
        cblas_dgemm(Order, TransA, TransB, m, n, k, alpha, a, lda, b, ldb, beta, c, ldc);
    }
    template<>
    MKL_BOOST_UBLAS_INLINE
    void
    gemm(const  CBLAS_ORDER Order, const  CBLAS_TRANSPOSE TransA,
         const  CBLAS_TRANSPOSE TransB, const MKL_INT m, const MKL_INT n,
         const MKL_INT k, const float alpha, const float *a,
         const MKL_INT lda, const float *b, const MKL_INT ldb,
         const float beta, float *c, const MKL_INT ldc)
    {
        cblas_sgemm(Order, TransA, TransB, m, n, k, alpha, a, lda, b, ldb, beta, c, ldc);
    }
    template<>
    MKL_BOOST_UBLAS_INLINE
    void
    gemm(const  CBLAS_ORDER Order, const  CBLAS_TRANSPOSE TransA,
         const  CBLAS_TRANSPOSE TransB, const MKL_INT m, const MKL_INT n,
         const MKL_INT k, const std::complex<double> alpha, const std::complex<double> *a,
         const MKL_INT lda, const std::complex<double> *b, const MKL_INT ldb,
         const std::complex<double> beta, std::complex<double> *c, const MKL_INT ldc)
    {
        cblas_zgemm(Order, TransA, TransB, m, n, k, static_cast<const void *>(&alpha),
            static_cast<const void *>(a), lda, static_cast<const void *>(b), ldb,
            static_cast<const void *>(&beta), static_cast<void *>(c), ldc);
    }
    template<>
    MKL_BOOST_UBLAS_INLINE
    void
    gemm(const  CBLAS_ORDER Order, const  CBLAS_TRANSPOSE TransA,
         const  CBLAS_TRANSPOSE TransB, const MKL_INT m, const MKL_INT n,
         const MKL_INT k, const std::complex<float> alpha, const std::complex<float> *a,
         const MKL_INT lda, const std::complex<float> *b, const MKL_INT ldb,
         const std::complex<float> beta, std::complex<float> *c, const MKL_INT ldc)
    {
        cblas_cgemm(Order, TransA, TransB, m, n, k, static_cast<const void *>(&alpha),
            static_cast<const void *>(a), lda, static_cast<const void *>(b), ldb,
            static_cast<const void *>(&beta), static_cast<void *>(c), ldc);
    }

    template<class T, class F, class A>
    MKL_BOOST_UBLAS_INLINE
    T *  // M
    pointer(const matrix<T,F,A> &m)
    {
        //return &m(0,0);
        return const_cast<T*>(&m.data().begin()[0]);
    }
    template<class T, class F, class A>
    MKL_BOOST_UBLAS_INLINE
    T *  // trans(M)
    pointer(const matrix_unary2<matrix<T,F,A>,scalar_identity<T> > &m)
    {
        return pointer(m.expression().expression());
    }
    template<class T, class F, class A>
    MKL_BOOST_UBLAS_INLINE
    T *  // trans(conj(M))
    pointer(const matrix_unary2<matrix_unary1<matrix<T,F,A>,scalar_conj<T> > const ,scalar_identity<T> > &m)
    {
        return pointer(m.expression().expression().expression());
    }
    template<class T, class F, class A>
    MKL_BOOST_UBLAS_INLINE
    T *  // conj(trans(M))
    pointer(const matrix_unary1<matrix_unary2<matrix<T,F,A>,scalar_identity<T> >,scalar_conj<T> > &m)
    {
        return pointer(m.expression().expression().expression());
    }

    template<class E>
    MKL_BOOST_UBLAS_INLINE
    MKL_INT
    leading_dimension(const matrix_expression<E> &e, row_major_tag)
    {
        return e().size1();
    }
    template<class E>
    MKL_BOOST_UBLAS_INLINE
    MKL_INT
    leading_dimension(const matrix_expression<E> &e, column_major_tag)
    {
        return e().size2();
    }
    template<class T, class F, class A>
    MKL_BOOST_UBLAS_INLINE
    MKL_INT
    leading_dimension(const matrix<T,F,A> &m, row_major_tag)
    {
        return m.size2();
    }
    template<class T, class F, class A>
    MKL_BOOST_UBLAS_INLINE
    MKL_INT
    leading_dimension(const matrix<T,F,A> &m, column_major_tag)
    {
        return m.size1();
    }

    template<class T>
    MKL_BOOST_UBLAS_INLINE
    CBLAS_ORDER
    storage_layout(T);
    template<>
    MKL_BOOST_UBLAS_INLINE
    CBLAS_ORDER
    storage_layout(row_major_tag) {
        return CblasRowMajor;
    }
    template<>
    MKL_BOOST_UBLAS_INLINE
    CBLAS_ORDER
    storage_layout(column_major_tag) {
        return CblasColMajor;
    }

    template<class T>
    MKL_BOOST_UBLAS_INLINE
    int supported_type(T)
    { return 0; }
    template<>
    MKL_BOOST_UBLAS_INLINE
    int supported_type(float)
    { return 1; }
    template<>
    MKL_BOOST_UBLAS_INLINE
    int supported_type(double)
    { return 2; }
    template<>
    MKL_BOOST_UBLAS_INLINE
    int supported_type(std::complex<float>)
    { return 3; }
    template<>
    MKL_BOOST_UBLAS_INLINE
    int supported_type(std::complex<double>)
    { return 4; }

    template<class E1, class E2, class T, class F, class A>
    MKL_BOOST_UBLAS_INLINE
    void
    gemm(const  CBLAS_TRANSPOSE transa, const  CBLAS_TRANSPOSE transb,
         const matrix_expression<E1> &a, const matrix_expression<E2> &b,
         matrix<T,F,A> &c)
    {
        typedef T value_type;
        typedef typename F::orientation_category orientation_category;
        if(supported_type(value_type()) > 0) {
            MKL_INT m = a().size1();
            MKL_INT k = a().size2();
            MKL_INT n = b().size2();
            MKL_INT lda = leading_dimension(a(),orientation_category());
            MKL_INT ldb = leading_dimension(b(),orientation_category());
            MKL_INT ldc = leading_dimension(c,orientation_category());
			
	/*boost 1.57 change the function OneElement definition from 1.0 to 0.0 in boost/numeric/ublas/detail/concepts.hpp 
	template<class T>
    T
    OneElement (T) {
        return T(0.0);
    }
		They explain this in boost/numeric/ublas/detail/concepts.hpp
		Replaced the ZeroElement and OneElement functions with the templated versions
		because the former where giving warnings with clang.  Anyway, It seems a bug or something else. so change 
	//value_type alpha = OneElement(value_type());  // boost/numeric/ublas/detail/concepts.hpp  to 
	*/    
            value_type alpha = 1.0;  
			value_type beta = ZeroElement(value_type());  // boost/numeric/ublas/detail/concepts.hpp
            CBLAS_ORDER layout = storage_layout(orientation_category());
            T *pa = pointer(a());
            T *pb = pointer(b());
            T *pc = pointer(c);
            gemm(layout, transa, transb, m, n, k, alpha, pa, lda, pb, ldb, beta, pc, ldc);
        } else {
            c = gemm(a,b);
        }
    }

}  // namespace mkl

}}}
#endif  // BOOST_VERSION
#endif  // _MKL_BOOST_UBLAS_GEMM_
