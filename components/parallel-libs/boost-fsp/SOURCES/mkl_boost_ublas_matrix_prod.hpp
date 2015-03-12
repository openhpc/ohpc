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

#ifndef _MKL_BOOST_UBLAS_MATRIX_PROD_
#define _MKL_BOOST_UBLAS_MATRIX_PROD_

#ifdef NDEBUG

#include <boost/version.hpp>
#if defined (BOOST_VERSION) && (BOOST_VERSION >= 103401)

#include <boost/numeric/ublas/matrix.hpp>

#include "mkl_boost_ublas_gemm.hpp"

namespace boost { namespace numeric { namespace ublas {

    template<class T, class F, class A>
    MKL_BOOST_UBLAS_INLINE
    matrix<T,F,A>    // prod( m1, m2 )
    prod(const matrix<T,F,A> &m1,
         const matrix<T,F,A> &m2)
    {
        matrix<T,F,A> temporary(m1.size1(),m2.size2());
        mkl::gemm(CblasNoTrans, CblasNoTrans, m1, m2, temporary);
        return temporary;
    }
    template<class T, class F, class A>
    MKL_BOOST_UBLAS_INLINE
    matrix<T,F,A>    // prod( trans(m1), m2 )
    prod(const matrix_unary2<matrix<T,F,A>,scalar_identity<T> > &m1,
         const matrix<T,F,A> &m2)
    {
        matrix<T,F,A> temporary(m1.size1(),m2.size2());
        mkl::gemm(CblasTrans, CblasNoTrans, m1, m2, temporary);
        return temporary;
    }
    template<class T, class F, class A>
    MKL_BOOST_UBLAS_INLINE
    matrix<T,F,A>    // prod( trans(conj(m1)), m2 )
    prod(const matrix_unary2<matrix_unary1<matrix<T,F,A>,scalar_conj<T> > const ,scalar_identity<T> > &m1,
         const matrix<T,F,A> &m2)
    {
        matrix<T,F,A> temporary(m1.size1(),m2.size2());
        mkl::gemm(CblasConjTrans, CblasNoTrans, m1, m2, temporary);
        return temporary;
    }
    template<class T, class F, class A>
    MKL_BOOST_UBLAS_INLINE
    matrix<T,F,A>    // prod( conj(trans(m1)), m2 )
    prod(const matrix_unary1<matrix_unary2<matrix<T,F,A>,scalar_identity<T> >,scalar_conj<T> > &m1,
         const matrix<T,F,A> &m2)
    {
        matrix<T,F,A> temporary(m1.size1(),m2.size2());
        mkl::gemm(CblasConjTrans, CblasNoTrans, m1, m2, temporary);
        return temporary;
    }
    template<class T, class F, class A>
    MKL_BOOST_UBLAS_INLINE
    matrix<T,F,A>    // prod( m1, trans(m2) )
    prod(const matrix<T,F,A> &m1,
         const matrix_unary2<matrix<T,F,A>,scalar_identity<T> > &m2)
    {
        matrix<T,F,A> temporary(m1.size1(),m2.size2());
        mkl::gemm(CblasNoTrans, CblasTrans, m1, m2, temporary);
        return temporary;
    }
    template<class T, class F, class A>
    MKL_BOOST_UBLAS_INLINE
    matrix<T,F,A>    // prod( trans(m1), trans(m2) )
    prod(const matrix_unary2<matrix<T,F,A>,scalar_identity<T> > &m1,
         const matrix_unary2<matrix<T,F,A>,scalar_identity<T> > &m2)
    {
        matrix<T,F,A> temporary(m1.size1(),m2.size2());
        mkl::gemm(CblasTrans, CblasTrans, m1, m2, temporary);
        return temporary;
    }
    template<class T, class F, class A>
    MKL_BOOST_UBLAS_INLINE
    matrix<T,F,A>    // prod( trans(conj(m1)), trans(m2) )
    prod(const matrix_unary2<matrix_unary1<matrix<T,F,A>,scalar_conj<T> > const ,scalar_identity<T> > &m1,
         const matrix_unary2<matrix<T,F,A>,scalar_identity<T> > &m2)
    {
        matrix<T,F,A> temporary(m1.size1(),m2.size2());
        mkl::gemm(CblasConjTrans, CblasTrans, m1, m2, temporary);
        return temporary;
    }
    template<class T, class F, class A>
    MKL_BOOST_UBLAS_INLINE
    matrix<T,F,A>    // prod( conj(trans(m1)), trans(m2) )
    prod(const matrix_unary1<matrix_unary2<matrix<T,F,A>,scalar_identity<T> >,scalar_conj<T> > &m1,
         const matrix_unary2<matrix<T,F,A>,scalar_identity<T> > &m2)
    {
        matrix<T,F,A> temporary(m1.size1(),m2.size2());
        mkl::gemm(CblasConjTrans, CblasTrans, m1, m2, temporary);
        return temporary;
    }
    template<class T, class F, class A>
    MKL_BOOST_UBLAS_INLINE
    matrix<T,F,A>    // prod( m1, trans(conj(m2)) )
    prod(const matrix<T,F,A> &m1,
         const matrix_unary2<matrix_unary1<matrix<T,F,A>,scalar_conj<T> > const ,scalar_identity<T> > &m2)
    {
        matrix<T,F,A> temporary(m1.size1(),m2.size2());
        mkl::gemm(CblasNoTrans, CblasConjTrans, m1, m2, temporary);
        return temporary;
    }
    template<class T, class F, class A>
    MKL_BOOST_UBLAS_INLINE
    matrix<T,F,A>    // prod( trans(m1), trans(conj(m2)) )
    prod(const matrix_unary2<matrix<T,F,A>,scalar_identity<T> > &m1,
         const matrix_unary2<matrix_unary1<matrix<T,F,A>,scalar_conj<T> > const ,scalar_identity<T> > &m2)
    {
        matrix<T,F,A> temporary(m1.size1(),m2.size2());
        mkl::gemm(CblasTrans, CblasConjTrans, m1, m2, temporary);
        return temporary;
    }
    template<class T, class F, class A>
    MKL_BOOST_UBLAS_INLINE
    matrix<T,F,A>    // prod( trans(conj(m1)), trans(conj(m2)) )
    prod(const matrix_unary2<matrix_unary1<matrix<T,F,A>,scalar_conj<T> > const ,scalar_identity<T> > &m1,
         const matrix_unary2<matrix_unary1<matrix<T,F,A>,scalar_conj<T> > const ,scalar_identity<T> > &m2)
    {
        matrix<T,F,A> temporary(m1.size1(),m2.size2());
        mkl::gemm(CblasConjTrans, CblasConjTrans, m1, m2, temporary);
        return temporary;
    }
    template<class T, class F, class A>
    MKL_BOOST_UBLAS_INLINE
    matrix<T,F,A>    // prod( conj(trans(m1)), trans(conj(m2)) )
    prod(const matrix_unary1<matrix_unary2<matrix<T,F,A>,scalar_identity<T> >,scalar_conj<T> > &m1,
         const matrix_unary2<matrix_unary1<matrix<T,F,A>,scalar_conj<T> > const ,scalar_identity<T> > &m2)
    {
        matrix<T,F,A> temporary(m1.size1(),m2.size2());
        mkl::gemm(CblasConjTrans, CblasConjTrans, m1, m2, temporary);
        return temporary;
    }
    template<class T, class F, class A>
    MKL_BOOST_UBLAS_INLINE
    matrix<T,F,A>    // prod( m1, conj(trans(m2)) )
    prod(const matrix<T,F,A> &m1,
         const matrix_unary1<matrix_unary2<matrix<T,F,A>,scalar_identity<T> >,scalar_conj<T> > &m2)
    {
        matrix<T,F,A> temporary(m1.size1(),m2.size2());
        mkl::gemm(CblasNoTrans, CblasConjTrans, m1, m2, temporary);
        return temporary;
    }
    template<class T, class F, class A>
    MKL_BOOST_UBLAS_INLINE
    matrix<T,F,A>    // prod( trans(m1), conj(trans(m2)) )
    prod(const matrix_unary2<matrix<T,F,A>,scalar_identity<T> > &m1,
         const matrix_unary1<matrix_unary2<matrix<T,F,A>,scalar_identity<T> >,scalar_conj<T> > &m2)
    {
        matrix<T,F,A> temporary(m1.size1(),m2.size2());
        mkl::gemm(CblasTrans, CblasConjTrans, m1, m2, temporary);
        return temporary;
    }
    template<class T, class F, class A>
    MKL_BOOST_UBLAS_INLINE
    matrix<T,F,A>    // prod( trans(conj(m1)), conj(trans(m2)) )
    prod(const matrix_unary2<matrix_unary1<matrix<T,F,A>,scalar_conj<T> > const ,scalar_identity<T> > &m1,
         const matrix_unary1<matrix_unary2<matrix<T,F,A>,scalar_identity<T> >,scalar_conj<T> > &m2)
    {
        matrix<T,F,A> temporary(m1.size1(),m2.size2());
        mkl::gemm(CblasConjTrans, CblasConjTrans, m1, m2, temporary);
        return temporary;
    }
    template<class T, class F, class A>
    MKL_BOOST_UBLAS_INLINE
    matrix<T,F,A>    // prod( conj(trans(m1)), conj(trans(m2)) )
    prod(const matrix_unary1<matrix_unary2<matrix<T,F,A>,scalar_identity<T> >,scalar_conj<T> > &m1,
         const matrix_unary1<matrix_unary2<matrix<T,F,A>,scalar_identity<T> >,scalar_conj<T> > &m2)
    {
        matrix<T,F,A> temporary(m1.size1(),m2.size2());
        mkl::gemm(CblasConjTrans, CblasConjTrans, m1, m2, temporary);
        return temporary;
    }

}}}
#endif  // BOOST_VERSION
#endif  // NDEBUG
#endif  // _MKL_BOOST_UBLAS_MATRIX_PROD_
