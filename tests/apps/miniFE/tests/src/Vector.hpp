#ifndef _Vector_hpp_
#define _Vector_hpp_

//@HEADER
// ************************************************************************
//
// MiniFE: Simple Finite Element Assembly and Solve
// Copyright (2006-2013) Sandia Corporation
//
// Under terms of Contract DE-AC04-94AL85000, there is a non-exclusive
// license for use of this work by or on behalf of the U.S. Government.
//
// This library is free software; you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as
// published by the Free Software Foundation; either version 2.1 of the
// License, or (at your option) any later version.
//
// This library is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this library; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
// USA
//
// ************************************************************************
//@HEADER

#include <vector>

#include <assert.h>
#include <stdlib.h>
#include <sys/mman.h>
#define HUGE_PAGE_SIZE (2 * 1024 * 1024)
#define ALIGN_TO_PAGE_SIZE(x) \
    (((x) + HUGE_PAGE_SIZE - 1) / HUGE_PAGE_SIZE * HUGE_PAGE_SIZE)

namespace miniFE {

#ifdef MINIFE_HUGE_PAGES
void *malloc_huge_pages(size_t size)
{
    // Use 1 extra page to store allocation metadata
    // (libhugetlbfs is more efficient in this regard)
    size_t real_size = ALIGN_TO_PAGE_SIZE(size + HUGE_PAGE_SIZE);
    char *ptr = (char *)mmap(NULL, real_size, PROT_READ | PROT_WRITE,
                  MAP_PRIVATE | MAP_ANONYMOUS |
                  MAP_POPULATE | MAP_HUGETLB, -1, 0);
    if (ptr == MAP_FAILED) {
	   fprintf(stderr, "MFE: Allocation of huge pages for vector failed, resorting to malloc...\n");
           // The mmap() call failed. Try to malloc instead
           ptr = (char *)malloc(real_size);
           if (ptr == NULL) return NULL;
           real_size = 0;
    }
    // Save real_size since mmunmap() requires a size parameter
    *((size_t *)ptr) = real_size;
    // Skip the page with metadata
    return ptr + HUGE_PAGE_SIZE;
}
#endif

template<typename Scalar,
         typename LocalOrdinal,
         typename GlobalOrdinal>
struct Vector {
  typedef Scalar ScalarType;
  typedef LocalOrdinal LocalOrdinalType;
  typedef GlobalOrdinal GlobalOrdinalType;

  Vector(GlobalOrdinal startIdx, LocalOrdinal local_sz)
   : startIndex(startIdx),
     local_size(local_sz)
  {
#ifdef MINIFE_HUGE_PAGES
    coefs = (MINIFE_SCALAR*) malloc_huge_pages((sizeof(MINIFE_SCALAR) * local_size) + 64);
#else
    posix_memalign((void**) &coefs, 64, sizeof(MINIFE_SCALAR) * local_size);
#endif

    if(((unsigned long long int) coefs) % 64 > 0) {
	coefs = coefs + (((unsigned long long int )coefs) % 64);
    }

    const MINIFE_LOCAL_ORDINAL n = (MINIFE_LOCAL_ORDINAL) local_sz;

    #pragma omp parallel for
    for(MINIFE_LOCAL_ORDINAL i = 0; i < n; ++i) {
	coefs[i] = 0;
    }
  }

  ~Vector()
  {
  }

  const GlobalOrdinal startIndex;
  const LocalOrdinal local_size;
//  std::vector<Scalar> coefs;

  MINIFE_SCALAR* MINIFE_RESTRICT coefs __attribute__ ((aligned (64)));
};


}//namespace miniFE

#endif

