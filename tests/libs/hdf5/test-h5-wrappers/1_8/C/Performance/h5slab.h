/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/* Common definitions used by h5slab example programs (h5slabread, h5slabwrite).
 *
 * Created by Albert Cheng 2010/7/13.
 */
#include <stdlib.h>
#include <string.h>
#include "hdf5.h"
#if 1
#define NX    65536  
#define NY    65536       /* dataset dimensions */
#define CX    256        /* height of hyperslab */
#define CY    4096        /* width of hyperslab */
#else
#define NX    256  
#define NY    256       /* dataset dimensions */
#define CX    32        /* height of hyperslab */
#define CY    16        /* width of hyperslab */
#endif
#define RC    (NX/CX)
#define CC    (NY/CY)
