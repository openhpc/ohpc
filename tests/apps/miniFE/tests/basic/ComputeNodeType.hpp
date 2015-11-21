#ifndef _ComputeNodeType_hpp_
#define _ComputeNodeType_hpp_

#if defined(MINIFE_HAVE_TBB)

#include <tbb/task_scheduler_init.h>
#include <TBBNode.hpp>
typedef TBBNode ComputeNodeType;

#elif defined(MINIFE_HAVE_TPI)

#include <TPI.h>
#include <TPINode.hpp>
typedef TPINode ComputeNodeType;

#elif defined(MINIFE_HAVE_CUDA)

#include <CudaNode.hpp>
typedef CUDANode ComputeNodeType;

#else

#include <SerialComputeNode.hpp>
typedef SerialComputeNode ComputeNodeType;

#endif

#endif

