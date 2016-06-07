#ifdef MINIFE_HAVE_TBB

#include "TBBNode.hpp"

tbb::task_scheduler_init TBBNode::tsi_(tbb::task_scheduler_init::deferred);

#endif

