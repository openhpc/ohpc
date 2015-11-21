#ifndef TBBNODE_HPP_
#define TBBNODE_HPP_

#include <tbb/blocked_range.h>
#include <tbb/parallel_for.h>
#include <tbb/parallel_reduce.h>
#include <tbb/task_scheduler_init.h>
#include <stdlib.h>

#include <NoOpMemoryModel.hpp>

#include <iostream> // debug

template <class WDPin>
struct BlockedRangeWDP {
  mutable WDPin wd;
  BlockedRangeWDP(WDPin &in) : wd(in) {}
  inline void operator()(tbb::blocked_range<int> &rng) const
  {
    for(int i=rng.begin(); i<rng.end(); ++i) {
      wd(i);
    }
  }
};

template <class WDPin>
struct BlockedRangeWDPReducer {
  WDPin wd;
  BlockedRangeWDPReducer(WDPin &in) : wd(in) {}
  BlockedRangeWDPReducer(BlockedRangeWDPReducer &in, tbb::split) : wd(in.wd)
  {
    wd.result = wd.identity();
  }
  void operator()(tbb::blocked_range<int> &rng)
  { 
    for(int i=rng.begin(); i<rng.end(); ++i) {
      wd.result = wd.reduce(wd.result, wd.generate(i));
    }
  }
  inline void join( const BlockedRangeWDPReducer<WDPin> &other ) {
    wd.result = wd.reduce( wd.result, other.wd.result );
  }
};

class TBBNode : public NoOpMemoryModel {
  public:

    TBBNode(int numThreads=0) {
      if (numThreads >= 1) {
        tsi_.initialize(numThreads);
      }
      else {
        tsi_.initialize(tbb::task_scheduler_init::automatic);
      }
    }

    ~TBBNode() {}

    template <class WDP>
    void parallel_for(int length, WDP wd) {
      BlockedRangeWDP<WDP> tbb_wd(wd);
      tbb::parallel_for(tbb::blocked_range<int>(0,length), tbb_wd, tbb::auto_partitioner()); 
    }

    template <class WDP>
    void parallel_reduce(int length, WDP &wd) {
      BlockedRangeWDPReducer<WDP> tbb_wd(wd);
      tbb::parallel_reduce(tbb::blocked_range<int>(0,length), tbb_wd, tbb::auto_partitioner());
      wd.result = tbb_wd.wd.result;  // have to put result from final tbb_wd into orginal wd
    }

  private:
    static tbb::task_scheduler_init tsi_;
};

#endif
