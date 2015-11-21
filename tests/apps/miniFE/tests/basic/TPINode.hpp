#ifndef TPINODE_HPP_
#define TPINODE_HPP_

#include <TPI.h>

#include <NoOpMemoryModel.hpp>

#include <iostream> // debug

inline
void tpi_work_span(TPI_Work* work, int n,
                   size_t& ibeg, size_t& iend)
{
  const int chunk = ( n + work->count - 1 ) / work->count ;

  iend = chunk * ( work->rank + 1 );
  ibeg = chunk * ( work->rank );

  if ( n < iend ) { iend = n; }
}

template<class WDP>
void tpi_execute(TPI_Work * work)
{
  const WDP* const_wdp = static_cast<const WDP*>(work->info);
  WDP* wdp = const_cast<WDP*>(const_wdp);
  size_t n = wdp->n;
  size_t ibeg = 0, iend = n;
  tpi_work_span(work, n, ibeg, iend);
  for(size_t i=ibeg; i<iend; ++i) {
    (*wdp)(i);
  }
}

template<class WDP>
void tpi_reduction_work(TPI_Work * work)
{
  const WDP* wdp = static_cast<const WDP*>(work->info);
  size_t n = wdp->n;
  size_t ibeg = 0, iend = n;
  tpi_work_span(work, n, ibeg, iend);

  typedef typename WDP::ReductionType ReductionType;
  ReductionType tmpres = wdp->result, tmpi;

  for(size_t i=ibeg; i<iend; ++i) {
    tmpi = wdp->generate(i);
    tmpres = wdp->reduce(tmpres, tmpi);
  }
  *(static_cast<ReductionType*>(work->reduce)) = tmpres;
}

template<class WDP>
void tpi_reduction_join(TPI_Work * work, const void* src)
{
  typedef typename WDP::ReductionType ReductionType;

  const WDP* wdp = static_cast<const WDP*>(work->info);

  ReductionType& work_reduce = *(static_cast<ReductionType*>(work->reduce));

  work_reduce = wdp->reduce(work_reduce, *(static_cast<const ReductionType*>(src)) );
}

template<class WDP>
void tpi_reduction_init(TPI_Work * work)
{
  typedef typename WDP::ReductionType ReductionType;

  const WDP* wdp = static_cast<const WDP*>(work->info);

  *(static_cast<ReductionType*>(work->reduce)) = wdp->identity();
}

class TPINode : public NoOpMemoryModel {
  public:

    TPINode(int numThreads=0)
     : numThreads_(numThreads)
    {
      if (numThreads >= 1) {
        TPI_Init(numThreads);
      }
    }

    ~TPINode()
    {
      if (numThreads_ >= 1) {
        TPI_Finalize();
      }
    }

    template <class WDP>
    void parallel_for(int length, WDP & wd ) {
      TPI_Run_threads(tpi_execute<WDP>, &wd, 0 );
    }

    template <class WDP>
    void parallel_reduce(int length, WDP & wd ) {
      typedef typename WDP::ReductionType ReductionType;
      ReductionType result = 0;
      TPI_Run_threads_reduce(tpi_reduction_work<WDP>, &wd,
                             tpi_reduction_join<WDP>,
                             tpi_reduction_init<WDP>, sizeof(result), &result);
      wd.result = result;
    }

  private:
    int numThreads_;
};

#endif

