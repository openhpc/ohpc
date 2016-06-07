#ifndef SERIALCOMPUTENODE_HPP_
#define SERIALCOMPUTENODE_HPP_

#include <NoOpMemoryModel.hpp>

class SerialComputeNode : public NoOpMemoryModel {
  public:
    template <class WDP>
    void parallel_for(unsigned int length, WDP wd) {
      for(int i=0; i<length; ++i) {
        wd(i);
      }
    }

    template <class WDP>
    void parallel_reduce(unsigned int length, WDP &wd) {
      wd.result = wd.identity();
      for(int i=0; i<length; ++i) {
        wd.result = wd.reduce(wd.result, wd.generate(i));
      }
    }

};

#endif
