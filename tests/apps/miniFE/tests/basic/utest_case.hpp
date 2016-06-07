#ifndef _utest_case_hpp_
#define _utest_case_hpp_

#include <vector>

class utest_case;

std::vector<utest_case*>& get_utest_cases()
{
  static std::vector<utest_case*> utest_cases;
  return utest_cases;
}

//When a class that inherits the utest_case class is constructed,
//it gets added to the vector of utest_cases returned by
//the above 'get_utest_cases' function.
class utest_case {
public:
  utest_case(){ get_utest_cases().push_back(this); }
  ~utest_case(){}
  virtual const char* name() = 0;
  virtual bool run() = 0;
};

//The following macro declares and instantiates a class that
//inherits the above utest_case interfaces.
//
//use the macro like this:
//   UTEST_CASE(mytest)
//   {
//      ... test code here ...
//   }
//
//See example usages in utest_cases.hpp
//
#define UTEST_CASE(TESTNAME) \
  class TESTNAME##_utest : public utest_case { \
  public: \
    TESTNAME##_utest(){} \
    const char* name() {return #TESTNAME;} \
    bool run(); \
  }; \
  \
  TESTNAME##_utest instance_##TESTNAME##_utest; \
  \
  bool TESTNAME##_utest::run()

#define TEST_EQUAL(A,B) \
  if ((A) != (B)) return false;

#define TEST_EQUAL_TOL(A,B,tol) \
  if (std::abs((A) - (B)) > tol) return false;

#endif

