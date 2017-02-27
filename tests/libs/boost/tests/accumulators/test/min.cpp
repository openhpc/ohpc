//  (C) Copyright Eric Niebler 2005.
//  Use, modification and distribution are subject to the
//  Boost Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#define BOOST_TEST_MODULE accumulators
#define BOOST_TEST_DYN_LINK
#include <boost/test/unit_test.hpp>
#include <boost/accumulators/accumulators.hpp>
#include <boost/accumulators/statistics/stats.hpp>
#include <boost/accumulators/statistics/min.hpp>

using namespace boost;
using namespace unit_test;
using namespace accumulators;

///////////////////////////////////////////////////////////////////////////////
// test_stat
//
void test_stat()
{
    accumulator_set<int, stats<tag::min> > acc;

    acc(1);
    BOOST_CHECK_EQUAL(1, (min)(acc));

    acc(0);
    BOOST_CHECK_EQUAL(0, (min)(acc));

    acc(2);
    BOOST_CHECK_EQUAL(0, (min)(acc));
}

BOOST_AUTO_TEST_CASE( min_accumulator )
{
  test_stat();
}
