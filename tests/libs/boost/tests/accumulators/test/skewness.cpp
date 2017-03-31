//  (C) Copyright 2006 Eric Niebler, Olivier Gygi.
//  Use, modification and distribution are subject to the
//  Boost Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

// Test case for skewness.hpp

#define BOOST_TEST_MODULE accumulators
#define BOOST_TEST_DYN_LINK
#include <boost/random.hpp>
#include <boost/test/unit_test.hpp>
#include <boost/test/floating_point_comparison.hpp>
#include <boost/accumulators/numeric/functional/vector.hpp>
#include <boost/accumulators/numeric/functional/complex.hpp>
#include <boost/accumulators/numeric/functional/valarray.hpp>
#include <boost/accumulators/accumulators.hpp>
#include <boost/accumulators/statistics/stats.hpp>
#include <boost/accumulators/statistics/skewness.hpp>

using namespace boost;
using namespace unit_test;
using namespace boost::accumulators;

///////////////////////////////////////////////////////////////////////////////
// test_stat
//
void test_stat()
{
    // tolerance in %
    // double epsilon = 1;

    accumulator_set<double, stats<tag::skewness > > acc1;
    accumulator_set<int, stats<tag::skewness > > acc2;

    // two random number generators
    boost::lagged_fibonacci607 rng;
    boost::normal_distribution<> mean_sigma(0,1);
    boost::variate_generator<boost::lagged_fibonacci607&, boost::normal_distribution<> > normal(rng, mean_sigma);

    for (std::size_t i=0; i<100000; ++i)
    {
        acc1(normal());
    }

    // This check fails because epsilon is relative and not absolute
    // BOOST_CHECK_CLOSE( skewness(acc1), 0., epsilon );

    acc2(2);
    acc2(7);
    acc2(4);
    acc2(9);
    acc2(3);

    BOOST_CHECK_EQUAL( mean(acc2), 5 );
    BOOST_CHECK_EQUAL( accumulators::moment<2>(acc2), 159./5. );
    BOOST_CHECK_EQUAL( accumulators::moment<3>(acc2), 1171./5. );
    BOOST_CHECK_CLOSE( skewness(acc2), 0.406040288214, 1e-6 );
}

BOOST_AUTO_TEST_CASE( accumulate_skewness )
{
  test_stat();
}

