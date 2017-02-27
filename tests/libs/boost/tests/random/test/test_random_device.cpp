/* boost random_test.cpp various tests
 *
 * Copyright (c) 2010 Steven Watanabe
 * Distributed under the Boost Software License, Version 1.0. (See
 * accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENCE_1_0.txt)
 *
 * $Id$
 */

#define BOOST_TEST_MODULE random
#define BOOST_TEST_DYN_LINK
#include <boost/test/unit_test.hpp>
#include <boost/random/random_device.hpp>

BOOST_AUTO_TEST_CASE( random_device_test )
{
    boost::random_device rng;
    double entropy = rng.entropy();
    BOOST_CHECK_GE(entropy, 0);
    for(int i = 0; i < 100; ++i) {
        boost::random_device::result_type val = rng();
        BOOST_CHECK_GE(val, (rng.min)());
        BOOST_CHECK_LE(val, (rng.max)());
    }

    boost::uint32_t a[10];
    rng.generate(a, a + 10);

    return;
  }
