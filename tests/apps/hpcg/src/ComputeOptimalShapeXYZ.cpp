
#include <cmath>
#include <cstdlib>

#include <map>

#include "ComputeOptimalShapeXYZ.hpp"
#include "MixedBaseCounter.hpp"

static void
ComputePrimeFactors(int n, std::map<int, int> & factors) {
  int d, sq = int((sqrt(double(n)))+1L);
  div_t r;

  // remove 2 as a factor with shifts instead "/" and "%"
  for (; n > 1 && (n & 1) == 0; n >>= 1) {
    factors[2]++;
  }

  // keep removing subsequent odd numbers
  for (d = 3; d <= sq; d += 2) {
    while (1) {
      r = div(n, d);
      if (r.rem == 0) {
        factors[d]++;
        n = r.quot;
        continue;
      }
      break;
    }
  }
  if (n > 1 || factors.size() == 0)  // left with a prime or x==1
    factors[n]++;
}

static int
pow_i(int x, int p) {
  int v;

  if (0 == x || 1 == x) return x;

  if (p < 0)
    return 0;

  for (v = 1; p; p >>= 1) {
    if (1 & p)
      v *= x;
    x *= x;
  }

  return v;
}

void
ComputeOptimalShapeXYZ(int xyz, int & x, int & y, int & z) {
  std::map<int, int> factors;

  ComputePrimeFactors( xyz, factors ); // factors are sorted: ascending order

  std::map<int, int>::iterator iter = factors.begin();

  // there is at least one prime factor
  x = (iter++)->first; // cache the first factor, move to the next one

  y = iter != factors.end() ? (iter++)->first : y; // try to cache the second factor in "y"

  if (factors.size() == 1) { // only a single factor
    z = pow_i(x, factors[x] / 3);
    y = pow_i(x, factors[x] / 3 + ((factors[x] % 3) >= 2 ? 1 : 0));
    x = pow_i(x, factors[x] / 3 + ((factors[x] % 3) >= 1 ? 1 : 0));

  } else if (factors.size() == 2 && factors[x] == 1 && factors[y] == 1) { // two distinct prime factors
    z = 1;

  } else if (factors.size() == 2 && factors[x] + factors[y] == 3) { // three prime factors, one repeated
    z = factors[x] == 2 ? x : y; // test which factor is repeated

  } else if (factors.size() == 3 && factors[x] == 1 && factors[y] == 1 && iter->second == 1) { // three distinct and single prime factors
    z = iter->first;

  } else { // 3 or more prime factors so try all possible 3-subsets

    int i, distinct_factors[32+1], count_factors[32+1];

    i = 0;
    for (std::map<int, int>::iterator iter = factors.begin(); iter != factors.end(); ++iter, ++i) {
      distinct_factors[i] = iter->first;
      count_factors[i]    = iter->second;
    }

    // count total number of prime factors in "c_main" and distribute some factors into "c1"
    MixedBaseCounter c_main(count_factors, factors.size()), c1(count_factors, factors.size());

    // at the beginning, minimum area is the maximum area
    double area, min_area = 2.0 * xyz + 1.0;

    for (c1.next(); ! c1.is_zero(); c1.next()) {
      MixedBaseCounter c2(c_main, c1); // "c2" gets the factors remaining in "c_main" that "c1" doesn't have
      for (c2.next(); ! c2.is_zero(); c2.next()) {
        int tf1 = c1.product(distinct_factors);
        int tf2 = c2.product(distinct_factors);
        int tf3 = xyz / tf1/ tf2; // we derive the third dimension, we don't keep track of the factors it has

        area = tf1 * double(tf2) + tf2 * double(tf3) + tf1 * double(tf3);
        if (area < min_area) {
          min_area = area;
          x = tf1;
          y = tf2;
          z = tf3;
        }
      }
    }
  }
}
