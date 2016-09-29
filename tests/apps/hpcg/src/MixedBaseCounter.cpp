
#include <map>

#include "MixedBaseCounter.hpp"

MixedBaseCounter::MixedBaseCounter(int *counts, int length) {
  this->length = length;

  int i;

  for (i = 0; i < 32; ++i) {
    this->max_counts[i] = counts[i];
    this->cur_counts[i] = 0;
  }
  // terminate with 0's
  this->max_counts[i]      = this->cur_counts[i]      = 0;
  this->max_counts[length] = this->cur_counts[length] = 0;
}

MixedBaseCounter::MixedBaseCounter(MixedBaseCounter & left, MixedBaseCounter & right) {
  this->length = left.length;
  for (int i = 0; i < left.length; ++i) {
    this->max_counts[i] = left.max_counts[i] - right.cur_counts[i];
    this->cur_counts[i] = 0;
  }
}

void
MixedBaseCounter::next() {
  for (int i = 0; i < this->length; ++i) {
    this->cur_counts[i]++;
    if (this->cur_counts[i] > this->max_counts[i]) {
      this->cur_counts[i] = 0;
      continue;
    }
    break;
  }
}

int
MixedBaseCounter::is_zero() {
  for (int i = 0; i < this->length; ++i)
    if (this->cur_counts[i])
      return 0;
  return 1;
}

int
MixedBaseCounter::product(int * multipliers) {
  int k=0, x=1;

  for (int i = 0; i < this->length; ++i)
    for (int j = 0; j < this->cur_counts[i]; ++j) {
      k = 1;
      x *= multipliers[i];
    }

  return x * k;
}
