

class MixedBaseCounter {
  private:
    int length; //!< number of prime factor counts (cannot exceed 32 for a 32-bit integer)
    int max_counts[32+1]; //!< maximum value for prime factor counts
    int cur_counts[32+1]; //!< current prime factor counts

  public:
    MixedBaseCounter(int *counts, int length);
    MixedBaseCounter(MixedBaseCounter & left, MixedBaseCounter & right); 
    void next();
    int is_zero();
    int product(int * multipliers);
};
