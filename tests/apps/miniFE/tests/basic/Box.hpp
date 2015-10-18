#ifndef _Box_hpp_
#define _Box_hpp_

/**
  * a 'Box' is 3 pairs of ints, where each pair specifies a lower
  * and upper bound for one of the 3 spatial dimensions.
  *
  * This struct stores the 3 pairs as a simple array of 6 ints,
  * but defines the bracket operator so that it can be referenced
  * using 2-dimensional array notation like this:
  * int xmin = box[0][0]; int xmax = box[0][1];
  * int ymin = box[1][0]; int ymax = box[1][1];
  * int zmin = box[2][0]; int zmax = box[2][1];
 */
struct Box {
  int ranges[6];
  int* operator[](int xyz) { return &ranges[xyz*2]; }
  const int* operator[](int xyz) const { return &ranges[xyz*2]; }
};

#endif

