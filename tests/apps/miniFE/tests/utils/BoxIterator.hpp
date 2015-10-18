#ifndef _BoxTraverser_hpp_
#define _BoxTraverser_hpp_

//@HEADER
// ************************************************************************
//
// MiniFE: Simple Finite Element Assembly and Solve
// Copyright (2006-2013) Sandia Corporation
//
// Under terms of Contract DE-AC04-94AL85000, there is a non-exclusive
// license for use of this work by or on behalf of the U.S. Government.
//
// This library is free software; you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as
// published by the Free Software Foundation; either version 2.1 of the
// License, or (at your option) any later version.
//
// This library is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this library; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
// USA
//
// ************************************************************************
//@HEADER

namespace miniFE {

/** Class for traversing a 3-dimensional 'box' of indices.

  //One way to traverse a 'box[3][2]' is to use a triply-nested for-loop:
  for(int z=box[2][0]; z<box[2][1]; ++z) {
    for(int y=box[1][0]; y<box[1][1]; ++y) {
      for(int x=box[0][0]; x<box[0][1]; ++x) {
        ...
      }
    }
  }

  //Another way is to use this BoxIterator class, like so:
  //BoxIterator iter = BoxIterator::begin(box);
  //BoxIterator end = BoxIterator::end(box);
  for(; iter != end; ++iter) {
    int x = iter.x;
    int y = iter.y;
    int z = iter.z;
    ...
  }
*/
class BoxIterator {
public:
  ~BoxIterator(){}

  static BoxIterator begin(const Box& box)
  {
    return BoxIterator(box);
  }

  static BoxIterator end(const Box& box)
  {
    return BoxIterator(box, true/*at_end==true*/);
  }

  BoxIterator& operator=(const BoxIterator& src)
  {
    box_[0][0] = src.box_[0][0]; box_[0][1] = src.box_[0][1];
    box_[1][0] = src.box_[1][0]; box_[1][1] = src.box_[1][1];
    box_[2][0] = src.box_[2][0]; box_[2][1] = src.box_[2][1];
    x = src.x;
    y = src.y;
    z = src.z;
    return *this;
  }

  BoxIterator& operator++()
  {
    ++x;
    if (x >= box_[0][1]) {
      x = box_[0][0];
      ++y;
      if (y >= box_[1][1]) {
        y = box_[1][0];
        ++z;
        if (z >= box_[2][1]) {
          z = box_[2][1];
          y = box_[1][1];
          x = box_[0][1];
        }
      }
    }
    return *this;
  }

  BoxIterator operator++(int)
  {
    BoxIterator temp = *this;
    ++(*this);
    return temp;
  }

  bool operator==(const BoxIterator& rhs) const
  {
    return x == rhs.x && y == rhs.y && z == rhs.z;
  }

  bool operator!=(const BoxIterator& rhs) const
  {
    return !(this->operator==(rhs));
  }

  int x;
  int y;
  int z;

private:
  BoxIterator(const Box& box, bool at_end = false)
   : x(box[0][0]),
     y(box[1][0]),
     z(box[2][0]),
     box_()
  {
    box_[0][0] = box[0][0]; box_[0][1] = box[0][1];
    box_[1][0] = box[1][0]; box_[1][1] = box[1][1];
    box_[2][0] = box[2][0]; box_[2][1] = box[2][1];
    if (at_end) {
      x = box[0][1];
      y = box[1][1];
      z = box[2][1];
    }
  }

  Box box_;
};//class BoxTraverser

}//namespace miniFE

#endif

