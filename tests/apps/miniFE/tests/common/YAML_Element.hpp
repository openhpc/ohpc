//@HEADER
// ************************************************************************
// 
//               Mantevo: A collection of mini-applications for HPC
//                 Copyright (2008) Sandia Corporation
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
// Questions? Contact Michael A. Heroux (maherou@sandia.gov) 
// 
// ************************************************************************
//@HEADER

// Changelog
//
// Version 0.1
// - Initial version.
//
/////////////////////////////////////////////////////////////////////////

#ifndef YAML_ELEMENT_H
#define YAML_ELEMENT_H
#include <string>
#include <vector>
//! The Mantevo YAML_Element class for registering key-value pairs of performance data

/*!
  Mantevo mini-applications generate a collection of performance data for each run of the executable.  YAML_Element, and
  the related YAML_Doc class, provide a uniform facility for gathering and reporting this data using the YAML text format.
*/
class YAML_Element {
  public:

  //! Default constructor.
  YAML_Element (){key="";value="";}
  //! Construct with known key-value pair
  YAML_Element (const std::string& key_arg, const std::string& value_arg);
  //! Destructor
  ~YAML_Element ();
  //! Key accessor method
  std::string getKey(){return key;}
  //! Add a child element to an element list associated with this element, value of type double
  YAML_Element* add(const std::string& key_arg, double value_arg);
  //! Add a child element to an element list associated with this element, value of type int
  YAML_Element* add(const std::string& key_arg, int value_arg);
#ifndef MINIFE_NO_LONG_LONG
  //! Add a child element to an element list associated with this element, value of type long long
  YAML_Element* add(const std::string& key_arg, long long value_arg);
#endif
  //! Add a child element to an element list associated with this element, value of type size_t
  YAML_Element* add(const std::string& key_arg, size_t value_arg);
  //! Add a child element to an element list associated with this element, value of type string
  YAML_Element* add(const std::string& key_arg, const std::string& value_arg);
  //! get the element in the list with the given key
  YAML_Element* get(const std::string& key_arg);
  std::string printYAML(std::string space);
  
protected:
  std::string key;
  std::string value;
  std::vector<YAML_Element*> children;

private:
  std::string convert_double_to_string(double value_arg);
  std::string convert_int_to_string(int value_arg);
#ifndef MINIFE_NO_LONG_LONG
  std::string convert_long_long_to_string(long long value_arg);
#endif
  std::string convert_size_t_to_string(size_t value_arg);
};
#endif /* YAML_ELEMENT_H */
