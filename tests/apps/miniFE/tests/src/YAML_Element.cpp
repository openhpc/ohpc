
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

#include <vector>
#include <iostream>
#include <fstream>
#include <sstream>
#include "YAML_Element.hpp"
using namespace std;
YAML_Element::YAML_Element(const std::string& key_arg, const std::string& value_arg){
  key = key_arg;
  value = value_arg;
}

YAML_Element::~YAML_Element(){
  for (size_t i=0; i<children.size(); i++) {
    delete children[i];
  }
  children.clear();
}

/*
* Add an element to the vector
* QUESTION: if an element is not added because the key already exists,
* will this lead to memory leakage?
*/
YAML_Element* YAML_Element::add(const std::string& key_arg, double value_arg) {
  this->value = "";
  string converted_value = convert_double_to_string(value_arg);
  YAML_Element* element = new YAML_Element(key_arg,converted_value);
  children.push_back(element);
  return element;
}

YAML_Element* YAML_Element::add(const std::string& key_arg, int value_arg) {
  this->value = "";
  string converted_value = convert_int_to_string(value_arg);
  YAML_Element* element = new YAML_Element(key_arg,converted_value);
  children.push_back(element);
  return element;
}

#ifndef MINIFE_NO_LONG_LONG

YAML_Element* YAML_Element::add(const std::string& key_arg, long long value_arg) {
  this->value = "";
  string converted_value = convert_long_long_to_string(value_arg);
  YAML_Element* element = new YAML_Element(key_arg,converted_value);
  children.push_back(element);
  return element;
}

#endif

YAML_Element* YAML_Element::add(const std::string& key_arg, size_t value_arg) {
  this->value = "";
  string converted_value = convert_size_t_to_string(value_arg);
  YAML_Element* element = new YAML_Element(key_arg,converted_value);
  children.push_back(element);
  return element;
}

YAML_Element* YAML_Element::add(const std::string& key_arg, const std::string& value_arg) {
  this->value = "";
  YAML_Element* element = new YAML_Element(key_arg, value_arg);
  children.push_back(element);
  return element;
}

/*
* returns pointer to the YAML_Element for the given key.
* I, cam, believe an exception should be thrown if there is no
* element in the vector for the specified key
*/
YAML_Element* YAML_Element::get(const std::string& key_arg) {
  for (size_t i=0; i<children.size(); i++) {
    if(children[i]->getKey() == key_arg){
      return children[i];
    }
  }
  return 0;
}

/*
* prints a line of a YAML document.  Correct YAML depends on
* correct spacing; the parameter space should be the proper
* amount of space for the parent element
*/
string YAML_Element::printYAML(std::string space){
  string yaml_line = space + key + ": " + value + "\n";
  for(int i=0; i<2; i++) space = space + " ";
  for(size_t i=0; i<children.size(); i++){
    yaml_line = yaml_line + children[i]->printYAML(space);
  }
  return yaml_line;
}

string YAML_Element::convert_double_to_string(double value_arg){
  stringstream strm;
  strm << value_arg;
  return strm.str();
}
string YAML_Element::convert_int_to_string(int value_arg){
  stringstream strm;
  strm << value_arg;
  return strm.str();
}

#ifndef MINIFE_NO_LONG_LONG

string YAML_Element::convert_long_long_to_string(long long value_arg){
  stringstream strm;
  strm << value_arg;
  return strm.str();
}

#endif

string YAML_Element::convert_size_t_to_string(size_t value_arg){
  stringstream strm;
  strm << value_arg;
  return strm.str();
}
