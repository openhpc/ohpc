#ifndef _param_utils_hpp_
#define _param_utils_hpp_

//@HEADER
// ************************************************************************
//
//               miniFE: simple finite-element assembly and linear-solve
//                 Copyright (2006) Sandia Corporation
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

#include <string>
#include <sstream>

//Parameter-parsing Utilities:
//
//The functions declared below are intended to assist with parsing
//input-parameters which may be command-line arguments and/or lines in a
//text file.
//
// Scenario: You want your program to accept parameters that are specified
// as command-line arguments and/or as lines in a text file (such
// as a YAML output file). i.e., your program can be run like this:
// % program.exe foo=3.14159 bar: 42
// or
// % program.exe input_file=params.txt
// or
// % program.exe foo=3.14159 input_file = params.txt
//
//Example:
// Here is example code to obtain parameters using the 3 functions
// 'read_args_into_string', 'read_file_into_string' and 'parse_parameter':
//
//   std::string arg_string;
//
//   //put command-line-arguments into 'arg_string':
//   read_args_into_string(argc, argv, arg_string);
//
//   //do the command-line-arguments specify an 'input_file'?
//   std::string filename =
//      parse_parameter<std::string>(arg_string,"input_file","none-specified");
//
//   if (filename != "none-specified") {
//     std::string tmp;
//     read_file_into_string(filename, tmp);
//     arg_string += tmp;
//   }
//
//  //now parse the parameters:
//  float foo = parse_parameter<float>(arg_string, "foo", -9.9);
//  int bar   = parse_parameter<int>(arg_string, "bar", -1);
//
//See the comments below for parse_parameter, for formatting requirements of
//named parameter-value pairs.
//

namespace Mantevo {

/**
 * Concatenate command-line arguments into a single string.
 *
 * Note: this function is purely serial. If argc and argv have different
 * values on different MPI processes, then you need to resolve that by
 * broadcasting arg_string's contents.
 */
void read_args_into_string(int argc, char** argv, std::string& arg_string);

/**
 * Read the contents of a text-file into a single string.
 *
 * Note: this function is purely serial. If you want file_contents on multiple
 * MPI processes, you need to broadcast it (or call this function on each
 * MPI process...).
 */
void read_file_into_string(const std::string& filename,
                           std::string& file_contents);

/**
 * Parse a named parameter value from input 'arg_string'.
 *
 * Search 'arg_string' for an occurrence of param_name and attempt to parse
 * a value into the return-type. If param_name is not found, then default_value
 * is returned.
 *
 * Example:
 * arg_string = "foo = 3.14159";
 * float foo = parse_parameter<float>(arg_string, "foo", -999.9);
 * //foo should now contain the value 3.14159; if 'foo' was not found in
 * //arg_string, then -999.9 would have been returned.
 *
 * Other legal name-value separators are ':' and ' '. Extra spaces are also ok,
 * e.g. "foo : 3.114159".
 *
 * Note that if a YAML file is read into a string, that would be a valid input
 * string for this function.
 */
template<typename T>
T parse_parameter(const std::string& arg_string,
                const std::string& param_name,
                const T& default_value)
{
  std::string::size_type pos = arg_string.find(param_name);
  if (pos == std::string::npos) {
    //if param_name is not found in arg_string, return default_value:
    return default_value;
  }

  pos += param_name.size();

  if (arg_string.size() <= pos) return default_value;

  //skip past ' ', '=' or ':':
  while(pos < arg_string.size() &&
        (arg_string[pos] == ' ' ||
         arg_string[pos] == '=' ||
         arg_string[pos] == ':'))
  {
    ++pos;
  }

  if (arg_string[pos] == '=' || arg_string[pos] == ':') ++pos;

  std::string str = arg_string.substr(pos);

  std::istringstream isstr(str);

  T return_val = default_value;

  //parse value into return_val:
  isstr >> return_val;

  //if parse failed, return default_value:
  if (!isstr) return default_value;

  return return_val;
}

}//namespace Mantevo

#endif

