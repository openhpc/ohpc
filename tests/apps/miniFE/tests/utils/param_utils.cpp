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

#include <param_utils.hpp>

#include <sstream>
#include <fstream>

namespace Mantevo {

//-------------------------------------------------------------
void read_args_into_string(int argc, char** argv, std::string& arg_string)
{
  arg_string = argv[0];
  for(int i=1; i<argc; ++i) {
    arg_string += " " + std::string(argv[i]);
  }
}

//-------------------------------------------------------------
void read_file_into_string(const std::string& filename,
                           std::string& file_contents)
{
  file_contents.clear();
  std::ifstream ifs(filename.c_str());
  char line[256];
  while(!ifs.eof()) {
    ifs.getline(line, 256);
    file_contents += " " + std::string(line);
  }
}

}//namespace Mantevo

