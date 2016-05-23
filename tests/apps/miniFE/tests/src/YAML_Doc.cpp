
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

#include <ctime>
#include <cstdlib>
#include <ctime>
#include <vector>
#include <iostream>
#include <fstream>
#include <sstream>
#ifdef REDSTORM
#include <time.h>
#include <sys/stat.h>
#include <sys/types.h>
#endif
#include "YAML_Doc.hpp"
using namespace std;

//set the microapp_name and version which will become part of the YAML doc.
YAML_Doc::YAML_Doc(const std::string& miniApp_Name, const std::string& miniApp_Version, const std::string& destination_Directory, const std::string& destination_FileName){
  miniAppName = miniApp_Name;
  miniAppVersion = miniApp_Version;
  destinationDirectory = destination_Directory;
  destinationFileName = destination_FileName;
}

//inherits the destructor from YAML_Element
YAML_Doc::~YAML_Doc(void){
}

/*
* generates YAML from the elements of the document and saves it
* to a file
*/
string YAML_Doc::generateYAML(){
  string yaml;
  yaml =  yaml + "Mini-Application Name: " + miniAppName + "\n";
  yaml =  yaml + "Mini-Application Version: " + miniAppVersion + "\n";
  for(size_t i=0; i<children.size(); i++){
    yaml = yaml + children[i]->printYAML("");
  }
  
  time_t rawtime;
  tm * ptm;
  time ( &rawtime );
  ptm = localtime(&rawtime);
  char sdate[25];
  //use tm_mon+1 because tm_mon is 0 .. 11 instead of 1 .. 12
  sprintf (sdate,"%04d:%02d:%02d-%02d:%02d:%02d",ptm->tm_year + 1900, ptm->tm_mon+1,
    ptm->tm_mday, ptm->tm_hour, ptm->tm_min,ptm->tm_sec);

  string filename;
  if (destinationFileName=="") 
    filename = miniAppName + "-" + miniAppVersion + "_";
  else 
    filename = destinationFileName;
  filename = filename + string(sdate) + ".yaml";
  if (destinationDirectory!="" && destinationDirectory!=".") {
    string mkdir_cmd = "mkdir " + destinationDirectory;
#ifdef REDSTORM
    mkdir(destinationDirectory.c_str(),0755);
#else
    system(mkdir_cmd.c_str());
#endif
    filename = destinationDirectory + "/" + destinationFileName;
  }
  else 
    filename = "./" + filename;

  ofstream myfile;
  myfile.open(filename.c_str());
  myfile << yaml;
  myfile.close();
  return yaml;
}


