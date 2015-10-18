
//@HEADER
// ***************************************************
//
// HPCG: High Performance Conjugate Gradient Benchmark
//
// Contact:
// Michael A. Heroux ( maherou@sandia.gov)
// Jack Dongarra     (dongarra@eecs.utk.edu)
// Piotr Luszczek    (luszczek@eecs.utk.edu)
//
// ***************************************************
//@HEADER

#include <cstdlib>
#include <ctime>
#include <iostream>
#include <fstream>
#include <sstream>
#include "YAML_Doc.hpp"
using namespace std;

/*!
  Sets the application name and version which will become part of the YAML doc.

  @param[in] miniApp_Name application name
  @param[in] miniApp_Version application name
  @param[in] destination_Directory destination directory for the YAML document
  @param[in] destination_FileName file name for the YAML document
*/
YAML_Doc::YAML_Doc(const std::string & miniApp_Name, const std::string & miniApp_Version, const std::string & destination_Directory, const std::string & destination_FileName) {
  miniAppName = miniApp_Name;
  miniAppVersion = miniApp_Version;
  destinationDirectory = destination_Directory;
  destinationFileName = destination_FileName;
}

//inherits the destructor from YAML_Element
YAML_Doc::~YAML_Doc(void) {
}

/*!
  Generates YAML from the elements of the document and saves it to a file.

  @return returns the complete YAML document as a string
*/
string YAML_Doc::generateYAML() {
  string yaml;
  yaml =  yaml + miniAppName + "\n";
  yaml =  yaml +  miniAppVersion + "\n";
  for (size_t i=0; i<children.size(); i++) {
    yaml = yaml + children[i]->printYAML("");
  }

  time_t rawtime;
  tm * ptm;
  time ( &rawtime );
  ptm = localtime(&rawtime);
  char sdate[25];
  //use tm_mon+1 because tm_mon is 0 .. 11 instead of 1 .. 12
  sprintf (sdate,"%04d.%02d.%02d.%02d.%02d.%02d",ptm->tm_year + 1900, ptm->tm_mon+1,
      ptm->tm_mday, ptm->tm_hour, ptm->tm_min,ptm->tm_sec);

  string filename;
  if (destinationFileName=="")
    filename = miniAppName + "-" + miniAppVersion + "_";
  else
    filename = destinationFileName;
  filename = filename + string(sdate) + ".yaml";
  if (destinationDirectory!="" && destinationDirectory!=".") {
    string mkdir_cmd = "mkdir " + destinationDirectory;
    system(mkdir_cmd.c_str());
    filename = destinationDirectory + "/" + destinationFileName;
  } else
    filename = "./" + filename;

  ofstream myfile;
  myfile.open(filename.c_str());
  myfile << yaml;
  myfile.close();
  return yaml;
}
