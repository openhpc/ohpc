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
#ifdef _OPENMP
#include <omp.h>
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
#ifdef _OPENMP
  sprintf (sdate,"%04d:%02d:%02d-%02d:%02d:%02d:OMP%04d",ptm->tm_year + 1900, ptm->tm_mon+1,
    ptm->tm_mday, ptm->tm_hour, ptm->tm_min,ptm->tm_sec, omp_get_max_threads());
#else
  sprintf (sdate,"%04d:%02d:%02d-%02d:%02d:%02d",ptm->tm_year + 1900, ptm->tm_mon+1,
    ptm->tm_mday, ptm->tm_hour, ptm->tm_min,ptm->tm_sec);
#endif

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


