
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

