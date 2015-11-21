#include <cassert>
#include <map>
#include <string>

int main()
{
  std::map<std::string,int> amap;

  amap["fafafooey"] = -1;
  amap["fafafooey"] += 43;

  assert(amap["fafafooey"] == 42);
  return(0);
}

