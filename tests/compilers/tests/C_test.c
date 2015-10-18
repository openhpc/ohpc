#include <stdio.h>
#include <assert.h>

int main()
{
  int a = -1;
  a += 43;
  
  assert(a == 42);
  return(0);
}

