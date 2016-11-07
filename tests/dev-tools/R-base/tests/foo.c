#include <R.h>
#include <Rdefines.h>
#include <stdio.h>

//SEXP hola() {
//  printf("Hello World!\n");
//  return(R_NilValue);
//}


SEXP hola() {
  SEXP result;
  printf("Hello World!\n");
  PROTECT(result=NEW_INTEGER(1));
  INTEGER(result)[0] = 42;

  UNPROTECT(1);
  
  return(result);
}

