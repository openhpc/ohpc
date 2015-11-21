
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

#include <cstdio>

#include "ReadHpcgDat.hpp"

static int
SkipUntilEol(FILE *stream) {
  int chOrEof;
  bool finished;

  do {
    chOrEof = fgetc( stream );
    finished = (chOrEof == EOF) || (chOrEof == '\n') || (chOrEof == '\r');
  } while (! finished);

  if ('\r' == chOrEof) { // on Windows, \r might be followed by \n
    int chOrEofExtra = fgetc( stream );

    if ('\n' == chOrEofExtra || EOF == chOrEofExtra)
      chOrEof = chOrEofExtra;
    else
      ungetc(chOrEofExtra, stream);
  }

  return chOrEof;
}

int
ReadHpcgDat(int *localDimensions, int *secondsPerRun) {
  FILE * hpcgStream = fopen("hpcg.dat", "r");

  if (! hpcgStream)
    return -1;

  SkipUntilEol(hpcgStream); // skip the first line

  SkipUntilEol(hpcgStream); // skip the second line

  for (int i = 0; i < 3; ++i)
    if (fscanf(hpcgStream, "%d", localDimensions+i) != 1 || localDimensions[i] < 10)
      localDimensions[i] = 10;

  SkipUntilEol( hpcgStream ); // skip the rest of the second line

  if (fscanf(hpcgStream, "%d", secondsPerRun) != 1 || secondsPerRun[0] < 1)
    secondsPerRun[0] = 30 * 60; // 30 minutes

  fclose(hpcgStream);

  return 0;
}
