#include <metis.h>
#include <assert.h>
#include <stdlib.h>
#include <stdio.h>

int main ( int argc, char *argv[] )
{
  const int NUM_ELEMENTS = 7434;

  int i;
  idx_t numParts   = 4;
  idx_t numCommon  = 1;
  idx_t numElements;
  idx_t numNodes;
  FILE *fp_mesh;
  FILE *fp_out;

  idx_t objval;
  idx_t eptr [NUM_ELEMENTS+1];
  idx_t eind [3*NUM_ELEMENTS];
  idx_t epart[NUM_ELEMENTS];
  idx_t npart[3*NUM_ELEMENTS];

  //int istart, iend;
  int count;
  
  fp_mesh = fopen("metis.mesh","r");
  assert(fp_mesh != NULL);

  assert( fscanf(fp_mesh,"%i",&numElements) != EOF);
  assert( numElements == NUM_ELEMENTS);
  numNodes = numElements*3;

  count = 0;

  eptr[0]=0;
  for(i=0;i<numElements;i++)
    {
      assert( fscanf(fp_mesh,"%i %i %i",&eind[count],&eind[count+1],&eind[count+2]) != EOF);
      count += 3;
      eptr[i+1]=count;
    }

  fclose(fp_mesh);

  printf("numelements = %i\n",numElements);
  printf("num nodes   = %i\n",numNodes);

  assert( METIS_PartMeshDual(&numElements,&numNodes,eptr,eind,NULL,NULL,&numCommon,&numParts,
                             NULL,NULL,&objval,epart,npart) == METIS_OK);

  /* Save output of resulting element coloring */

  fp_out = fopen("C_test.out","w");
  assert(fp_out != NULL);
  
  for(i=0;i<numElements;i++)
    fprintf(fp_out,"%i\n",epart[i]);

  fclose(fp_out);

  return 0;
}  

