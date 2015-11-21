/*
 * adios_read_ext_test.c
 *
 *  Created on: Jun 15, 2014
 *      Author: xczou
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <inttypes.h>
#include <assert.h>
#include "mpi.h"
#include "adios_read.h"
#include "adios_read_ext.h"

void test_adios_inq_var_transform(ADIOS_FILE *fp, ADIOS_VARINFO *v)
{
    int blockId = 0;
    //======For this example, it only has one block =========/
    ADIOS_VARTRANSFORM *tfv = adios_inq_var_transform(fp, v);
    ADIOS_TRANSFORM_METADATA *tmetas = tfv->transform_metadatas;
    ADIOS_TRANSFORM_METADATA tmeta = tmetas[blockId];
    assert(tmeta.length == 24);
    uint64_t *threeData = (uint64_t*)tmeta.content;
    printf("meta size: %" PRIu64 ", index size: %" PRIu64
			", data size: %" PRIu64 "\n"
			, threeData[0], threeData[1], threeData[2]);

    adios_free_var_transform(tfv);
}


/*
 * ADIOS_PG_INTERSECTIONS * adios_find_intersecting_pgs(
		const ADIOS_FILE *fp, int varid, const ADIOS_SELECTION *sel
		, const int from_step, const int nsteps);
 */

void test_adios_find_intersecting_pgs(ADIOS_FILE *fp, ADIOS_VARINFO *v ){

	int from_step =0;
	int nsteps =1, i = 0,j =0;

	uint64_t start[3] = {128,64,64};
	uint64_t count[3] = {1,1,1};
	ADIOS_SELECTION *emtpySel =  adios_selection_boundingbox(3, start, count);
	ADIOS_PG_INTERSECTIONS* intersectedPGs = adios_find_intersecting_pgs(
			fp, v->varid, emtpySel, from_step, nsteps);

	assert(intersectedPGs->npg == 0);
//  if there is no bounding box,  intersections is still non-NULL
//	assert(intersectedPGs->intersections == NULL);
	adios_selection_delete(emtpySel);
	printf("passed emtpy selelction \n");
	adios_free_pg_intersections(&intersectedPGs);

	//TODO: free ADIOS_PG_INTERSECTIONS


	start[0]= 12; start[1] = 10; start[2] =46;
	count[0]= 188; count[1]=20; count[2]=3;
	ADIOS_SELECTION *overlapSel =  adios_selection_boundingbox(3, start, count);
	ADIOS_PG_INTERSECTIONS* intersectedPGs2 = adios_find_intersecting_pgs(
				fp, v->varid, overlapSel, from_step, nsteps);
	int totalnpg = intersectedPGs2->npg;
	ADIOS_PG_INTERSECTION *  PGs = intersectedPGs2->intersections;


	printf("user's input selection box: ");
	if (overlapSel->type == ADIOS_SELECTION_BOUNDINGBOX ){
			const ADIOS_SELECTION_BOUNDINGBOX_STRUCT *olBB = &(overlapSel->u.bb);
			for(i=0; i < olBB->ndim; i++){
				printf("(%" PRIu64 ": %"PRIu64"), " , olBB->start[i], olBB->count[i] );
			}
			printf("\n");
	}

	printf("intersected # of PGs: %d \n", totalnpg);
	ADIOS_PG_INTERSECTION pg;
	for(j= 0; j < totalnpg; j ++){
		pg = PGs[j];
		printf("intersected PG[%d], timestep[%d], PG id in TS[%d]\n", pg.blockidx, pg.timestep, pg.blockidx_in_timestep);
		ADIOS_SELECTION * pgSelBox = pg.pg_bounds_sel;

		printf("overlapped PG bounding box: ");
		if (pgSelBox->type == ADIOS_SELECTION_BOUNDINGBOX ){
			const ADIOS_SELECTION_BOUNDINGBOX_STRUCT *pgBB = &(pgSelBox->u.bb);
				for(i=0; i < pgBB->ndim; i++){
					printf("(%" PRIu64 ": %"PRIu64"), " , pgBB->start[i], pgBB->count[i] );
				}
				printf("\n");
		}
		ADIOS_SELECTION * intersectedBox = pg.intersection_sel;
		printf("intersected bounding box: ");
		if (intersectedBox->type == ADIOS_SELECTION_BOUNDINGBOX ){
				const ADIOS_SELECTION_BOUNDINGBOX_STRUCT *interBB = &(intersectedBox->u.bb);
					for(i=0; i < interBB->ndim; i++){
						printf("(%" PRIu64 ": %"PRIu64"), " , interBB->start[i], interBB->count[i] );
					}
					printf("\n");
		}
	}

	adios_selection_delete(overlapSel);
	adios_free_pg_intersections(&intersectedPGs2);

}


/*
 * RUN with one processor is enough
 */
int main (int argc, char ** argv)
{

    char        filename [256];
    int         i, j, datasize, if_any;
    MPI_Comm    comm = MPI_COMM_WORLD;
    enum ADIOS_READ_METHOD method = ADIOS_READ_METHOD_BP;
    ADIOS_SELECTION * sel1, * sel2;
    ADIOS_VARCHUNK * chunk = 0;
    double * data = NULL;
    uint64_t start[2], count[2], npoints, * points;

    MPI_Init (&argc, &argv);

    if (argc < 2 ){
    	printf(" usage: %s {input bp file} \n", argv[0]);
    	return 1;
    }
    adios_read_init_method (method, comm, NULL);

    ADIOS_FILE * fp = adios_read_open_file (argv[1], method, comm);

    if ( fp == NULL){
    	printf(" can not open file %s \n", argv[1]);
    	return 1;
    }

    char varName[256] = "rdm";

    ADIOS_VARINFO* v = adios_inq_var(fp, varName);
    //====================start to test ==================//

    data_view_t  dv = PHYSICAL_DATA_VIEW;
    adios_read_set_data_view(fp, dv);
    test_adios_inq_var_transform(fp, v);



    dv = LOGICAL_DATA_VIEW;
    adios_read_set_data_view(fp, dv);
    test_adios_find_intersecting_pgs(fp,v);


    adios_read_close (fp);

    adios_read_finalize_method (ADIOS_READ_METHOD_BP);


    MPI_Finalize ();
    return 0;
}

