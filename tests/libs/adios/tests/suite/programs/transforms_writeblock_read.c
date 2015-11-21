/*
 * transforms_writeblock_read.c
 *
 *  Created on: Oct 15, 2014
 *      Author: David A. Boyuka II
 */

#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <assert.h>

#include <adios_read.h>

#define SHIFT_N(n) { argc -= (n); argv += (n); }
#define SHIFT SHIFT_N(1)

#define MPI_Assert(comm, check) { if (!(check)) { fprintf(stderr, "[rank %d/%d] Assertion '" #check "' failed\n", rank, size); MPI_Abort((comm), 1); abort(); } }

static const MPI_Comm COMM = MPI_COMM_WORLD;
static int rank, size;

static void test_file_mode_reads_on_var(ADIOS_FILE *fp, const char *bp_filename, const char *varname) {
	int i;

	ADIOS_VARINFO *varinfo = adios_inq_var(fp, varname);
	MPI_Assert(COMM, varinfo);

	if (varinfo->value != NULL) {
		//if (rank == 0) fprintf(stderr, "(skipping scalar variable '%s')\n", varname);
		adios_free_varinfo(varinfo);
		return;
	}

	fprintf(stderr, "[rank %d/%d] Starting file-mode writeblock reads on %s:/%s\n", rank, size, bp_filename, varname);

	adios_inq_var_blockinfo(fp, varinfo);
	MPI_Assert(COMM, varinfo->blockinfo);

	const enum ADIOS_DATATYPES datatype = varinfo->type;
	const int datatypesize = adios_get_type_size(datatype, NULL);

	int timestep, timestep_blockidx, blockidx = 0;
	for (timestep = 0; timestep < varinfo->nsteps; ++timestep) {
		for (timestep_blockidx = 0; timestep_blockidx < varinfo->nblocks[timestep]; ++timestep_blockidx, ++blockidx) {
			if (blockidx % size != rank) continue;

			const ADIOS_VARBLOCK *vb = &varinfo->blockinfo[blockidx];

			ADIOS_SELECTION *block_bb = adios_selection_boundingbox(varinfo->ndim, vb->start, vb->count);
			ADIOS_SELECTION *block_wb = adios_selection_writeblock(timestep_blockidx);
			ADIOS_SELECTION *block_abs_wb = adios_selection_writeblock(blockidx);
			block_abs_wb->u.block.is_absolute_index = 1;

			uint64_t blocksize = datatypesize;
			for (i = 0; i < varinfo->ndim; ++i)
				blocksize *= vb->count[i];

			void *buf_bb = malloc(blocksize);
			void *buf_wb = malloc(blocksize);
			void *buf_abs_wb = malloc(blocksize);
			memset(buf_bb,     0, blocksize);
			memset(buf_wb,     1, blocksize);
			memset(buf_abs_wb, 2, blocksize);
			MPI_Assert(COMM, buf_bb && buf_wb && buf_abs_wb);

			adios_schedule_read(fp, block_bb,     varname, timestep, 1, buf_bb    );
			adios_schedule_read(fp, block_wb,     varname, timestep, 1, buf_wb    );
			adios_schedule_read(fp, block_abs_wb, varname, timestep, 1, buf_abs_wb);
			adios_perform_reads(fp, 1);

			fprintf(stderr, "[rank %d/%d] Checking file-mode blockidx %d BB vs. WB...\n", rank, size, blockidx);
			MPI_Assert(COMM, memcmp(buf_bb, buf_wb, blocksize) == 0);
			fprintf(stderr, "[rank %d/%d] Checking file-mode blockidx %d BB vs. abs-WB...\n", rank, size, blockidx);
			MPI_Assert(COMM, memcmp(buf_bb, buf_abs_wb, blocksize) == 0);

			free(buf_bb); free(buf_wb); free(buf_abs_wb);
			adios_selection_delete(block_bb);
			adios_selection_delete(block_wb);
			adios_selection_delete(block_abs_wb);
		}
	}

	adios_free_varinfo(varinfo);

	fprintf(stderr, "[rank %d/%d] Finished file-mode writeblock reads on %s:/%s\n", rank, size, bp_filename, varname);
}

static void test_file_mode_reads(const char *bp_filename) {
	int i;
	ADIOS_FILE *fp = adios_read_open_file(bp_filename, ADIOS_READ_METHOD_BP, COMM);
	MPI_Assert(COMM, fp);

	fprintf(stderr, "[rank %d/%d] Starting file-mode writeblock reads on %s\n", rank, size, bp_filename);

	for (i = 0; i < fp->nvars; ++i) {
		const char *varname = fp->var_namelist[i];
		test_file_mode_reads_on_var(fp, bp_filename, varname);

		MPI_Barrier(COMM);
	}

	adios_read_close(fp);

	fprintf(stderr, "[rank %d/%d] Finished file-mode writeblock reads on %s\n", rank, size, bp_filename);
}

int main(int argc, char **argv) {
	MPI_Init(&argc, &argv);

	const char *cmd = *argv; SHIFT;

	if (argc != 1) {
		if (rank == 0) fprintf(stderr, "Usage: %s <BP filename>\n", cmd);
		MPI_Abort(COMM, 1);
	}

	const char *bp_filename = *argv; SHIFT;

	MPI_Comm_rank(COMM, &rank);
	MPI_Comm_size(COMM, &size);
	adios_read_init_method(ADIOS_READ_METHOD_BP, COMM, "");

	if (rank == 0) fprintf(stderr, "Starting file-mode writeblock tests on %s (%d ranks)...\n", bp_filename, size);
	test_file_mode_reads(bp_filename);

	adios_read_finalize_method(ADIOS_READ_METHOD_BP);
	MPI_Finalize();
}
