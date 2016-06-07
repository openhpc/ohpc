/* 
 * ADIOS is freely available under the terms of the BSD license described
 * in the COPYING file in the top level directory of this source distribution.
 *
 * Copyright (c) 2008 - 2009.  UT-BATTELLE, LLC. All rights reserved.
 */

/* ADIOS C test: 
 *  Write a global array over time, with multiple blocks per process
 *    Similar to examples/C/global-array-time/adios_global_time_noxml.c
 *  Then open for reading and check if the blockinfo for each block is correct. 
 *  Do the reading twice, once with opening as file (all steps at once) and 
 *     once as streaming (step-by-step)
 *  Additionally, read a scalar's all instances in the file using writeblock 
 *     selection.
 *
 * How to run: mpirun -np <N> blocks
 * Output: blocks.bp
 * Exit code: the number of errors found (0=OK)
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include "adios.h"
#include "adios_read.h"
#include "adios_error.h"

const static char fname[] = "blocks.bp";
const static MPI_Comm  comm = MPI_COMM_WORLD;
static int rank, size;
static int nerrors = 0;

int write_blocks ();
void print_written_info();
int read_all ();
int read_stepbystep ();
int read_scalar ();
int read_scalar_stepbystep ();

/* Remember (on rank 0) what was written (from all process) to check against it at reading */
static int nblocks_per_step;
static int nsteps;
static uint64_t * block_offset;  // block_offset[ step*nblocks_per_step + i ] is i-th block offset written in "step".
static uint64_t * block_count;   // block_count [ step*nblocks_per_step + i ] is i-th block size written in "step".
static uint64_t * gdims;  // gdims[i] is the global dimension in i-th "step".


int main (int argc, char ** argv) 
{
    MPI_Init (&argc, &argv);
    MPI_Comm_rank (comm, &rank);
    MPI_Comm_size (comm, &size);

    nerrors = 0;
    write_blocks();
    if (!rank) {
        print_written_info(); // this is just for debug to check if rank 0 stores the correct values
        read_all();
        read_stepbystep();
        read_scalar();
        read_scalar_stepbystep ();
    }

    MPI_Barrier (comm);
    MPI_Finalize ();
    free (block_offset);
    free (block_count);
    free (gdims);
    if (!rank) printf ("----------- Done. Found %d errors -------\n", nerrors);
    return nerrors;
}

int write_blocks () 
{
    int         NX, G, O; 
    double      *t;
    /* ADIOS variables declarations for matching gwrite_temperature.ch */
    int         it, i, r;
    uint64_t    adios_groupsize, adios_totalsize;

    if (!rank) printf ("------- Write blocks -------\n");
    // We will have "3 steps * 2 blocks per process * number of processes" blocks
    nsteps = 3;
    nblocks_per_step = 2;
    block_offset = (uint64_t*) malloc (sizeof(uint64_t) * nsteps * nblocks_per_step * size);
    block_count  = (uint64_t*) malloc (sizeof(uint64_t) * nsteps * nblocks_per_step * size);
    gdims        = (uint64_t*) malloc (sizeof(uint64_t) * nsteps);

    adios_init_noxml (comm);
    adios_allocate_buffer (ADIOS_BUFFER_ALLOC_NOW, 10);

    int64_t       m_adios_group;
    int64_t       m_adios_file;

    adios_declare_group (&m_adios_group, "restart", "", adios_flag_yes);
    adios_select_method (m_adios_group, "MPI", "", "");

    adios_define_var (m_adios_group, "NX"
            ,"", adios_integer
            ,0, 0, 0);

    adios_define_var (m_adios_group, "G"
            ,"", adios_integer
            ,0, 0, 0);

    /* have to define O and temperature as many times as we 
       write them within one step (twice) */
    for (it=0; it < nblocks_per_step; it++) {
        adios_define_var (m_adios_group, "O"
                ,"", adios_integer
                ,0, 0, 0);

        adios_define_var (m_adios_group, "t"
                ,"", adios_double
                ,"NX", "G", "O");
    }

    for (it =0; it < nsteps; it++) {
        if (!rank) printf ("Step %d:\n", it);
        NX = 10+it;
        G = nblocks_per_step * NX * size;

        t = (double *) malloc (NX*sizeof(double));

        for (i = 0; i < NX; i++)
            t[i] = rank + it*0.1 + 0.01;

        MPI_Barrier (comm);
        if (it==0) 
            adios_open (&m_adios_file, "restart", fname, "w", comm);
        else
            adios_open (&m_adios_file, "restart", fname, "a", comm);
        adios_groupsize = 4 + 4 + 4 + NX * 8
            + 4 + 4 + 4 + NX * 8;
        adios_group_size (m_adios_file, adios_groupsize, &adios_totalsize);

        adios_write(m_adios_file, "NX", (void *) &NX);
        adios_write(m_adios_file, "G", (void *) &G);
        O = rank * nblocks_per_step * NX;
        adios_write(m_adios_file, "O", (void *) &O);
        adios_write(m_adios_file, "t", t);

        printf ("rank %d: block 1: size=%d, offset=%d\n", rank, NX, O);
        for (r = 0; r < size; r++) {
            block_count  [it*nblocks_per_step*size + nblocks_per_step*r] = NX; 
            block_offset [it*nblocks_per_step*size + nblocks_per_step*r] = r * nblocks_per_step * NX; 
        }

        for (i = 0; i < NX; i++)
            t[i] += 0.01;

        O = rank * nblocks_per_step * NX + NX;
        adios_write(m_adios_file, "O", (void *) &O);
        adios_write(m_adios_file, "t", t);

        printf ("rank %d: block 2: size=%d, offset=%d\n", rank, NX, O);
        for (r = 0; r < size; r++) {
            block_count  [it*nblocks_per_step*size + nblocks_per_step*r + 1] = NX; 
            block_offset [it*nblocks_per_step*size + nblocks_per_step*r + 1] = r * nblocks_per_step * NX + NX; 
        }
        gdims [it] = G;

        adios_close (m_adios_file);
        MPI_Barrier (comm);

        free(t);
    }

    adios_finalize (rank);

    return 0;
}

void print_written_info()
{
    int s, r, b;
    printf ("\n------- Information recorded on rank 0 (read will compare to this info)  --------\n");
    for (s = 0; s < nsteps; s++) {
        printf ("Step %d:\n", s);
        printf ("  Global dim = %d\n", gdims[s]);
        for (r = 0; r < size; r++) {
            for (b = 0; b < nblocks_per_step; b++) {
                printf ("  rank %d: block %d: size=%llu, offset=%llu\n", r, b+1, 
                        block_count  [s*nblocks_per_step*size + nblocks_per_step*r + b],
                        block_offset [s*nblocks_per_step*size + nblocks_per_step*r + b]
                       );
            }
        }
    }
}

int print_varinfo (ADIOS_FILE *f, int start_step) 
{
    ADIOS_VARINFO * v;
    int i,j,k;

    v = adios_inq_var (f, "t");
    adios_inq_var_blockinfo (f, v);

    printf ("ndim = %d\n",  v->ndim);
    printf ("dims[%llu]",  v->dims[0]);
    if (v->dims[0] != gdims[start_step]) 
    {
        printf ("\tERROR: expected [%llu]", gdims[start_step]);
        nerrors++;
    }
    printf("\n");
    printf ("nsteps = %d\n",  v->nsteps);
    printf ("sum_nblocks = %d\n",  v->sum_nblocks);
    k = 0; // blockinfo is a contigous 1D array of elements from 0 to v->sum_nblocks-1
    for (i = 0; i < v->nsteps; i++) {
        printf ("  nblocks[%d] = %d\n", i, v->nblocks[i]);
        for (j = 0; j < v->nblocks[i]; j++) {
            printf("    block %2d: [%llu:%llu]", j,
                        v->blockinfo[k].start[0],
                        v->blockinfo[k].start[0] + v->blockinfo[k].count[0]-1);
            
            if (v->blockinfo[k].start[0] != block_offset [(start_step+i)*nblocks_per_step*size + j] ||
                v->blockinfo[k].count[0] != block_count  [(start_step+i)*nblocks_per_step*size + j] ) 
            {
                nerrors++;
                printf ("\tERROR: expected [%llu:%llu]",
                    block_offset [(start_step+i)*nblocks_per_step*size + j],
                    block_offset [(start_step+i)*nblocks_per_step*size + j] + 
                      block_count  [(start_step+i)*nblocks_per_step*size + j] -1
                );
            }
            printf("\n");
            k++;
        }
    }
    adios_free_varinfo (v);
}


int read_all ()
{
        ADIOS_FILE * f;
        float timeout_sec = 0.0; 
        int steps = 0;
    int retval = 0;
    MPI_Comm    comm = MPI_COMM_SELF;

    adios_read_init_method (ADIOS_READ_METHOD_BP, comm, "verbose=3");
    printf ("\n--------- Read as file  ------------\n");
    f = adios_read_open_file (fname, ADIOS_READ_METHOD_BP, comm);
    if (f == NULL) {
        printf ("Error at opening file: %s\n", adios_errmsg());
        retval = adios_errno;
    }
    else
    {
        /* Processing all the steps at once */
        print_varinfo (f, 0);
        adios_read_close (f);
    }
    adios_read_finalize_method (ADIOS_READ_METHOD_BP);
    return retval;
}

int read_stepbystep ()
{
    ADIOS_FILE * f;
    float timeout_sec = 0.0; 
    int steps = 0;
    int retval = 0;
    MPI_Comm    comm = MPI_COMM_SELF;

    adios_read_init_method (ADIOS_READ_METHOD_BP, comm, "verbose=3");
    printf ("\n--------- Read as stream  ------------\n");
    f = adios_read_open (fname, ADIOS_READ_METHOD_BP,
                          comm, ADIOS_LOCKMODE_NONE, timeout_sec);
    if (adios_errno == err_file_not_found)
    {
        printf ("Stream not found after waiting %f seconds: %s\n",
                timeout_sec, adios_errmsg());
        retval = adios_errno;
    }
    else if (adios_errno == err_end_of_stream)
    {
        printf ("Stream terminated before open. %s\n", adios_errmsg());
        retval = adios_errno;
    }
    else if (f == NULL) {
        printf ("Error at opening stream: %s\n", adios_errmsg());
        retval = adios_errno;
    }
    else
    {
        /* Processing loop over the steps (we are already in the first one) */
        while (adios_errno != err_end_of_stream) {
            steps++; // steps start counting from 1
            printf ("Step: %d\n", f->current_step);
            print_varinfo (f, f->current_step);

            // advance to 1) next available step with 2) blocking wait
            adios_advance_step (f, 0, timeout_sec);
            if (adios_errno == err_step_notready)
            {
                //printf ("No new step arrived within the timeout. Quit. %s\n",
                //        adios_errmsg());
                break; // quit while loop
            }
        }
        adios_read_close (f);
    }
    adios_read_finalize_method (ADIOS_READ_METHOD_BP);
    //printf ("We have processed %d steps\n", steps);
    return retval;
}



int print_scalar (ADIOS_FILE *f, char * name) 
{
    ADIOS_VARINFO * v;
    int i,j,k;

    v = adios_inq_var (f, name);
    adios_inq_var_blockinfo (f, v);

    printf ("Scalar '%s':\n",  name);
    printf ("nsteps = %d\n",  v->nsteps);
    printf ("nblocks per step = %d\n",  v->nblocks[0]);

    int err;

    /* Read one writeblock across all timesteps */
    int *data = (int*) calloc (v->nsteps, sizeof(int));
    ADIOS_SELECTION *s;
    printf ("Read same instance across all timesteps:\n");
    for (i=0; i < v->nblocks[0]; i++) {
        s = adios_selection_writeblock(i);
        err = adios_schedule_read_byid(f, s, v->varid, 0, v->nsteps, data);
        if (!err) 
        { 
            err = adios_perform_reads(f, 1);
            if (!err) 
            { 
                err = 0;
                printf ("  block %d = [",  i);
                for (j=0; j < v->nsteps; j++) {
                    printf ("%d", data[j]);
                    if (data[j] != 
                        block_offset [j*nblocks_per_step*size + i]) 
                    {
                        err = 1;
                    }
                    if (j < v->nsteps-1) printf(",");
                }
                printf("]");

                if (err) 
                {
                    nerrors++;
                    printf ("\tERROR expected = [");
                    for (j=0; j < v->nsteps; j++) {
                        printf ("%llu", block_offset [j*nblocks_per_step*size + i]);
                        if (j < v->nsteps-1) printf(",");
                    }
                    printf("]");
                }
                printf("\n");

            } else {
                printf ("ERROR at reading scalar '%s': %s\n", name, adios_errmsg());
            } 
        } else {
                printf ("ERROR at scheduling read for scalar '%s': %s\n", name, adios_errmsg());
        }
        adios_selection_delete(s);
    }

    /* Now read piecewise, one writeblock at a time */
    printf ("Read each instance individually:\n");
    for (j=0; j < v->nsteps; j++) {
        printf ("  step %d: \n",  j);
        for (i=0; i < v->nblocks[j]; i++) {
            s = adios_selection_writeblock(i);
            err = adios_schedule_read_byid(f, s, v->varid, j, 1, data);
            if (!err) 
            { 
                err = adios_perform_reads(f, 1);
                if (!err) 
                { 
                    printf ("    block %d = %d", i, data[0]);
                    if (data[0] != 
                        block_offset [j*nblocks_per_step*size + i]) 
                    {
                        printf ("\tERROR expected = %llu", 
                                block_offset [j*nblocks_per_step*size + i]);
                        nerrors++;
                    }
                    printf ("\n");
                } else {
                    printf ("ERROR at reading scalar '%s': %s\n", name, adios_errmsg());
                } 
            } else {
                printf ("ERROR at scheduling read for scalar '%s': %s\n", name, adios_errmsg());
            }
            adios_selection_delete(s);
        }
    }

    /* Now get them piecewise, but not with reading but through statistics */
    printf ("Get each instance individually from available statistics:\n");
    adios_inq_var_stat (f, v, 0, 1);
    if (v->statistics && v->statistics->blocks) {
        ADIOS_VARSTAT *stat = v->statistics;
        int blockid = 0;
        for (j=0; j < v->nsteps; j++) {
            printf ("  step %d: \n",  j);
            for (i=0; i < v->nblocks[j]; i++) {
                printf ("    block %d = %d", i, *(int*)stat->blocks->mins[blockid]);
                if (*(int*)stat->blocks->mins[blockid] != 
                        block_offset [j*nblocks_per_step*size + i]) 
                {
                    printf ("\tERROR expected = %llu", 
                            block_offset [j*nblocks_per_step*size + i]);
                    nerrors++;
                }
                printf ("\n");
                blockid++;
            }
        }
    }

    adios_free_varinfo (v);
    free(data);
}

int read_scalar ()
{
    ADIOS_FILE * f;
    float timeout_sec = 0.0; 
    int steps = 0;
    int retval = 0;
    MPI_Comm    comm = MPI_COMM_SELF;

    adios_read_init_method (ADIOS_READ_METHOD_BP, comm, "verbose=3");
    printf ("\n--------- Read all instances of the scalar 'O'  ------------\n");
    f = adios_read_open_file (fname, ADIOS_READ_METHOD_BP, comm);
    if (f == NULL) {
        printf ("Error at opening file: %s\n", adios_errmsg());
        retval = adios_errno;
    }
    else
    {
        /* Read the scalar O with writeblock selection */
        print_scalar (f, "O");
        adios_read_close (f);
    }
    adios_read_finalize_method (ADIOS_READ_METHOD_BP);
    return retval;
}

int read_scalar_stepbystep ()
{
    ADIOS_FILE * f;
    float timeout_sec = 0.0; 
    int steps = 0;
    int retval = 0;
    MPI_Comm    comm = MPI_COMM_SELF;

    adios_read_init_method (ADIOS_READ_METHOD_BP, comm, "verbose=3");
    printf ("\n--------- Read scalar in stream using varinfo->value  ------------\n");
    f = adios_read_open (fname, ADIOS_READ_METHOD_BP,
                          comm, ADIOS_LOCKMODE_NONE, timeout_sec);
    if (adios_errno == err_file_not_found)
    {
        printf ("Stream not found after waiting %f seconds: %s\n",
                timeout_sec, adios_errmsg());
        retval = adios_errno;
    }
    else if (adios_errno == err_end_of_stream)
    {
        printf ("Stream terminated before open. %s\n", adios_errmsg());
        retval = adios_errno;
    }
    else if (f == NULL) {
        printf ("Error at opening stream: %s\n", adios_errmsg());
        retval = adios_errno;
    }
    else
    {
        /* Processing loop over the steps (we are already in the first one) */
        while (adios_errno != err_end_of_stream) {
            steps++; // steps start counting from 1
            printf ("Step: %d\n", f->current_step);

            /* Check the scalar O with varinfo->value */
            ADIOS_VARINFO * v = adios_inq_var (f, "NX");
            int value =  *(int*)v->value;
            printf ("Scalar NX = %d", value);
            if (value != 
                    block_count [f->current_step*nblocks_per_step*size]) 
            {
                printf ("\tERROR expected = %d", 
                        block_count [f->current_step*nblocks_per_step*size]);
                nerrors++;
            }
            printf ("\n");

            // advance to 1) next available step with 2) blocking wait
            adios_advance_step (f, 0, timeout_sec);
            if (adios_errno == err_step_notready)
            {
                //printf ("No new step arrived within the timeout. Quit. %s\n",
                //        adios_errmsg());
                break; // quit while loop
            }
        }
        adios_read_close (f);
    }
    adios_read_finalize_method (ADIOS_READ_METHOD_BP);
    //printf ("We have processed %d steps\n", steps);
    return retval;
}
