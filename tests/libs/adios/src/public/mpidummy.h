/*
 * ADIOS is freely available under the terms of the BSD license described
 * in the COPYING file in the top level directory of this source distribution.
 *
 * Copyright (c) 2008 - 2009.  UT-BATTELLE, LLC. All rights reserved.
 */

#ifndef __MPI_DUMMY_H__
#define __MPI_DUMMY_H__

/*
   A dummy MPI 'implementation' for the BP READ API, to have an MPI-free version of the API
*/
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <stdint.h>
#include <stdio.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef int MPI_Comm;
typedef uint64_t MPI_Status;
typedef int MPI_File;
typedef int MPI_Info;
typedef int MPI_Datatype;  /* Store the byte size of a type in such vars */
typedef uint64_t MPI_Offset;
typedef int MPI_Fint;

#define MPI_SUCCESS                 0
#define MPI_ERR_BUFFER              1      /* Invalid buffer pointer */
#define MPI_ERR_COUNT               2      /* Invalid count argument */
#define MPI_ERR_TYPE                3      /* Invalid datatype argument */
#define MPI_ERR_TAG                 4      /* Invalid tag argument */
#define MPI_ERR_COMM                5      /* Invalid communicator */
#define MPI_MAX_ERROR_STRING        512
#define MPI_MODE_RDONLY             O_RDONLY
#define MPI_SEEK_SET                SEEK_SET
#define MPI_SEEK_CUR                SEEK_CUR
#define MPI_SEEK_END                SEEK_END
#define MPI_BYTE                    1          /* I need the size of the type here */
#define MPI_INFO_NULL               0

#define MPI_COMM_NULL               0
#define MPI_COMM_WORLD              1
#define MPI_COMM_SELF               2

#define MPI_INT                     1
#define MPI_CHAR                    2
#define MPI_DOUBLE                  3

#define MPI_ANY_SOURCE              0
#define MPI_ANY_TAG                 0

#define MPI_SUM                     0

#define MPI_MAX_PROCESSOR_NAME      32
int MPI_Init(int *argc, char ***argv);
int MPI_Finalize();
int MPI_Initialized( int* flag ) ;

int MPI_Barrier(MPI_Comm comm);
int MPI_Bcast(void *buffer, int count, MPI_Datatype datatype, int root, MPI_Comm comm);

int MPI_Comm_dup(MPI_Comm comm, MPI_Comm *newcomm);
int MPI_Comm_rank(MPI_Comm comm, int *rank);
int MPI_Comm_size(MPI_Comm comm, int *size);
int MPI_Comm_free(MPI_Comm *comm);
MPI_Comm MPI_Comm_f2c(MPI_Fint comm);

int MPI_Gather(void *sendbuf, int sendcnt, MPI_Datatype sendtype, void *recvbuf, int recvcnt, MPI_Datatype recvtype, int root, MPI_Comm comm) ;
int MPI_Gatherv(void *sendbuf, int sendcnt, MPI_Datatype sendtype, void *recvbuf, int *recvcnts, int *displs, MPI_Datatype recvtype, int root, MPI_Comm comm);
int MPI_Allgather(void *sendbuf, int sendcount, MPI_Datatype sendtype,
                  void *recvbuf, int recvcount, MPI_Datatype recvtype,
                  MPI_Comm comm);

int MPI_Scatter(void *sendbuf, int sendcnt, MPI_Datatype sendtype, void *recvbuf, int recvcnt, MPI_Datatype recvtype, int root, MPI_Comm comm);
int MPI_Scatterv(void *sendbuf, int *sendcnts, int *displs, MPI_Datatype sendtype, void *recvbuf, int recvcnt, MPI_Datatype recvtype, int root, MPI_Comm comm);



int MPI_File_open(MPI_Comm comm, char *filename, int amode, MPI_Info info, MPI_File *fh);
int MPI_File_close(MPI_File *fh);
int MPI_File_get_size(MPI_File fh, MPI_Offset *size);
int MPI_File_read(MPI_File fh, void *buf, int count, MPI_Datatype datatype, MPI_Status *status);
int MPI_File_seek(MPI_File fh, MPI_Offset offset, int whence);

int MPI_Get_count(MPI_Status *status, MPI_Datatype datatype, int *count);
int MPI_Error_string(int errorcode, char *string, int *resultlen);
int MPI_Comm_split ( MPI_Comm comm, int color, int key, MPI_Comm *comm_out );

int MPI_Get_processor_name (char *name, int *resultlen);

double MPI_Wtime();

#ifdef __cplusplus
}
#endif

#endif
