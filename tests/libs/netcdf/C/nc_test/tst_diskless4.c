/*
  Copyright 2008, UCAR/Unidata
  See COPYRIGHT file for copying and redistribution conditions.

  This program tests the large file bug in netCDF 3.6.2,
  creating byte and short variables larger than 4 GiB.

  $Id: tst_big_var.c,v 1.9 2010/05/15 00:50:10 russ Exp $
*/

#include <nc_tests.h>
#include <netcdf.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#undef XFAIL

#define FILE_NAME "tst_diskless4.nc"
#define CHUNKSIZE 4096
#define DATASIZE (CHUNKSIZE/sizeof(int))
#define DIMMAX 1000000000

typedef enum Tag {Create,CreateDiskless,Open,OpenDiskless} Tag;

#define REPORT err_report(status,__FILE__,__LINE__)

static void
err_report(int status, char* file, int line)
{
    printf("***FAIL: %s: line=%d status=%d %s\n",file,line,status,nc_strerror(status));
#ifdef XFAIL
    exit(0);
#else
    exit(1);
#endif
}

int
main(int argc, char **argv)
{
    int status = NC_NOERR;
    int i,j,iv;
    unsigned int data[DATASIZE];
    size_t start[1];
    size_t count[1];
    Tag tag = Create; 
    int cmode = 0;
    int ncid;
    int dimids[1];
    void* memory;
    int nvars;
    int varids[4096];
    size_t varsize;
    size_t filesize;

    /* Get the specified var/file size */
    if(argc > 1) {
	filesize = atol(argv[1]);
    } else {
	if(sizeof(size_t) == 4)
	    filesize = 1000000000L;
	else if(sizeof(size_t) == 8)
	    filesize = 3000000000L;
	else {
	    fprintf(stderr,"Cannot compute filesize\n");
	    exit(1);
	}
    }

    /* Test that we can malloc that much space */
    memory = malloc(filesize);
    if(memory == NULL) {
        fprintf(stderr,"Cannot malloc %lu bytes\n",(unsigned long)filesize);
	exit(1);
    }
    free(memory);

    if(argc > 2) {
        if(strcmp(argv[2],"create")==0) tag = Create;
        else if(strcmp(argv[2],"creatediskless")==0) tag = CreateDiskless;
        else if(strcmp(argv[2],"open")==0) tag = Open;
        else if(strcmp(argv[2],"opendiskless")==0) tag = OpenDiskless;
	else {
	    fprintf(stderr,"illegal tag: %s",argv[2]);
	    exit(1);
	}
    } else
	tag = Create; /* default */
    
    switch (tag) {
    case Create: printf("\n*** Create file\n"); break;
    case CreateDiskless: printf("\n*** Create file diskless\n"); break;
    case Open: printf("\n*** Open file\n"); break;
    case OpenDiskless: printf("\n*** Open file diskless\n"); break;
    }

    switch (tag) {
    case Create:	  cmode = NC_CLOBBER; break;
    case CreateDiskless:  cmode = NC_CLOBBER|NC_DISKLESS|NC_WRITE; break;
    case Open:		  cmode = 0; break;
    case OpenDiskless:	  cmode = NC_DISKLESS; break;
    }

    switch (tag) {
    case Create:
    case CreateDiskless:
	/* Try to alloc as much as possible initially */
        if((status=nc__create(FILE_NAME, cmode, filesize, NULL, &ncid)))
	    REPORT;
        if((status=nc_set_fill(ncid, NC_NOFILL, NULL)))
	    REPORT;
	/* Only need 1 dimension */
        if((status=nc_def_dim(ncid, "dim", DIMMAX, &dimids[0])))
	    REPORT;
	break;
    case Open:
    case OpenDiskless:
        if((status=nc_open(FILE_NAME, cmode, &ncid)))
	    REPORT;
        if((status=nc_inq_dimid(ncid, "dim", &dimids[0])))
	    REPORT;
	break;
    }

    varsize = DIMMAX;
    nvars = filesize / varsize;
        assert((filesize % DIMMAX) == 0);
        assert(nvars < 4096);

    for(iv=0;iv<nvars;iv++) {
	char varname[32];
        sprintf(varname,"var%d",iv);
	switch (tag) {
        case Create:
        case CreateDiskless:
            if((status=nc_def_var(ncid, varname, NC_BYTE, 1, &dimids[0], &varids[iv])))
	        REPORT;
	break;
        case Open:
        case OpenDiskless:
            if((status=nc_inq_varid(ncid, varname, &varids[iv])))
	        REPORT;
	    break;
	}
    }

    if(tag == Create || tag == CreateDiskless) {
        if((status=nc_enddef(ncid)))
	    REPORT;
    }

    for(iv=0;iv<nvars;iv++) {
        size_t pieces = varsize/CHUNKSIZE;
        switch (tag) {
        case Create:
        case CreateDiskless:
	    /* Fill and put as integers */
	    for(i=0;i<pieces;i++) {
		start[0] = i*CHUNKSIZE;
		count[0] = CHUNKSIZE;
		for(j=0;j<DATASIZE;j++) data[j] = iv*((i*CHUNKSIZE)+j);
		if((status=nc_put_vara(ncid,varids[iv],start,count,(void*)data)))
		    REPORT;
	    }
	    break;
        case Open:
        case OpenDiskless:
	    /* Read the var contents and validate */
	    for(i=0;i<pieces;i++) {
		start[0] = i*CHUNKSIZE;
		count[0] = CHUNKSIZE;
		if((status=nc_get_vara(ncid,varids[iv],start,count,(void*)data)))
		    REPORT;
		for(j=0;j<DATASIZE;j++) {
		    unsigned int expected = iv*((i*CHUNKSIZE)+j);
	   	    if(data[j] != expected) {
		        printf("mismatch: iv=%d i=%u j=%u data=%u; should be %u\n",
				iv, i,j,data[j],expected);
		        err++;
		    }
		}
	    }
	    break;
	}
    }

    if((status=nc_close(ncid)))
	REPORT;

    SUMMARIZE_ERR;
    exit(0);
    return 0;
}
