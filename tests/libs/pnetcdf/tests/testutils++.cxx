/*
 *  Copyright (C) 2003, Northwestern University and Argonne National Laboratory
 *  See COPYRIGHT notice in top-level directory.
 */
/* $Id: testutils.c 2674 2016-12-04 05:35:09Z wkliao $ */


#include <stdio.h>
#include <limits.h>
#include <string.h>
#include <mpi.h>

#include <pnetcdf.h>
#include "testutils.h"

void parse_read_args(int argc, char **argv, int rank, params *p)
{
	int err, inlen, outlen;
	if ( rank == 0 ) {
		if (argc == 3 ) {
			strncpy(p->infname, argv[1], PATH_MAX-1);
			strncpy(p->outfname, argv[2], PATH_MAX-1);
			p->infname[PATH_MAX-1] = '\0';
			p->outfname[PATH_MAX-1] = '\0';
		} else if (argc == 1) {
			strcpy(p->infname, "../data/test_double.nc");
			strcpy(p->outfname, "testread.nc");
		} else {
			fprintf(stderr, "Usage: %s: <source> <destination>\n", 
					argv[0]);
			MPI_Abort(MPI_COMM_WORLD, 1);
		}
		inlen = strlen(p->infname);
		outlen = strlen(p->outfname);
	}

	err = MPI_Bcast(&inlen, 1, MPI_INT, 0, MPI_COMM_WORLD);
        MPI_ERR(err)
	err = MPI_Bcast(p->infname, inlen+1, MPI_CHAR, 0, MPI_COMM_WORLD);
        MPI_ERR(err)
	err = MPI_Bcast(&outlen, 1, MPI_INT, 0, MPI_COMM_WORLD);
        MPI_ERR(err)
	err = MPI_Bcast(p->outfname, outlen+1, MPI_CHAR, 0, MPI_COMM_WORLD);
        MPI_ERR(err)
}

void parse_write_args(int argc, char **argv, int rank, params *p)
{
	int err, outlen;
	if ( rank == 0 ) {
		if (argc == 2 ) {
			strncpy(p->outfname, argv[1], PATH_MAX-1);
			p->outfname[PATH_MAX-1] = '\0';
		} else if (argc == 1) {
			strcpy(p->outfname, "testwrite.nc");
		} else {
			fprintf(stderr, "Usage: %s: <destination>\n", argv[0]);
			MPI_Abort(MPI_COMM_WORLD, 1);
		}
		outlen = strlen(p->outfname);
	}
	err = MPI_Bcast(&outlen, 1, MPI_INT, 0, MPI_COMM_WORLD);
        MPI_ERR(err)
	err = MPI_Bcast(p->outfname, outlen+1, MPI_CHAR, 0, MPI_COMM_WORLD);
        MPI_ERR(err)
}


char* nc_err_code_name(int err)
{
    static char unknown_str[32];

    if (err > 0) { /* system error */
        const char *cp = (const char *) strerror(err);
        if (cp == NULL)
            sprintf(unknown_str,"Unknown error code %d",err);
        else
            sprintf(unknown_str,"Error code %d (%s)",err,cp);
        return unknown_str;
    }

    switch (err) {
        case (NC_NOERR):			return "NC_NOERR";
        case (NC_EBADID):			return "NC_EBADID";
        case (NC_ENFILE):			return "NC_ENFILE";
        case (NC_EEXIST):			return "NC_EEXIST";
        case (NC_EINVAL):			return "NC_EINVAL";
        case (NC_EPERM):			return "NC_EPERM";
        case (NC_ENOTINDEFINE):			return "NC_ENOTINDEFINE";
        case (NC_EINDEFINE):			return "NC_EINDEFINE";
        case (NC_EINVALCOORDS):			return "NC_EINVALCOORDS";
        case (NC_EMAXDIMS):			return "NC_EMAXDIMS";
        case (NC_ENAMEINUSE):			return "NC_ENAMEINUSE";
        case (NC_ENOTATT):			return "NC_ENOTATT";
        case (NC_EMAXATTS):			return "NC_EMAXATTS";
        case (NC_EBADTYPE):			return "NC_EBADTYPE";
        case (NC_EBADDIM):			return "NC_EBADDIM";
        case (NC_EUNLIMPOS):			return "NC_EUNLIMPOS";
        case (NC_EMAXVARS):			return "NC_EMAXVARS";
        case (NC_ENOTVAR):			return "NC_ENOTVAR";
        case (NC_EGLOBAL):			return "NC_EGLOBAL";
        case (NC_ENOTNC):			return "NC_ENOTNC";
        case (NC_ESTS):				return "NC_ESTS";
        case (NC_EMAXNAME):			return "NC_EMAXNAME";
        case (NC_EUNLIMIT):			return "NC_EUNLIMIT";
        case (NC_ENORECVARS):			return "NC_ENORECVARS";
        case (NC_ECHAR):			return "NC_ECHAR";
        case (NC_EEDGE):			return "NC_EEDGE";
        case (NC_ESTRIDE):			return "NC_ESTRIDE";
        case (NC_EBADNAME):			return "NC_EBADNAME";
        case (NC_ERANGE):			return "NC_ERANGE";
        case (NC_ENOMEM):			return "NC_ENOMEM";
        case (NC_EVARSIZE):			return "NC_EVARSIZE";
        case (NC_EDIMSIZE):			return "NC_EDIMSIZE";
        case (NC_ETRUNC):			return "NC_ETRUNC";
        case (NC_EAXISTYPE):			return "NC_EAXISTYPE";
        case (NC_EDAP):				return "NC_EDAP";
        case (NC_ECURL):			return "NC_ECURL";
        case (NC_EIO):				return "NC_EIO";
        case (NC_ENODATA):			return "NC_ENODATA";
        case (NC_EDAPSVC):			return "NC_EDAPSVC";
        case (NC_EDAS):				return "NC_EDAS";
        case (NC_EDDS):				return "NC_EDDS";
        case (NC_EDATADDS):			return "NC_EDATADDS";
        case (NC_EDAPURL):			return "NC_EDAPURL";
        case (NC_EDAPCONSTRAINT):		return "NC_EDAPCONSTRAINT";
        case (NC_ETRANSLATION):			return "NC_ETRANSLATION";
        case (NC_EACCESS):			return "NC_EACCESS";
        case (NC_EAUTH):			return "NC_EAUTH";
        case (NC_ENOTFOUND):			return "NC_ENOTFOUND";
        case (NC_ECANTREMOVE):			return "NC_ECANTREMOVE";
        case (NC_EHDFERR):			return "NC_EHDFERR";
        case (NC_ECANTREAD):			return "NC_ECANTREAD";
        case (NC_ECANTWRITE):			return "NC_ECANTWRITE";
        case (NC_ECANTCREATE):			return "NC_ECANTCREATE";
        case (NC_EFILEMETA):			return "NC_EFILEMETA";
        case (NC_EDIMMETA):			return "NC_EDIMMETA";
        case (NC_EATTMETA):			return "NC_EATTMETA";
        case (NC_EVARMETA):			return "NC_EVARMETA";
        case (NC_ENOCOMPOUND):			return "NC_ENOCOMPOUND";
        case (NC_EATTEXISTS):			return "NC_EATTEXISTS";
        case (NC_ENOTNC4):			return "NC_ENOTNC4";
        case (NC_ESTRICTNC3):			return "NC_ESTRICTNC3";
        case (NC_ENOTNC3):			return "NC_ENOTNC3";
        case (NC_ENOPAR):			return "NC_ENOPAR";
        case (NC_EPARINIT):			return "NC_EPARINIT";
        case (NC_EBADGRPID):			return "NC_EBADGRPID";
        case (NC_EBADTYPID):			return "NC_EBADTYPID";
        case (NC_ETYPDEFINED):			return "NC_ETYPDEFINED";
        case (NC_EBADFIELD):			return "NC_EBADFIELD";
        case (NC_EBADCLASS):			return "NC_EBADCLASS";
        case (NC_EMAPTYPE):			return "NC_EMAPTYPE";
        case (NC_ELATEFILL):			return "NC_ELATEFILL";
        case (NC_ELATEDEF):			return "NC_ELATEDEF";
        case (NC_EDIMSCALE):			return "NC_EDIMSCALE";
        case (NC_ENOGRP):			return "NC_ENOGRP";
        case (NC_ESTORAGE):			return "NC_ESTORAGE";
        case (NC_EBADCHUNK):			return "NC_EBADCHUNK";
        case (NC_ENOTBUILT):			return "NC_ENOTBUILT";
        case (NC_EDISKLESS):			return "NC_EDISKLESS";
        case (NC_ECANTEXTEND):			return "NC_ECANTEXTEND";
        case (NC_EMPI):				return "NC_EMPI";
        // case (NC_EURL):				return "NC_EURL";
        // case (NC_ECONSTRAINT):			return "NC_ECONSTRAINT";
        case (NC_ESMALL):			return "NC_ESMALL";
        case (NC_ENOTINDEP):			return "NC_ENOTINDEP";
        case (NC_EINDEP):			return "NC_EINDEP";
        case (NC_EFILE):			return "NC_EFILE";
        case (NC_EREAD):			return "NC_EREAD";
        case (NC_EWRITE):			return "NC_EWRITE";
        case (NC_EOFILE):			return "NC_EOFILE";
        case (NC_EMULTITYPES):			return "NC_EMULTITYPES";
        case (NC_EIOMISMATCH):			return "NC_EIOMISMATCH";
        case (NC_ENEGATIVECNT):			return "NC_ENEGATIVECNT";
        case (NC_EUNSPTETYPE):			return "NC_EUNSPTETYPE";
        case (NC_EINVAL_REQUEST):		return "NC_EINVAL_REQUEST";
        case (NC_EAINT_TOO_SMALL):		return "NC_EAINT_TOO_SMALL";
        case (NC_ENOTSUPPORT):			return "NC_ENOTSUPPORT";
        case (NC_ENULLBUF):			return "NC_ENULLBUF";
        case (NC_EPREVATTACHBUF):		return "NC_EPREVATTACHBUF";
        case (NC_ENULLABUF):			return "NC_ENULLABUF";
        case (NC_EPENDINGBPUT):			return "NC_EPENDINGBPUT";
        case (NC_EINSUFFBUF):			return "NC_EINSUFFBUF";
        case (NC_ENOENT):			return "NC_ENOENT";
        case (NC_EINTOVERFLOW):			return "NC_EINTOVERFLOW";
        case (NC_ENOTENABLED):			return "NC_ENOTENABLED";
        case (NC_EBAD_FILE):			return "NC_EBAD_FILE";
        case (NC_ENO_SPACE):			return "NC_ENO_SPACE";
        case (NC_EQUOTA):			return "NC_EQUOTA";
        case (NC_ENULLSTART):			return "NC_ENULLSTART";
        case (NC_ENULLCOUNT):			return "NC_ENULLCOUNT";
        case (NC_EINVAL_CMODE):			return "NC_EINVAL_CMODE";
        case (NC_EINVAL_OMODE):			return "NC_EINVAL_OMODE";
        case (NC_ETYPESIZE):			return "NC_ETYPESIZE";
        case (NC_ETYPE_MISMATCH):		return "NC_ETYPE_MISMATCH";
        case (NC_ETYPESIZE_MISMATCH):		return "NC_ETYPESIZE_MISMATCH";
        case (NC_ESTRICTCDF2):			return "NC_ESTRICTCDF2";
        case (NC_ENOTRECVAR):			return "NC_ENOTRECVAR";
        case (NC_ENOTFILL):			return "NC_ENOTFILL";
        case (NC_EMULTIDEFINE):			return "NC_EMULTIDEFINE";
        case (NC_EMULTIDEFINE_OMODE):		return "NC_EMULTIDEFINE_OMODE";
        case (NC_EMULTIDEFINE_CMODE):		return "NC_EMULTIDEFINE_CMODE";
        case (NC_EMULTIDEFINE_DIM_NUM):		return "NC_EMULTIDEFINE_DIM_NUM";
        case (NC_EMULTIDEFINE_DIM_SIZE):	return "NC_EMULTIDEFINE_DIM_SIZE";
        case (NC_EMULTIDEFINE_DIM_NAME):	return "NC_EMULTIDEFINE_DIM_NAME";
        case (NC_EMULTIDEFINE_VAR_NUM):		return "NC_EMULTIDEFINE_VAR_NUM";
        case (NC_EMULTIDEFINE_VAR_NAME):	return "NC_EMULTIDEFINE_VAR_NAME";
        case (NC_EMULTIDEFINE_VAR_NDIMS):	return "NC_EMULTIDEFINE_VAR_NDIMS";
        case (NC_EMULTIDEFINE_VAR_DIMIDS):	return "NC_EMULTIDEFINE_VAR_DIMIDS";
        case (NC_EMULTIDEFINE_VAR_TYPE):	return "NC_EMULTIDEFINE_VAR_TYPE";
        case (NC_EMULTIDEFINE_VAR_LEN):		return "NC_EMULTIDEFINE_VAR_LEN";
        case (NC_EMULTIDEFINE_NUMRECS):		return "NC_EMULTIDEFINE_NUMRECS";
        case (NC_EMULTIDEFINE_VAR_BEGIN):	return "NC_EMULTIDEFINE_VAR_BEGIN";
        case (NC_EMULTIDEFINE_ATTR_NUM):	return "NC_EMULTIDEFINE_ATTR_NUM";
        case (NC_EMULTIDEFINE_ATTR_SIZE):	return "NC_EMULTIDEFINE_ATTR_SIZE";
        case (NC_EMULTIDEFINE_ATTR_NAME):	return "NC_EMULTIDEFINE_ATTR_NAME";
        case (NC_EMULTIDEFINE_ATTR_TYPE):	return "NC_EMULTIDEFINE_ATTR_TYPE";
        case (NC_EMULTIDEFINE_ATTR_LEN):	return "NC_EMULTIDEFINE_ATTR_LEN";
        case (NC_EMULTIDEFINE_ATTR_VAL):	return "NC_EMULTIDEFINE_ATTR_VAL";
        case (NC_EMULTIDEFINE_FNC_ARGS):	return "NC_EMULTIDEFINE_FNC_ARGS";
        case (NC_EMULTIDEFINE_FILL_MODE):	return "NC_EMULTIDEFINE_FILL_MODE";
        case (NC_EMULTIDEFINE_VAR_FILL_MODE):	return "NC_EMULTIDEFINE_VAR_FILL_MODE";
        case (NC_EMULTIDEFINE_VAR_FILL_VALUE):	return "NC_EMULTIDEFINE_VAR_FILL_VALUE";
        default:
              sprintf(unknown_str,"Unknown code %d",err);
    }
    return unknown_str;
}

