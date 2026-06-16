/*
 *  Copyright (C) 2003, Northwestern University and Argonne National Laboratory
 *  See COPYRIGHT notice in top-level directory.
 */
/* $Id$ */


#include <stdio.h>
#include <limits.h>
#include <string.h> /* strchr(), strerror(), strdup(), strcpy(), strlen() */
#include <mpi.h>

#include <pnetcdf.h>
#include "testutils.h"

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

/*----< inq_env_hint() >-----------------------------------------------------*/
int
inq_env_hint(char *hint_key, char **hint_value)
{
    char *warn_str="Warning: skip ill-formed hint set in PNETCDF_HINTS";
    char *env_str;

    /* read hints set in the environment variable PNETCDF_HINTS, a string of
     * hints separated by ";" and each hint is in the form of hint=value. E.g.
     * "cb_nodes=16;cb_config_list=*:6". If this environment variable is set,
     * this subroutine allocates char array for hint_value, copy the hint
     * value to it, and return 1. Otherwise it returns 0 with *value set to
     * NULL.
     */

    *hint_value = NULL;

    /* get environment variable PNETCDF_HINTS */
    if ((env_str = getenv("PNETCDF_HINTS")) != NULL) {
        char *env_str_cpy, *hint, *next_hint, *key, *val, *deli;
        char *hint_saved=NULL;

        env_str_cpy = strdup(env_str);
        next_hint = env_str_cpy;

        do {
            hint = next_hint;
            deli = strchr(hint, ';');
            if (deli != NULL) {
                *deli = '\0'; /* add terminate char */
                next_hint = deli + 1;
            }
            else next_hint = "\0";
            if (hint_saved != NULL) free(hint_saved);

            /* skip all-blank hint */
            hint_saved = strdup(hint);
            if (strtok(hint, " \t") == NULL) continue;

            free(hint_saved);
            hint_saved = strdup(hint); /* save hint for error message */

            deli = strchr(hint, '=');
            if (deli == NULL) { /* ill-formed hint */
                printf("%s: '%s'\n", warn_str, hint_saved);
                continue;
            }
            *deli = '\0';

            /* hint key */
            key = strtok(hint, "= \t");
            if (key == NULL || NULL != strtok(NULL, "= \t")) {
                /* expect one token before = */
                printf("%s: '%s'\n", warn_str, hint_saved);
                continue;
            }

            /* hint value */
            val = strtok(deli+1, "= \t");
            if (NULL != strtok(NULL, "= \t")) { /* expect one token before = */
                printf("%s: '%s'\n", warn_str, hint_saved);
                continue;
            }
            if (strcasecmp(key,hint_key) == 0) {
                /* inquired hint is found */
                if (val != NULL) {
                    *hint_value = (char*) malloc(strlen(val)+1);
                    strcpy(*hint_value, val);
                }
                if (hint_saved != NULL) free(hint_saved);
                free(env_str_cpy);
                return (val == NULL) ? 0 : 1;
            }
        } while (*next_hint != '\0');

        if (hint_saved != NULL) free(hint_saved);
        free(env_str_cpy);
    }
    return 0;
}

/* File system types recognized by ROMIO in MPICH 4.0.0 */
static const char* fstypes[] = {"ufs", "nfs", "xfs", "pvfs2", "gpfs", "panfs", "lustre", "daos", "testfs", "ime", "quobyte", NULL};

/* Return a pointer to filename by removing the file system type prefix name if
 * there is any.  For example, when filename = "lustre:/home/foo/testfile.nc",
 * remove "lustre:" to return a pointer to "/home/foo/testfile.nc", so the name
 * can be used in POSIX open() calls.
 */
char* remove_file_system_type_prefix(const char *filename)
{
    char *ret_filename = (char*)filename;

    if (filename == NULL) return NULL;

    if (strchr(filename, ':') != NULL) { /* there is a prefix end with ':' */
        /* check if prefix is one of recognized file system types */
        int i=0;
        while (fstypes[i] != NULL) {
            size_t prefix_len = strlen(fstypes[i]);
            if (!strncmp(filename, fstypes[i], prefix_len)) { /* found */
                ret_filename += prefix_len + 1;
                break;
            }
            i++;
        }
    }

    return ret_filename;
}

